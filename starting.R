rm(list = ls())
options(scipen = 100, digits = 4)




# -------------------------------------------------------------------
# CONFIGURATION

# Definisce tutti i parametri generali dell'analisi: ticker Yahoo Finance,
# intervallo temporale, numero di ritardi usati nei test e nei grafici,
# soglie per il download e percorsi di output. In questa sezione viene
# anche creato un timestamp univoco per evitare di sovrascrivere PDF e CSV.
# -------------------------------------------------------------------

ticker <- "BZ=F"
# Yahoo Finance does not provide Brent futures history back to the 1980s.
# A 2007 start avoids discarding the ticker because of incomplete coverage.
date_start <- "2007-01-01"
date_end <- format(Sys.Date(), "%Y-%m-%d")
lag_analysis <- 15
adf_lags <- 6
max_ar_order <- 15
yfr_bad_data_threshold <- 0
output_dir <- "output"
plots_dir <- file.path(output_dir, "plots")
data_dir <- file.path(output_dir, "data")
logs_dir <- file.path(output_dir, "logs")
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
plots_file <- file.path(plots_dir, sprintf("plots_%s.pdf", run_timestamp))
series_file <- file.path(data_dir, sprintf("brent_%s.csv", run_timestamp))
log_file <- file.path(logs_dir, sprintf("console_%s.txt", run_timestamp))


# -------------------------------------------------------------------
# DEPENDENCIES

# Elenca i pacchetti necessari per download dati, test di stazionarieta`,
# diagnostica dei rendimenti, selezione/stima di modelli ARIMA e GARCH e
# gestione delle serie temporali. La funzione seguente installa eventuali
# dipendenze mancanti e poi le carica nella sessione R corrente.
# -------------------------------------------------------------------

required_packages <- c(
    "yfR",
    "fBasics",
    "fUnitRoots",
    "forecast",
    "FinTS",
    "fDMA",
    "fGarch",
    "timeSeries"
)

install_and_load <- function(packages) {
    for (pkg in packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            install.packages(pkg, repos = "https://cloud.r-project.org")
        }
        library(pkg, character.only = TRUE)
    }
}

install_and_load(required_packages)




# -------------------------------------------------------------------
# HELPERS

# Raccoglie le funzioni di supporto usate nel resto dello script. Queste
# funzioni isolano le operazioni ripetitive o tecniche: imputazione dei
# mancanti, download e pulizia della serie, grafici ACF/PACF, diagnostica
# statistica e gestione dei warning prodotti da confint().
# -------------------------------------------------------------------

fill_missing <- function(values) {
    missing_idx <- which(is.na(values))
    if (length(missing_idx) == 0) {
        return(values)
    }

    filled_values <- values
    observed_idx <- which(!is.na(values))
    if (length(observed_idx) < 2) {
        return(filled_values)
    }

    for (i in seq_len(length(observed_idx) - 1)) {
        left_idx <- observed_idx[i]
        right_idx <- observed_idx[i + 1]

        if (right_idx - left_idx <= 1) {
            next
        }

        gap_idx <- (left_idx + 1):(right_idx - 1)
        filled_values[gap_idx] <- approx(
            x = c(left_idx, right_idx),
            y = c(values[left_idx], values[right_idx]),
            xout = gap_idx
        )$y
    }

    filled_values
}


download_brent_series <- function(ticker, start_date, end_date) {
    message(sprintf(
        "Scarico '%s' da Yahoo Finance (%s -> %s)...",
        ticker, start_date, end_date
    ))

    raw_data <- yfR::yf_get(
        tickers = ticker,
        first_date = start_date,
        last_date = end_date,
        thresh_bad_data = yfr_bad_data_threshold,
        freq_data = "daily",
        do_cache = FALSE,
        do_parallel = FALSE,
        be_quiet = TRUE
    )

    if (is.null(raw_data) || nrow(raw_data) == 0) {
        stop(
            sprintf(
                paste(
                    "Nessuna osservazione scaricata per il ticker '%s'.",
                    "Controlla il ticker o riduci l'intervallo richiesto (%s -> %s)."
                ),
                ticker, start_date, end_date
            )
        )
    }

    # Se vuoi cambiare il prezzo usato nella serie, qui puoi sostituire
    # "price_adjusted" con una delle altre colonne disponibili da yfR:
    # "price_open", "price_high", "price_low", "price_close", "price_adjusted".
    clean_data <- raw_data[, c("ref_date", "price_adjusted")]
    names(clean_data) <- c("DATE", "PRICE")
    clean_data$DATE <- as.Date(clean_data$DATE)
    clean_data$PRICE <- as.numeric(clean_data$PRICE)

    missing_before <- sum(is.na(clean_data$PRICE))
    if (missing_before > 0) {
        message(sprintf(
            paste(
                "Trovati %d valori mancanti nella serie scaricata.",
                "Li sostituisco con valori interpolati sulla retta tra osservazione precedente e successiva."
            ),
            missing_before
        ))
        clean_data$PRICE <- fill_missing(clean_data$PRICE)
    }

    missing_after <- sum(is.na(clean_data$PRICE))
    if (missing_after > 0) {
        stop(sprintf(
            paste(
                "Restano %d valori mancanti dopo l'imputazione.",
                "Questo accade tipicamente se gli NA sono all'inizio o alla fine della serie."
            ),
            missing_after
        ))
    }

    if (nrow(clean_data) == 0) {
        stop("La serie scaricata non contiene prezzi validi.")
    }

    message(sprintf(
        "Osservazioni valide scaricate: %d (%s -> %s)",
        nrow(clean_data),
        format(min(clean_data$DATE), "%Y-%m-%d"),
        format(max(clean_data$DATE), "%Y-%m-%d")
    ))

    clean_data
}


plot_acf_pacf <- function(series, lag_max, title_prefix) {
    op <- par(mfcol = c(2, 1))
    on.exit(par(op), add = TRUE)

    forecast::Acf(series, lag.max = lag_max, main = sprintf("%s - ACF", title_prefix))
    forecast::Pacf(series, lag.max = lag_max, main = sprintf("%s - PACF", title_prefix))
}


run_level_diagnostics <- function(series, lag_max, adf_lags) {
    basic_stats <- fBasics::basicStats(series)
    print(basic_stats)

    plot(series, type = "l", main = "Brent Adjusted Prices", ylab = "Price", xlab = "Index")

    jb_normality <- fBasics::normalTest(series, method = "jb")
    jb_test <- fBasics::jarqueberaTest(series)
    ljung_box <- Box.test(series, lag = lag_max, type = "Ljung")
    adf_results <- list(
        c = fUnitRoots::adfTest(series, lags = adf_lags, type = "c"),
        nc = fUnitRoots::adfTest(series, lags = adf_lags, type = "nc"),
        ct = fUnitRoots::adfTest(series, lags = adf_lags, type = "ct")
    )

    plot_acf_pacf(series, lag_max, "Oil prices")

    list(
        basic_stats = basic_stats,
        jb_normality = jb_normality,
        jb_test = jb_test,
        ljung_box = ljung_box,
        adf = adf_results
    )
}


run_return_diagnostics <- function(series, label, lag_max) {
    message(sprintf("Diagnostica per %s", label))

    t_test <- t.test(series)
    jb_test <- fBasics::jarqueberaTest(series)
    ljung_box <- stats::Box.test(series, lag = lag_max, type = "Ljung")
    arch_test <- FinTS::ArchTest(series, lag = lag_max)

    plot_acf_pacf(series, lag_max, label)

    list(
        t_test = t_test,
        jb_test = jb_test,
        ljung_box = ljung_box,
        arch_test = arch_test
    )
}


run_checks <- function(series, label, lag_max) {
    abs_ljung <- Box.test(abs(series), lag = lag_max, type = "Ljung")
    sq_ljung <- Box.test(series^2, lag = lag_max, type = "Ljung")
    f_dma_arch <- fDMA::archtest(series, lag = lag_max)
    variance_order <- ar(series^2, method = "mle")$order

    op <- par(mfrow = c(2, 1))
    on.exit(par(op), add = TRUE)

    forecast::Acf(series^2, lag.max = lag_max, main = sprintf("%s squared - ACF", label))
    forecast::Pacf(series^2, lag.max = lag_max, main = sprintf("%s squared - PACF", label))

    list(
        abs_ljung = abs_ljung,
        sq_ljung = sq_ljung,
        f_dma_arch = f_dma_arch,
        variance_order = variance_order
    )
}


confint_with_warning_report <- function(model, model_label) {
    warnings_found <- character(0)

    confint_result <- withCallingHandlers(
        confint(model),
        warning = function(w) {
            warnings_found <<- c(warnings_found, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )

    if (length(warnings_found) > 0) {
        unique_warnings <- unique(warnings_found)
        for (warning_text in unique_warnings) {
            message(sprintf(
                "Warning durante confint() per il modello '%s': %s",
                model_label, warning_text
            ))
        }
    }

    confint_result
}




# -------------------------------------------------------------------
# DATA

# Avvia l'esecuzione vera e propria dello script dentro un blocco locale,
# cosi` gli oggetti temporanei restano confinati e il device PDF viene
# chiuso in modo affidabile a fine esecuzione. In questa sezione vengono
# creati gli output, scaricata la serie, salvato il CSV e costruiti prezzi
# e rendimenti da usare nelle analisi successive.
# -------------------------------------------------------------------

local({
    dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

    log_connection <- file(log_file, open = "wt")
    sink(log_connection, split = TRUE)
    on.exit(
        {
            sink()
            close(log_connection)
        },
        add = TRUE
    )

    grDevices::graphics.off()
    grDevices::pdf(plots_file)
    on.exit(grDevices::dev.off(), add = TRUE)

    brent_data <- download_brent_series(ticker, date_start, date_end)
    utils::write.csv(brent_data, file = series_file, row.names = FALSE)
    prezzi_petrolio <- brent_data$PRICE

    ritorni <- diff(prezzi_petrolio)
    ritorni_log <- diff(log(prezzi_petrolio))


    # -------------------------------------------------------------------
    # DESCRIPTIVE ANALYSIS ON LEVELS

    # Analizza la serie dei prezzi in livello. Calcola statistiche descrittive,
    # testa normalita` e autocorrelazione, esegue test ADF con diverse
    # specificazioni deterministiche e produce il grafico della serie insieme
    # ai correlogrammi ACF/PACF.
    # -------------------------------------------------------------------

    level_diagnostics <- run_level_diagnostics(
        series = prezzi_petrolio,
        lag_max = lag_analysis,
        adf_lags = adf_lags
    )


    # -------------------------------------------------------------------
    # DIAGNOSTICS ON RETURNS

    # Costruisce e confronta la dinamica dei rendimenti semplici e logaritmici.
    # In questa parte vengono eseguiti test su media, normalita`, dipendenza
    # seriale ed effetti ARCH, selezionati ordini AR/ARIMA, stimati i modelli
    # lineari scelti e analizzati gli intervalli di confidenza e i residui.
    # -------------------------------------------------------------------

    returns_diagnostics <- list(
        simple = run_return_diagnostics(ritorni, "Simple returns", lag_analysis),
        log = run_return_diagnostics(ritorni_log, "Log returns", lag_analysis)
    )

    auto_order_simple <- ar(ritorni, aic = TRUE, order.max = max_ar_order, method = "mle")
    auto_order_log <- ar(ritorni_log, aic = TRUE, order.max = max_ar_order, method = "mle")

    auto_arima_simple <- auto.arima(ritorni, max.p = 6, max.q = 6, ic = "aic")
    auto_arima_log <- auto.arima(ritorni_log, max.p = 6, max.q = 6, ic = "aic")

    # NOTA: auto.arima(ritorni_log) seleziona un ARIMA(0,0,0) con media nulla.
    # La specifica ARMA(6,1) sui log-rendimenti e` mantenuta per confronto con
    # l'analisi manuale, ma puo` risultare numericamente instabile: se confint()
    # produce NaN su alcuni parametri, gli intervalli di confidenza di quel
    # modello non sono affidabili.
    modritorni <- arima(ritorni, order = c(6, 0, 0), include.mean = FALSE)
    modritorni_log <- arima(ritorni_log, order = c(6, 0, 1), include.mean = FALSE)

    summary(modritorni)
    summary(modritorni_log)

    confint_simple <- confint_with_warning_report(modritorni, "AR(6) su simple returns")
    confint_log <- confint_with_warning_report(modritorni_log, "ARMA(6,1) su log returns")

    res_simple <- modritorni$residuals
    res_log <- modritorni_log$residuals

    plot(res_simple, type = "l", main = "Residuals - AR(6) on simple returns", ylab = "Residuals")
    plot(res_log, type = "l", main = "Residuals - ARMA(6,1) on log returns", ylab = "Residuals")

    residual_diagnostics <- list(
        simple = list(
            jb = jarqueberaTest(res_simple),
            ljung = Box.test(res_simple, lag = lag_analysis),
            adf = adfTest(ritorni, lags = adf_lags, type = "nc"),
            arch = archtest(as.vector(res_simple), lag = lag_analysis)
        ),
        log = list(
            jb = jarqueberaTest(res_log),
            ljung = Box.test(res_log, lag = lag_analysis),
            adf = adfTest(ritorni_log, lags = adf_lags, type = "nc"),
            arch = archtest(as.vector(res_log), lag = lag_analysis)
        )
    )

    plot_acf_pacf(ritorni, lag_analysis, "Simple returns")
    plot_acf_pacf(ritorni_log, lag_analysis, "Log returns")

    heteroskedasticity_checks <- list(
        simple = run_checks(ritorni, "Simple returns", lag_analysis),
        log = run_checks(ritorni_log, "Log returns", lag_analysis)
    )

    aic_comparison <- cbind(AIC(modritorni), AIC(modritorni_log))
    bic_comparison <- cbind(BIC(modritorni), BIC(modritorni_log))

    print(level_diagnostics$jb_normality)
    print(level_diagnostics$jb_test)
    print(level_diagnostics$ljung_box)
    print(level_diagnostics$adf)

    print(returns_diagnostics)
    print(auto_order_simple$order)
    print(auto_order_log$order)
    print(auto_arima_simple)
    print(auto_arima_log)
    print(confint_simple)
    print(confint_log)
    print(residual_diagnostics)
    print(heteroskedasticity_checks)
    print(aic_comparison)
    print(bic_comparison)


    # -------------------------------------------------------------------
    # VOLATILITY MODELS

    # Stima modelli ARCH/GARCH sui log-rendimenti per descrivere la dinamica
    # della varianza condizionata. I modelli vengono confrontati tramite
    # criteri informativi e i residui standardizzati del GARCH selezionato
    # sono sottoposti a test per verificare se l'eteroschedasticita` residua
    # sia stata assorbita in modo soddisfacente.
    # -------------------------------------------------------------------

    modvolatilityarch <- garchFit(
        ritorni_log ~ arma(6, 1) + garch(9, 0),
        data = ritorni_log,
        include.mean = FALSE,
        trace = FALSE
    )

    modvolatilitygarch <- garchFit(
        ritorni_log ~ arma(6, 1) + garch(1, 1),
        data = ritorni_log,
        include.mean = FALSE,
        trace = FALSE
    )

    volatility_ic <- c(
        modvolatilityarch@fit$ics[2],
        modvolatilitygarch@fit$ics[2]
    )

    volatility_ic_scaled <- list(
        arch = modvolatilityarch@fit$ics * length(ritorni_log),
        garch = modvolatilitygarch@fit$ics * length(ritorni_log)
    )

    std_res_garch <- modvolatilitygarch@residuals / modvolatilitygarch@sigma.t

    std_res_diagnostics <- list(
        jb = jarqueberaTest(std_res_garch),
        ljung = Box.test(std_res_garch, lag = lag_analysis, type = "L", fitdf = 7),
        ljung_sq = Box.test(std_res_garch^2, lag = lag_analysis, type = "L"),
        arch = archtest(std_res_garch, lag = lag_analysis)
    )

    print(volatility_ic)
    print(volatility_ic_scaled)
    print(std_res_diagnostics)


    # -------------------------------------------------------------------
    # FORECAST

    # Replica il blocco di forecasting presente in enrico_plus.R usando gli
    # oggetti costruiti in questo script: ritorni_log al posto di ritorni1 e
    # prezzi_petrolio come ultimo prezzo osservato da cui ricavare il forecast
    # del livello.
    # -------------------------------------------------------------------

    forecast_garchfit_1step <- predict(modvolatilitygarch, n.ahead = 1)

    last_price <- tail(prezzi_petrolio, 1)
    mu_hat <- forecast_garchfit_1step$meanForecast[1]
    sigma_hat <- forecast_garchfit_1step$standardDeviation[1]
    price_forecast <- last_price * exp(mu_hat + 0.5 * sigma_hat^2)

    # In fGarch, multi-step predict() with the default conditional MSE can fail
    # for higher-order ARMA terms because of an indexing bug in the package code.
    previsioni_garch <- predict(modvolatilitygarch, n.ahead = 4, mse = "uncond")

    forecast_outputs <- list(
        garchfit_1step = forecast_garchfit_1step,
        garch_1step_mean = forecast_garchfit_1step$meanForecast,
        garch_1step_sigma = forecast_garchfit_1step$standardDeviation,
        price_forecast = price_forecast,
        garch_4step_mean = previsioni_garch$meanForecast,
        garch_4step_sigma = previsioni_garch$standardDeviation
    )

    print(forecast_outputs)


    # -------------------------------------------------------------------
    # CHARACTERISTIC ROOTS

    # Calcola le radici del polinomio caratteristico del modello AR sui
    # rendimenti semplici. Questo controllo serve a valutare la struttura
    # dinamica del modello stimato e a capire se la presenza di radici
    # complesse implichi componenti cicliche nella risposta del sistema.
    # -------------------------------------------------------------------

    p1 <- c(1, -modritorni$coef)
    r1 <- polyroot(p1)

    print(p1)
    print(r1)

    # Complex roots in the characteristic polynomial imply cyclical dynamics.
    message(sprintf("Grafici salvati in '%s'.", plots_file))
    message(sprintf("Serie storica salvata in '%s'.", series_file))
    message(sprintf("Output testuale salvato in '%s'.", log_file))
})
