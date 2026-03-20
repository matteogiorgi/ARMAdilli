# ============================================================
# Scarica e plotta una serie storica da Yahoo Finance
# Libreria: yfR  (equivalente R di yfinance per Python)
#
# Uso da riga di comando:
#   Rscript main.R [TICKER] [DATA_INIZIO] [DATA_FINE]
#
# Esempi:
#   Rscript main.R BZ=F 2015-01-01 2025-12-31   # Brent crude
#   Rscript main.R NG=F 2010-01-01               # Natural Gas
#   Rscript main.R HO=F                           # Heating Oil
#   Rscript main.R AAPL 2020-01-01 2024-12-31    # Apple
# ============================================================

# ---------- parametri -------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

TICKER     <- if (length(args) >= 1) args[1] else "BZ=F"
DATE_START <- if (length(args) >= 2) args[2] else "2010-01-01"
DATE_END   <- if (length(args) >= 3) args[3] else format(Sys.Date(), "%Y-%m-%d")

# ---------- dipendenze ------------------------------------------------
if (!requireNamespace("yfR", quietly = TRUE)) {
  message("Installo il pacchetto 'yfR'...")
  install.packages("yfR", repos = "https://cloud.r-project.org")
}
library(yfR)

# ---------- download --------------------------------------------------
message(sprintf("Scarico '%s' da Yahoo Finance (%s → %s)...", TICKER, DATE_START, DATE_END))

df <- yf_get(
  tickers    = TICKER,
  first_date = DATE_START,
  last_date  = DATE_END,
  freq_data  = "daily"
)

if (is.null(df) || nrow(df) == 0) {
  stop(sprintf("Nessuna osservazione valida scaricata per il ticker '%s'.", TICKER))
}

# ---------- salva CSV --------------------------------------------------
output_csv <- paste0(gsub("[^A-Za-z0-9]", "_", TICKER), ".csv")
write.csv(df, file = output_csv, row.names = FALSE)
message(sprintf("Serie salvata in: %s  (%d osservazioni)", normalizePath(output_csv), nrow(df)))

# ---------- plot -------------------------------------------------------
plot(
  df$ref_date,
  df$price_adjusted,
  type = "l",
  lwd  = 2,
  col  = "steelblue",
  main = sprintf("%s — Adj. Close", TICKER),
  xlab = "Data",
  ylab = "Prezzo aggiustato"
)
grid(col = "gray85")
