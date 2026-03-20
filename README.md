# ARMAdilli

- Il file da usare è `starting.R`, gli altri sono old.
- gli output prodotti sono suddivisi in:
    * `data`: directory contenente le serie storiche scaricate e usate nelle varie esecuzioni
    * `logs`: directory contenente i log testuali prodotti nelle varie esecuzioni (console output)
    * `plots`: directory contenente i plot prodotti nelle varie esecuzioni

[shared folder](https://mega.nz/folder/47kCnTKT#Arro1QyfF-PMpG7MIzbr2w)




## New stuff

- Aggiunto in `starting.R` il blocco di forecast sul Brent basato sul modello `ARMA(6,1)-GARCH(1,1)` già stimato nello script. L'output finale ora include:
  - forecast a 1 passo del log-rendimento atteso
  - volatilità condizionata attesa
  - forecast del prezzo del Brent (`price_forecast`)
  - forecast a 4 passi di media e volatilità

- Il forecast multi-step usa `predict(..., mse = "uncond")` per evitare un bug interno di `fGarch` che si presenta con il default `mse = "cond"` sui modelli ARMA di ordine più alto.

- Aggiunto il salvataggio dell'output testuale in `output/logs`, con file `console_<timestamp>.txt`, così ogni esecuzione conserva anche il log completo della console senza sovrascrivere i run precedenti.


### Previsione Brent

Nell'ultimo run registrato in `output/logs/console_20260320_122855.txt`, il modello `ARMA(6,1)-GARCH(1,1)` ha prodotto un forecast a 1 passo del prezzo del Brent pari a `108.5`, costruito a partire da:

- log-rendimento atteso a 1 passo: `-0.002219`
- volatilità condizionata attesa a 1 passo: `0.0465`

Le previsioni a 4 passi sui log-rendimenti sono:

- `-0.0022191`
- `0.0007854`
- `0.0001841`
- `-0.0001891`

Le corrispondenti volatilità condizionate previste sono:

- `0.04650`
- `0.04632`
- `0.04615`
- `0.04597`

Nel medesimo run, i test sui residui standardizzati del `GARCH(1,1)` non evidenziano autocorrelazione o effetti ARCH residui rilevanti (`p-value` circa `0.5` per Ljung-Box sui residui, `0.3` sui quadrati e `0.4` per il test ARCH), quindi il modello risulta coerente con la previsione prodotta.




## TODO

- **Medium**: lo script modifica silenziosamente i dati se compaiono `NA`, ma il CSV finale non conserva traccia di quali osservazioni siano state imputate. Se in futuro Yahoo restituisce mancanti, `starting.R` (line 147) sostituisce i valori e `starting.R` (line 307) salva solo `DATE` e `PRICE`; questo rende difficile audit e reproducibility.

- **Low**: il titolo del grafico dei livelli è fuorviante se cambi colonna prezzo. In `starting.R` (line 198) resta "Brent Adjusted Prices", ma sopra hai reso configurabile la scelta tra `price_open`, `price_high`, `price_low`, `price_close`, `price_adjusted`; se passi a `price_close`, il titolo diventa tecnicamente sbagliato.

- **Low**: `rm(list = ls())` in `starting.R` (line 1) e `graphics.off()` in `starting.R` (line 302) sono aggressivi per un uso interattivo in IDE. Non rompono l’analisi, ma cancellano stato e device dell’intera sessione, non solo quelli creati dallo script.


## Assumptions

- Mantenere `fill_missing()` come fallback automatico solo se Yahoo restituisce davvero `NA`.
- Il comportamento “chiudi tutti i device e genera solo PDF” sia intenzionale.
