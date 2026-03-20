# ARMAdilli

- Il file da usare è `starting.R`, gli altri sono old.
- gli output prodotti sono suddivisi in:
    * `data`: directory contenente le serie storiche scaricate e usate nelle varie esecuzioni
    * `plots`: directory contenente i plot prodotti nelle varie esecuzioni

[shared folder](https://mega.nz/folder/47kCnTKT#Arro1QyfF-PMpG7MIzbr2w)


## TODO

- **Medium**: lo script modifica silenziosamente i dati se compaiono `NA`, ma il CSV finale non conserva traccia di quali osservazioni siano state imputate. Se in futuro Yahoo restituisce mancanti, `starting.R` (line 147) sostituisce i valori e `starting.R` (line 307) salva solo `DATE` e `PRICE`; questo rende difficile audit e reproducibility.

- **Low**: il titolo del grafico dei livelli è fuorviante se cambi colonna prezzo. In `starting.R` (line 198) resta "Brent Adjusted Prices", ma sopra hai reso configurabile la scelta tra `price_open`, `price_high`, `price_low`, `price_close`, `price_adjusted`; se passi a `price_close`, il titolo diventa tecnicamente sbagliato.

- **Low**: `rm(list = ls())` in `starting.R` (line 1) e `graphics.off()` in `starting.R` (line 302) sono aggressivi per un uso interattivo in IDE. Non rompono l’analisi, ma cancellano stato e device dell’intera sessione, non solo quelli creati dallo script.


## Assumptions

- Mantenere `fill_missing()` come fallback automatico solo se Yahoo restituisce davvero `NA`.
- Il comportamento “chiudi tutti i device e genera solo PDF” sia intenzionale.