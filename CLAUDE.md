# R.AlphA.Home

## Contexte
Package CRAN de fonctions utilitaires R. "Feel at Home using R".

## Stack
- R >= 4.0.0
- 12 imports directs (data.table, dplyr, ggplot2, readxl, writexl, etc.)
- Tests: testthat

## Conventions
- Roxygen2 pour documentation
- Préfixes explicites pour fonctions externes (stats::, utils::, etc.)
- Formats supportés par importAll(): CSV, Excel, RDS uniquement

## Commandes utiles
```r
devtools::check()    # Vérification CRAN
devtools::build()    # Build tarball
devtools::test()     # Tests unitaires
```

## Points d'attention
- **Pas de stringi/stringr** : Éliminés pour éviter problèmes CI/CD macOS
- **Pas de arrow/qs** : Formats parquet/feather/qs non supportés
- Soumission CRAN : via https://cran.r-project.org/submit.html

## Version actuelle
1.1.3 (prête pour CRAN, version en ligne: 1.1.0)

## TODO
- [ ] Soumettre sur CRAN après le 7 janvier 2026 (vacances CRAN 23 déc - 7 jan)
