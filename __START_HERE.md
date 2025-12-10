# R.AlphA.Home

Package R de fonctions utilitaires personnelles pour simplifier les tâches courantes.

## Objectif

> "Feel at Home using R, Thanks to Shortcuts Functions Making it Simple"

Collection de raccourcis et outils réutilisables pour le travail quotidien en R.

## Catégories de fonctions

- **Data** : `cols_pad()`, `compareVars()`, `countSwitches()`, `importAll()`, `left_join_checks()`
- **Date/Time** : `rdate()`, `Rtimer` (classe R6)
- **Output** : `quickSave()`, `sepThsd()`, `printif()`
- **Graphics** : `lum_0_100()`, `ret_lum()`, `shiny_lum_0_100()`, `show_diff()`
- **System** : `root()`, `setOption()`, `loadCheck()`, `ralpha_fold()`/`ralpha_unfold()`

## Structure

```
R/              # Code source des fonctions
man/            # Documentation roxygen générée
tests/          # Tests unitaires
doc/log/        # Logs de développement (convention R.AlphA.Doc)
```

## Dépendances principales

data.table, dplyr, ggplot2, lubridate, stringr, R6, rstudioapi

## Liens

- CRAN : https://CRAN.R-project.org/package=R.AlphA.Home
- GitHub : https://github.com/R-alpha-act/R.AlphA.Home
