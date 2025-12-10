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

## Dépendances

**Imports directs** : 17 packages → 69 packages installés (mesure pak)

Voir `doc/log/2025-12-10_reduire-imports-description.md` pour le détail du travail de réduction des dépendances.

## Documentation développement

- `doc/log/` : Logs de travail (convention R.AlphA.Doc v4)
- `dev/audit_dependencies.R` : Script audit dépendances récursives

**Travaux récents** :
- [2025-12-10] Réduction imports 25→17 (voir `doc/log/2025-12-10_reduire-imports-description.md`)
- [2025-12-10] Arbre dépendances pak (voir `doc/log/2025-12-10_arbre-dependances-pak.md`)

## Liens

- CRAN : https://CRAN.R-project.org/package=R.AlphA.Home
- GitHub : https://github.com/R-alpha-act/R.AlphA.Home
