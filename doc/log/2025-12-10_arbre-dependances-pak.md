---
date: 2025-12-10
type: Audit
subject: Arbre de dépendances pak (snapshot)
fix: N/A
short_story: Snapshot de pak::pkg_deps_tree() pour planifier les sessions 3+. 69 packages, gros coupables identifiés.
status: Référence temporaire
category: Architecture
archive_after: 1m
keywords: [pak, dependencies, arbre, snapshot, audit]
---

# Arbre de dépendances pak

**Généré le** : 2025-12-10
**Version** : R.AlphA.Home 1.1.2
**Packages totaux** : 69

## Arbre complet

```
local::. 1.1.2

├─arrow 22.0.0 (31.28 MB)
│ ├─assertthat 0.2.1
│ ├─bit64 4.6.0-1
│ │ └─bit 4.6.0
│ ├─glue 1.8.0
│ ├─purrr 1.2.0           ← tiré par arrow ET tidyr
│ │ ├─cli 3.6.5
│ │ ├─lifecycle 1.0.4
│ │ ├─magrittr 2.0.4
│ │ ├─rlang 1.1.6
│ │ └─vctrs 0.6.5
│ ├─R6 2.6.1
│ ├─rlang
│ ├─tidyselect 1.2.1
│ │ └─withr 3.0.2
│ └─vctrs
│
├─data.table 1.17.8 (3.07 MB)  ← LÉGER, 0 deps
│
├─diffr 0.3.0                   ← LOURD via htmlwidgets
│ └─htmlwidgets 1.6.4
│   ├─htmltools 0.5.9
│   │ ├─base64enc
│   │ ├─digest
│   │ ├─fastmap
│   │ └─rlang
│   ├─jsonlite 2.0.0
│   ├─knitr 1.50              ← POURQUOI knitr ici ?!
│   │ ├─evaluate
│   │ ├─highr
│   │ │ └─xfun
│   │ ├─xfun
│   │ └─yaml
│   ├─rmarkdown 2.30          ← POURQUOI rmarkdown ?!
│   │ ├─bslib 5.68 MB         ← ÉNORME
│   │ │ ├─cachem
│   │ │ ├─jquerylib
│   │ │ ├─memoise
│   │ │ ├─mime
│   │ │ └─sass 2.41 MB
│   │ │   ├─fs
│   │ │   └─rappdirs
│   │ ├─fontawesome
│   │ └─tinytex
│   └─yaml
│
├─dplyr 1.1.4 (1.61 MB)
│ ├─cli
│ ├─generics
│ ├─glue
│ ├─lifecycle
│ ├─magrittr
│ ├─pillar
│ │ └─utf8
│ ├─R6
│ ├─rlang
│ ├─tibble
│ │ └─pkgconfig
│ ├─tidyselect
│ └─vctrs
│
├─ggplot2 4.0.1 (8.47 MB)
│ ├─cli
│ ├─gtable
│ ├─isoband 1.96 MB
│ ├─lifecycle
│ ├─rlang
│ ├─S7
│ ├─scales
│ │ ├─farver 1.97 MB
│ │ ├─labeling
│ │ ├─RColorBrewer
│ │ └─viridisLite 1.30 MB
│ ├─vctrs
│ └─withr
│
├─jsonlite                     ← tiré par diffr, peut être supprimé
│
├─lubridate 1.9.4 (1.01 MB)
│ ├─generics
│ └─timechange
│
├─magrittr                     ← 0 deps, léger
│
├─openxlsx 4.2.8.1 (3.41 MB)
│ ├─Rcpp 3.37 MB
│ ├─stringi 14.79 MB          ← ÉNORME !
│ └─zip
│
├─qs 0.27.3 (2.93 MB)
│ ├─RApiSerialize
│ ├─Rcpp
│ └─stringfish
│   └─RcppParallel
│
├─R.utils 2.13.0
│ ├─R.methodsS3
│ └─R.oo
│
├─R6                           ← 0 deps, léger
│
├─rstudioapi                   ← 0 deps, léger
│
├─stringi 14.79 MB             ← ÉNORME, tiré par openxlsx ET stringr
│
├─stringr 1.6.0
│ ├─cli
│ ├─glue
│ ├─lifecycle
│ ├─magrittr
│ ├─rlang
│ ├─stringi                   ← dépendance problématique
│ └─vctrs
│
├─tibble                       ← tiré par dplyr
│
└─tidyr 1.3.1
  ├─cli
  ├─dplyr
  ├─glue
  ├─lifecycle
  ├─magrittr
  ├─purrr                     ← tiré ici aussi
  ├─rlang
  ├─stringr
  ├─tibble
  ├─tidyselect
  └─vctrs
```

## Analyse

### Gros coupables (taille)

| Package | Taille | Pourquoi |
|---------|--------|----------|
| `stringi` | 14.79 MB | ICU library embarquée |
| `arrow` | 31.28 MB | Apache Arrow C++ |
| `ggplot2` | 8.47 MB | Graphiques complets |
| `bslib` | 5.68 MB | Bootstrap (via diffr→htmlwidgets→rmarkdown) |

### Chaînes problématiques

**diffr → htmlwidgets → rmarkdown → bslib**
- `diffr` tire `htmlwidgets`
- `htmlwidgets` tire `knitr` et `rmarkdown` (!?)
- `rmarkdown` tire `bslib` (5.68 MB), `fontawesome`, `tinytex`
- **Impact** : ~20 packages pour une fonction de diff

**openxlsx → stringi**
- `openxlsx` tire `stringi` (14.79 MB)
- `stringr` tire aussi `stringi`
- **Double dépendance** sur ce package problématique

**arrow → purrr, tidyr → purrr**
- `purrr` est tiré par 2 chemins
- Même supprimé des Imports, il revient

### Packages légers (0-1 deps)

- `data.table` - 0 deps, 3.07 MB
- `R6` - 0 deps
- `magrittr` - 0 deps
- `rstudioapi` - 0 deps

## Actions suggérées

### Session 3 (priorité haute)
- [ ] `diffr` → Suggests : supprime ~20 packages (htmlwidgets, rmarkdown, bslib...)
- [ ] `arrow` → Suggests : supprime ~10 packages (bit64, assertthat...)
- [ ] `qs` → Suggests : supprime ~4 packages (stringfish, RcppParallel...)
- [ ] `jsonlite` → supprimer (tiré par diffr)

### Session 4+ (stringi/stringr)
- [ ] Auditer si `stringr` peut être remplacé par base R
- [ ] Note : `openxlsx` tire `stringi` quand même, donc impact limité

### Long terme
- [ ] Évaluer si `openxlsx` peut être remplacé par `writexl` (plus léger, pas de stringi)

---

## Références

**Docs connexes** :
- Log principal : `./2025-12-10_reduire-imports-description.md`
- Script audit : `../../dev/audit_dependencies.R`
- Point d'entrée : `../../__START_HERE.md`

**Même travail ailleurs** :
- R.AlphA.AI : `../R.AlphA.AI/doc/log/2025-12-09_reduire-imports-description.md` (31→14 imports)
