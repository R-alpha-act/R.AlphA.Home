---
date: 2025-12-10
type: Refactor
subject: Réduire imports DESCRIPTION
fix: Supprimer doublons, Base R inutiles, et packages non utilisés
short_story: Audit 25 deps → 20 imports → 74 packages récursifs. Suppression doublons (diffr, R6), Base R (stats, utils, tools, grDevices), et purrr non utilisé.
status: En cours
category: Architecture
archive_after: 6m
keywords: [imports, suggests, DESCRIPTION, dépendances, dependencies, CRAN, base R, préfixes, shiny]
---

# RÉSUMÉ EXÉCUTIF

## Objectif
Réduire le nombre de packages dans `Imports` pour limiter la vulnérabilité aux packages tiers.

**Réf. croisée** : Même travail effectué sur R.AlphA.AI → `R.AlphA.AI/doc/log/2025-12-09_reduire-imports-description.md`

## État actuel après audit

```
Imports directs     : 20
Packages récursifs  : 74
Ratio               : 1 import = 3.7 packages en moyenne
```

## Progression

| Étape | Avant | Après | Changement |
|-------|-------|-------|------------|
| Initial | 25 imports | - | Dont doublons |
| Suppression doublons | 25 | 23 | -2 (diffr, R6) |
| Suppression Base R | 23 | 19 | -4 (stats, utils, tools, grDevices) |
| Suppression inutilisé | 19 | 18 | -1 (purrr) |
| **Session 1** | **25** | **20** | **-5 imports (-20%)** |
| Doublons transitifs | 20 | 17 | -3 (shiny, shinyWidgets, htmlwidgets) |
| **Session 2** | **20** | **17** | **-3 imports** |
| **Total** | **25** | **17** | **-8 imports (-32%)** |

---

# DÉTAILS TECHNIQUES

## Problèmes identifiés

### 1. Doublons dans DESCRIPTION
```
diffr,    # ligne 22
...
diffr,    # ligne 41 (doublon)

R6,       # ligne 33
...
R6,       # ligne 43 (doublon)
```

### 2. Base R inutiles
Ces packages sont toujours disponibles, pas besoin de les déclarer :
- `stats` → `runif()` utilisé dans countSwitches.R, rdate.R
- `grDevices` → `rgb()`, `col2rgb()` utilisés dans ret_lum.R, lum_0_100.R
- `utils` → `install.packages()` utilisé dans loadCheck.R
- `tools` → `file_ext()`, `file_path_sans_ext()` utilisés dans quickSave.R

### 3. Package non utilisé
- `purrr` : aucun `@import` ni usage trouvé dans le code

## Packages conservés

| Package | Fichiers | Usage |
|---------|----------|-------|
| arrow | importAll.R | read_parquet, read_feather |
| data.table | importAll.R, Rtimer.R, ralpha_unfold.R | fread, data.table, rbindlist |
| diffr | show_diff.R | Comparaison visuelle |
| dplyr | importAll.R, ralpha_unfold.R | filter, mutate, group_by |
| ggplot2 | quickSave.R, lum_0_100.R | Graphiques |
| htmlwidgets | show_diff.R | onRender |
| jsonlite | show_diff.R | toJSON |
| lubridate | rdate.R | decimal_date, date_decimal |
| magrittr | foldAllBr.R | add() |
| openxlsx | importAll.R, quickExport.R | read.xlsx, write.xlsx |
| qs | importAll.R | qread |
| R.utils | quickSave.R | copyDirectory |
| R6 | Rtimer.R | Classe R6 |
| rstudioapi | foldAllBr.R, root.R, quickSave.R, ralpha_unfold.R | Intégration RStudio |
| shiny | shiny_lum_0_100.R | tags, HTML |
| shinyWidgets | shiny_lum_0_100.R | setBackgroundColor |
| stringi | ralpha_unfold.R | stri_extract, stri_count |
| stringr | foldAllBr.R, importAll.R, left_join_checks.R, ralpha_unfold.R, cols_pad.R | str_* |
| tibble | foldAllBr.R, importAll.R, left_join_checks.R, ralpha_unfold.R | tibble, tribble |
| tidyr | left_join_checks.R | replace_na |

---

## Audit dépendances récursives

Script : `dev/audit_dependencies.R`

### Poids par import (trié)

```
shinyWidgets     36 │████████████████████████████████████
shiny            35 │███████████████████████████████████
diffr            32 │████████████████████████████████
htmlwidgets      31 │███████████████████████████████
tidyr            24 │████████████████████████
ggplot2          21 │█████████████████████
arrow            19 │███████████████████
dplyr            18 │██████████████████
stringr          11 │███████████
tibble           11 │███████████
openxlsx          8 │████████
qs                6 │██████
R.utils           5 │█████
lubridate         3 │███
stringi           3 │███
data.table        1 │█
jsonlite          1 │█
magrittr          0 │
R6                0 │
rstudioapi        0 │
```

### Dépendances exclusives

Si on supprime l'import, ces packages ne seront plus nécessaires :

| Import | Libère |
|--------|--------|
| arrow | assertthat, bit64, bit |
| diffr | htmlwidgets |
| ggplot2 | grid, gtable, isoband, S7, scales, farver, labeling, RColorBrewer, viridisLite |
| lubridate | timechange |
| openxlsx | zip |
| qs | RApiSerialize, stringfish, RcppParallel |
| R.utils | R.methodsS3, R.oo |
| shinyWidgets | shiny |
| tidyr | dplyr, stringr |

### Doublons transitifs (à supprimer)

- `shiny` : déjà tiré par shinyWidgets → supprimer de DESCRIPTION
- `htmlwidgets` : déjà tiré par diffr → supprimer de DESCRIPTION

---

## 🔍 ANALYSE SHINY

### Usage actuel

**Fichier unique** : `R/shiny_lum_0_100.R`

**Fonctions utilisées** :
- `shiny::tags$head()`, `shiny::tags$style()`, `shiny::HTML()` - création HTML
- `shinyWidgets::setBackgroundColor()` - fonction spécifique

**But** : Ajuster la couleur de fond d'une app Shiny pour éviter le contraste avec les thèmes sombres.

### Options pour supprimer shiny/shinyWidgets

| Option | Description | Impact |
|--------|-------------|--------|
| **A. Suggests** | Garder la fonction, mettre shiny/shinyWidgets en Suggests | -36 deps récursifs, fonction toujours dispo |
| **B. Supprimer** | Supprimer `shiny_lum_0_100()` du package | -36 deps, perte de fonctionnalité |
| **C. HTML pur** | Remplacer par du CSS pur sans shiny | -36 deps, fonction simplifiée |

### Option A : Migration vers Suggests

```r
# Dans shiny_lum_0_100.R
shiny_lum_0_100 <- function(lum) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Le package 'shiny' est requis. Installez-le avec install.packages('shiny')")
  }
  if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
    stop("Le package 'shinyWidgets' est requis. Installez-le avec install.packages('shinyWidgets')")
  }
  # ... reste du code avec préfixes shiny:: et shinyWidgets::
}
```

### Option C : Remplacement par HTML pur

```r
# Retourne du HTML/CSS pur, utilisable dans n'importe quel contexte Shiny
shiny_lum_0_100 <- function(lum) {
  lum_pc <- lum / 100
  hex <- sprintf("#%02x%02x%02x", round(lum_pc*255), round(lum_pc*255), round(lum_pc*255))
  hex_sidebar <- sprintf("#%02x%02x%02x", round(min(lum*1.2,100)/100*255), ...)

  # Retourne du CSS pur
  css <- sprintf("body { background-color: %s; } #sidebar { background-color: %s; }", hex, hex_sidebar)
  return(css)
}
```

### Décision

- [x] **Option A : Suggests** (garde fonctionnalité, réduit deps obligatoires) ✅ CHOISI

**Implémentation :**
- `shiny` et `shinyWidgets` déplacés vers Suggests
- `htmlwidgets` supprimé (tiré par diffr)
- `requireNamespace()` checks ajoutés dans `shiny_lum_0_100.R`
- Préfixes `shiny::`, `shinyWidgets::`, `grDevices::` ajoutés

---

## TODO

- [x] Supprimer doublons (diffr, R6)
- [x] Supprimer Base R (stats, utils, tools, grDevices)
- [x] Supprimer purrr
- [x] Supprimer @importFrom Base R dans roxygen
- [x] Ajouter préfixes explicites (stats::, utils::, tools::, grDevices::)
- [x] Régénérer NAMESPACE
- [x] Supprimer doublons transitifs (shiny, shinyWidgets, htmlwidgets)
- [x] Décider sort de shiny/shinyWidgets → Option A (Suggests)
- [ ] Évaluer autres candidats Suggests (arrow, qs, diffr) → voir Session 3

---

## 🔍 ANALYSE SESSION 3 (à faire)

### Candidats identifiés

| Package | Deps | Utilisé dans | Usage | Action proposée |
|---------|------|--------------|-------|-----------------|
| `arrow` | 19 | importAll.R | read_parquet, read_feather | → Suggests |
| `qs` | 6 | importAll.R | qread | → Suggests |
| `diffr` | 32 | show_diff.R | Comparaison visuelle | → Suggests |
| `jsonlite` | 1 | show_diff.R | toJSON (tiré par diffr) | Supprimer |

### Justification

- **arrow/qs** : Formats de fichiers optionnels dans `importAll()`. L'utilisateur qui lit du parquet/feather/qs a déjà ces packages installés.
- **diffr** : Fonction `show_diff()` est un utilitaire optionnel, pas une fonction core.
- **jsonlite** : Seul usage est dans `show_diff.R`, tiré transitoirement par diffr.

### Impact estimé

```
Avant Session 3 : 17 imports
Après Session 3 : 13 imports (-4)
Dépendances récursives supprimées : ~57
```

### TODO Session 3

- [ ] Migrer arrow vers Suggests + requireNamespace dans importAll.R
- [ ] Migrer qs vers Suggests + requireNamespace dans importAll.R
- [ ] Migrer diffr vers Suggests + requireNamespace dans show_diff.R
- [ ] Supprimer jsonlite (tiré par diffr)
- [ ] Régénérer NAMESPACE

---

## Fichiers modifiés

### Session 1
```
DESCRIPTION (25 → 20 imports)
R/countSwitches.R (@importFrom stats supprimé)
R/rdate.R (@importFrom stats supprimé, stats::runif ajouté)
R/ret_lum.R (@importFrom grDevices supprimé)
R/lum_0_100.R (@importFrom grDevices supprimé, grDevices::rgb ajouté)
R/loadCheck.R (@importFrom utils supprimé, utils::install.packages ajouté)
R/quickSave.R (@importFrom tools supprimé)
NAMESPACE (régénéré)
```

### Session 2
```
DESCRIPTION (20 → 17 imports, shiny/shinyWidgets → Suggests)
R/shiny_lum_0_100.R (requireNamespace + préfixes shiny::, shinyWidgets::, grDevices::)
NAMESPACE (à régénérer)
```

---

## Références

- R.AlphA.AI : `doc/log/2025-12-09_reduire-imports-description.md` (même travail, plus approfondi)
- CRAN Policy : https://cran.r-project.org/doc/manuals/R-exts.html#Package-Dependencies
