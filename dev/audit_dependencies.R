# ============================================================================='
# R.AlphA.Home - Audit des dépendances
# ============================================================================='
# Analyse les imports directs et leurs dépendances récursives pour identifier
# les packages "lourds" et les candidats à supprimer/migrer vers Suggests.
#
# PRÉREQUIS : Être dans le répertoire du package (setwd si besoin)
# EXÉCUTION : Bloc par bloc dans RStudio
# RÉF       : R.AlphA.AI/doc/log/2025-12-09_reduire-imports-description.md
# ============================================================================='

{
	rm(list = ls())
	if (!file.exists("DESCRIPTION")) stop("❌ Pas de DESCRIPTION - vérifier le working directory")
	desc <- read.dcf("DESCRIPTION")
	pkg_name <- desc[, "Package"]
	imports_raw <- desc[, "Imports"]
	imports <- strsplit(imports_raw, ",\\s*")[[1]] |> trimws()
	message("✅ Package : ", pkg_name)
	message("📦 Imports directs : ", length(imports))
} # 0. Initialisation
{
	message("\n=== IMPORTS DIRECTS (", length(imports), ") ===")
	for (i in seq_along(imports)) {
		message(sprintf("  %2d. %s", i, imports[i]))
	}
} # 1. Liste des imports directs
{
	all_deps <- tools::package_dependencies(
		imports,
		which = c("Imports", "Depends"),
		recursive = TRUE
	)
	dep_counts <- sapply(all_deps, length)
	all_unique <- unique(unlist(all_deps))
	message("\n=== DÉPENDANCES RÉCURSIVES ===")
	message("📊 Total packages installés : ", length(all_unique))
	message("📊 Ratio : 1 import direct = ", round(length(all_unique) / length(imports), 1), " packages en moyenne")
} # 2. Calcul dépendances récursives
{
	message("\n=== POIDS PAR IMPORT (trié) ===")
	sorted_idx <- order(dep_counts, decreasing = TRUE)
	for (i in sorted_idx) {
		pkg <- imports[i]
		n <- dep_counts[i]
		bar <- paste(rep("█", min(n, 40)), collapse = "")
		message(sprintf("  %-15s %3d │%s", pkg, n, bar))
	}
} # 3. Classement par poids
{
	message("\n=== 🎯 CANDIDATS À ÉVALUER (≥10 deps) ===")
	heavy <- imports[dep_counts >= 10]
	if (length(heavy) == 0) {
		message("  ✅ Aucun package avec ≥10 dépendances")
	} else {
		for (pkg in heavy) {
			message(sprintf("  • %-15s (%2d deps)", pkg, dep_counts[pkg]))
		}
	}
} # 4. Candidats lourds
{
	message("\n=== 🔗 DÉPENDANCES EXCLUSIVES ===")
	message("  (si on supprime l'import, ces packages ne seront plus nécessaires)\n")
	for (pkg in imports) {
		deps <- all_deps[[pkg]]
		if (length(deps) == 0) next
		other_deps <- unlist(all_deps[names(all_deps) != pkg])
		exclusive <- setdiff(deps, other_deps)
		if (length(exclusive) > 0) {
			message(sprintf("  %-15s libère : %s", pkg, paste(exclusive, collapse = ", ")))
		}
	}
} # 5. Dépendances exclusives
{
	message("\n=== 📋 RÉSUMÉ ===")
	message(sprintf("  Imports directs     : %d", length(imports)))
	message(sprintf("  Packages récursifs  : %d", length(all_unique)))
	message(sprintf("  Packages \"lourds\"   : %d (≥10 deps)", sum(dep_counts >= 10)))
	message("\n💡 Actions possibles :")
	message("  - Migrer vers Suggests les packages optionnels")
	message("  - Remplacer par base R (voir R.AlphA.AI/doc/log/2025-12-09)")
	message("  - Supprimer si non utilisé")
} # 6. Résumé et recommandations
{
	loadCheck("pak")
	if (!requireNamespace("pak", quietly = TRUE)) {
		message("⚠️ pak non installé - install.packages('pak') pour arbre précis")
	} else {
		message("\n=== ARBRE DE DÉPENDANCES (pak) ===")
		message("📊 Résolveur identique à l'installation réelle\n")
		deps <- pak::pkg_deps(".")
		n_deps <- nrow(deps) - 1  # -1 pour exclure le package lui-même
		message("📦 Packages qui seront installés : ", n_deps)
		message("\nArbre visuel :")
		pak::pkg_deps_tree(".")
	}
} # 7. Arbre précis avec pak (recommandé)

pak::pkg_deps_tree(".")


{
"
=== ARBRE DE DÉPENDANCES (pak) ===
📊 Résolveur identique à l'installation réelle

✔ Updated metadata database: 7.15 MB in 4 files.
✔ Updating metadata database ... done
📦 Packages qui seront installés : 40

Arbre visuel :
local::.
1.1.2 ✨👷🏽‍♀️ ⬇ (unknown size)
├─data.table 1.17.8 ✨🔧 ⬇ (3.05 MB)
├─diffobj 0.3.6 ✨🔧 ⬇ (1.03 MB)
│ └─crayon 1.5.3 ✨ ⬇ (164.41 kB)
├─dplyr 1.1.4 ✨🔧 ⬇ (1.60 MB)
│ ├─cli 3.6.5 ✨🔧
│ ├─generics 0.1.4 ✨ ⬇ (81.51 kB)
│ ├─glue 1.8.0 ✨🔧 ⬇ (173.70 kB)
│ ├─lifecycle 1.0.4 ✨ ⬇ (124.78 kB)
│ │ ├─cli
│ │ ├─glue
│ │ └─rlang 1.1.6 ✨🔧
│ ├─magrittr 2.0.4 ✨🔧
│ ├─pillar 1.11.1 ✨ ⬇ (656.90 kB)
│ │ ├─cli
│ │ ├─glue
│ │ ├─lifecycle
│ │ ├─rlang
│ │ ├─utf8 1.2.6 ✨🔧 ⬇ (209.16 kB)
│ │ └─vctrs 0.6.5 ✨🔧 ⬇ (1.89 MB)
│ │   ├─cli
│ │   ├─glue
│ │   ├─lifecycle
│ │   └─rlang
│ ├─R6 2.6.1 ✨
│ ├─rlang
│ ├─tibble 3.3.0 ✨🔧 ⬇ (690.09 kB)
│ │ ├─cli
│ │ ├─lifecycle
│ │ ├─magrittr
│ │ ├─pillar
│ │ ├─pkgconfig 2.0.3 ✨ ⬇ (18.45 kB)
│ │ ├─rlang
│ │ └─vctrs
│ ├─tidyselect 1.2.1 ✨ ⬇ (224.68 kB)
│ │ ├─cli
│ │ ├─glue
│ │ ├─lifecycle
│ │ ├─rlang
│ │ ├─vctrs
│ │ └─withr 3.0.2 ✨ ⬇ (222.97 kB)
│ └─vctrs
├─ggplot2 4.0.1 ✨ ⬇ (8.47 MB)
│ ├─cli
│ ├─gtable 0.3.6 ✨ ⬇ (224.61 kB)
│ │ ├─cli
│ │ ├─glue
│ │ ├─lifecycle
│ │ └─rlang
│ ├─isoband 0.3.0 ✨🔧 ⬇ (1.96 MB)
│ │ ├─cli
│ │ └─rlang
│ ├─lifecycle
│ ├─rlang
│ ├─S7 0.2.1 ✨🔧 ⬇ (343.28 kB)
│ ├─scales 1.4.0 ✨ ⬇ (863.61 kB)
│ │ ├─cli
│ │ ├─farver 2.1.2 ✨🔧 ⬇ (1.97 MB)
│ │ ├─glue
│ │ ├─labeling 0.4.3 ✨ ⬇ (61.49 kB)
│ │ ├─lifecycle
│ │ ├─R6
│ │ ├─RColorBrewer 1.1-3 ✨ ⬇ (53.32 kB)
│ │ ├─rlang
│ │ └─viridisLite 0.4.2 ✨ ⬇ (1.30 MB)
│ ├─vctrs
│ └─withr
├─lubridate 1.9.4 ✨🔧 ⬇ (1.00 MB)
│ ├─generics
│ └─timechange 0.3.0 ✨🔧 ⬇ (878.32 kB)
├─magrittr
├─openxlsx 4.2.8.1 ✨🔧 ⬇ (3.42 MB)
│ ├─Rcpp 1.1.0 ✨🔧
│ ├─stringi 1.8.7 ✨🔧 ⬇ (14.77 MB)
│ └─zip 2.3.3 ✨🔧 ⬇ (227.39 kB)
├─R.utils 2.13.0 ✨ ⬇ (1.44 MB)
│ ├─R.methodsS3 1.8.2 ✨ ⬇ (82.09 kB)
│ └─R.oo 1.27.1 ✨ ⬇ (988.79 kB)
│   └─R.methodsS3
├─R6
├─rstudioapi 0.17.1 ✨ ⬇ (318.13 kB)
├─tibble
└─tidyr 1.3.1 ✨🔧 ⬇ (1.32 MB)      # <-------------- casse les *****
  ├─cli
  ├─dplyr
  ├─glue
  ├─lifecycle
  ├─magrittr
  ├─purrr 1.2.0 ✨🔧 ⬇ (578.73 kB)
  │ ├─cli
  │ ├─lifecycle
  │ ├─magrittr
  │ ├─rlang
  │ └─vctrs
  ├─rlang
  ├─stringr 1.6.0 ✨ ⬇ (330.97 kB)
  │ ├─cli
  │ ├─glue
  │ ├─lifecycle
  │ ├─magrittr
  │ ├─rlang
  │ ├─stringi
  │ └─vctrs
  ├─tibble
  ├─tidyselect
  └─vctrs

Key:  ✨ new |  ⬇ download | 👷🏽‍♀️ build | 🔧 compile

"
} # dep tree avt de virer tidyr
{
	"
	1.1.2 ✨👷🏼‍♂️ ⬇ (unknown size)
├─data.table 1.17.8 ✨🔧 ⬇ (3.05 MB)
├─diffobj 0.3.6 ✨🔧 ⬇ (1.03 MB)
│ └─crayon 1.5.3 ✨ ⬇ (164.41 kB)
├─dplyr 1.1.4 ✨🔧 ⬇ (1.60 MB)
│ ├─cli 3.6.5 ✨🔧
│ ├─generics 0.1.4 ✨ ⬇ (81.51 kB)
│ ├─glue 1.8.0 ✨🔧 ⬇ (173.70 kB)
│ ├─lifecycle 1.0.4 ✨ ⬇ (124.78 kB)
│ │ ├─cli
│ │ ├─glue
│ │ └─rlang 1.1.6 ✨🔧
│ ├─magrittr 2.0.4 ✨🔧
│ ├─pillar 1.11.1 ✨ ⬇ (656.90 kB)
│ │ ├─cli
│ │ ├─glue
│ │ ├─lifecycle
│ │ ├─rlang
│ │ ├─utf8 1.2.6 ✨🔧 ⬇ (209.16 kB)
│ │ └─vctrs 0.6.5 ✨🔧 ⬇ (1.89 MB)
│ │   ├─cli
│ │   ├─glue
│ │   ├─lifecycle
│ │   └─rlang
│ ├─R6 2.6.1 ✨
│ ├─rlang
│ ├─tibble 3.3.0 ✨🔧 ⬇ (690.09 kB)
│ │ ├─cli
│ │ ├─lifecycle
│ │ ├─magrittr
│ │ ├─pillar
│ │ ├─pkgconfig 2.0.3 ✨ ⬇ (18.45 kB)
│ │ ├─rlang
│ │ └─vctrs
│ ├─tidyselect 1.2.1 ✨ ⬇ (224.68 kB)
│ │ ├─cli
│ │ ├─glue
│ │ ├─lifecycle
│ │ ├─rlang
│ │ ├─vctrs
│ │ └─withr 3.0.2 ✨ ⬇ (222.97 kB)
│ └─vctrs
├─ggplot2 4.0.1 ✨ ⬇ (8.47 MB)
│ ├─cli
│ ├─gtable 0.3.6 ✨ ⬇ (224.61 kB)
│ │ ├─cli
│ │ ├─glue
│ │ ├─lifecycle
│ │ └─rlang
│ ├─isoband 0.3.0 ✨🔧 ⬇ (1.96 MB)
│ │ ├─cli
│ │ └─rlang
│ ├─lifecycle
│ ├─rlang
│ ├─S7 0.2.1 ✨🔧 ⬇ (343.28 kB)
│ ├─scales 1.4.0 ✨ ⬇ (863.61 kB)
│ │ ├─cli
│ │ ├─farver 2.1.2 ✨🔧 ⬇ (1.97 MB)
│ │ ├─glue
│ │ ├─labeling 0.4.3 ✨ ⬇ (61.49 kB)
│ │ ├─lifecycle
│ │ ├─R6
│ │ ├─RColorBrewer 1.1-3 ✨ ⬇ (53.32 kB)
│ │ ├─rlang
│ │ └─viridisLite 0.4.2 ✨ ⬇ (1.30 MB)
│ ├─vctrs
│ └─withr
├─lubridate 1.9.4 ✨🔧 ⬇ (1.00 MB)
│ ├─generics
│ └─timechange 0.3.0 ✨🔧 ⬇ (878.32 kB)
├─magrittr
├─readxl 1.4.5 ✨🔧
│ ├─cellranger 1.1.0 ✨
│ │ ├─rematch 2.0.0 ✨ ⬇ (16.61 kB)
│ │ └─tibble
│ └─tibble
├─writexl 1.5.4 ✨🔧
├─R.utils 2.13.0 ✨ ⬇ (1.44 MB)
│ ├─R.methodsS3 1.8.2 ✨ ⬇ (82.09 kB)
│ └─R.oo 1.27.1 ✨ ⬇ (988.79 kB)
│   └─R.methodsS3
├─R6
├─rstudioapi 0.17.1 ✨ ⬇ (318.13 kB)
└─tibble

Key:  ✨ new |  ⬇ download | 👷🏼‍♂️ build | 🔧 compile
	"
} # nouveau dep tree
