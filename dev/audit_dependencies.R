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
