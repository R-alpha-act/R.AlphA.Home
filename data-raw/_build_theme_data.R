# ============================================================================='
# R.AlphA.Home - Build theme data
# ============================================================================='
# Generates:
#   - R/sysdata.rda  (colRef, colElts — internal)
#   - data/alphaThemes.rda (exported dataset)
#
# Usage: source("data-raw/_build_theme_data.R"); build_theme_data()
# ============================================================================='

build_theme_data <- function(pkg_path = getwd()) {

	old_wd <- getwd()
	on.exit(setwd(old_wd))
	setwd(pkg_path)

	# === colRef : palette de couleurs R.AlphA (SSOT) ========================='
	colRef <- list()
	{
		# couleurs R.AlphA officielles.
		colRef$ra_bleu_clair  <- "#778bc0"
		colRef$ra_bleu_moyen  <- "#404c7d"
		colRef$ra_bleu_fonce  <- "#1c2249"
		colRef$bleu_dark      <- "#0c101c"
		colRef$ra_vert        <- "#3B755F"
		colRef$ra_jaune       <- "#e2e386" # à utiliser pour attirer l'attention sur une action à faire.

		# couleurs non R.alphA mais utilisées quand même de temps en temps
		colRef$rouge          <- "#f25252"
		colRef$blanc          <- "#FFFFFF"
		colRef$gris_clair     <- "#DDDDDD"
		colRef$gris_bleu      <- "#7C98B6"
		colRef$gris_fond      <- "#3b4c61"
		colRef$turquoise      <- "#8fe6b3" # ne pas utiliser ce vieux bleu moche
		colRef$bleu_moyen     <- "#2e3861"
		colRef$bleu_glass     <- "#404C7D"
		colRef$bleu_ciel      <- "#66c9ff"
		colRef$bleu_clair     <- "#9BB5FA"
		colRef$bleu_pale      <- "#c2d1ff"
		colRef$rose           <- "#ff9bd3"
		colRef$marron         <- "#4D4333"
		colRef$rouge_fonce    <- "#B93029"
		colRef$bleu_semi      <- "#2e3861" # gutter background dark theme (gradient intermédiaire)
		colRef$testColor      <- "#F00" # flashy - for quick spotting
	} # Palette de couleurs globales en variables

	# === alphaThemes : registre multi-thèmes ================================='
	alphaThemes <- list()
	{
		# initialisation structure du theme
		alphaThemes$ralpha_dark <- list()
		alphaThemes$ralpha_dark$colElts <- list()
		alphaThemes$ralpha_dark$thName <- "ralpha_dark"
		tmpCols <- list()

		# Arrière-plans
		tmpCols$bg_editor               <- colRef$ra_bleu_fonce       # Fond principal de l'éditeur
		tmpCols$gutter_bg               <- colRef$bleu_semi           # Fond numéros de ligne

		# Texte principal
		tmpCols$text_primary            <- colRef$blanc               # Variables, texte normal

		# Syntaxe de base (les 4 couleurs qu'on voit le plus)
		tmpCols$syntax_string           <- colRef$turquoise           # "Chaînes de caractères"
		tmpCols$syntax_keyword          <- colRef$ra_jaune            # if, for, function, etc.
		tmpCols$syntax_symbol           <- colRef$bleu_ciel           # (), {}, <->, etc.
		tmpCols$syntax_function         <- colRef$bleu_ciel           # nom_de_fonction()
		tmpCols$syntax_comment          <- colRef$bleu_clair          # commentaires

		# Interaction utilisateur
		tmpCols$selection_background    <- "rgba(59, 117, 95, 0.6)"   # Texte sélectionné
		tmpCols$selection_border        <- "rgba(59, 117, 95, 1)"   # Texte sélectionné
		tmpCols$line_highlight          <- "rgba(119, 139, 192, 0.2)" # Ligne active
		tmpCols$cursor_color            <- colRef$blanc               # Curseur

		# Couleurs d'accent
		tmpCols$accent_blue             <- colRef$bleu_clair          # Numéros de ligne, éléments bleus
		tmpCols$accent_green            <- colRef$turquoise           # Packages, éléments verts
		tmpCols$accent_yellow           <- colRef$ra_jaune            # Entités, éléments jaunes
		tmpCols$accent_red              <- colRef$rose                # Erreurs, constantes
		tmpCols$accent_sombre           <- colRef$marron              # fold

		# Console
		tmpCols$console_background      <- colRef$bleu_clair          # Fond messages console
		tmpCols$console_error           <- colRef$rouge               # Erreurs
		tmpCols$console_warning         <- colRef$ra_jaune            # Warnings

		# Éléments techniques minimaux
		tmpCols$invalid_background      <- colRef$rouge_fonce         # Code invalide
		tmpCols$bracket_background      <- "rgba(128, 128, 128, 0.5)" # Brackets correspondantes
		tmpCols$console_error_bg        <- "#2e1a1a90"                # Fond erreurs console


		# enregistrement du theme dans alphaThemes
		alphaThemes$ralpha_dark$colElts <- tmpCols
	} # ralpha_dark
	{
		# initialisation structure du theme
		alphaThemes$ralpha_mid <- list()
		alphaThemes$ralpha_mid$colElts <- list()
		alphaThemes$ralpha_mid$thName <- "ralpha_mid"
		tmpCols <- list()

		# Arrière-plans
		tmpCols$bg_editor               <- colRef$gris_clair           # Fond principal de l'éditeur
		tmpCols$gutter_bg               <- colRef$bleu_moyen           # Fond numéros de ligne

		# Texte principal
		tmpCols$text_primary            <- colRef$blanc               # Variables, texte normal

		# Syntaxe de base (les 4 couleurs qu'on voit le plus)
		tmpCols$syntax_string           <- colRef$turquoise           # "Chaînes de caractères"
		tmpCols$syntax_keyword          <- colRef$ra_jaune            # if, for, function, etc.
		tmpCols$syntax_function         <- colRef$bleu_ciel           # nom_de_fonction()
		tmpCols$syntax_symbol           <- colRef$bleu_ciel           # (), {}, <->, etc.
		tmpCols$syntax_comment          <- colRef$ra_bleu_clair       # commentaires

		# Interaction utilisateur
		tmpCols$selection_background    <- "rgba(59, 117, 95, 0.6)"  # Texte sélectionné
		tmpCols$selection_border        <- "rgba(59, 117, 95, 1)"    # Bordure mots sélectionnés
		tmpCols$line_highlight          <- "rgba(119, 139, 192, 0.2)" # Ligne active
		tmpCols$cursor_color            <- colRef$blanc               # Curseur

		# Couleurs d'accent
		tmpCols$accent_blue             <- colRef$ra_bleu_clair       # Numéros de ligne, éléments bleus
		tmpCols$accent_green            <- colRef$ra_vert             # Packages, éléments verts
		tmpCols$accent_yellow           <- colRef$ra_jaune            # Entités, éléments jaunes
		tmpCols$accent_red              <- colRef$rouge               # Erreurs, constantes
		tmpCols$accent_sombre           <- colRef$gris_fond           # Fold background

		# Console
		tmpCols$console_background      <- colRef$ra_bleu_clair       # Fond messages console
		tmpCols$console_error_bg        <- "#2e1a1a90"                # Fond erreurs console
		tmpCols$console_error           <- colRef$rouge               # Erreurs
		tmpCols$console_warning         <- colRef$ra_jaune            # Warnings

		# Éléments techniques minimaux
		tmpCols$invalid_background      <- colRef$rouge_fonce         # Code invalide
		tmpCols$bracket_background      <- "rgba(128, 128, 128, 0.5)" # Brackets correspondantes


		# enregistrement du theme dans alphaThemes
		alphaThemes$ralpha_mid$colElts <- tmpCols

	} # ralpha_mid (work in progress)
	colElts <- alphaThemes$ralpha_dark$colElts

	# === Save ================================================================'
	message("\u2713 colRef (", length(colRef), " couleurs)")
	message("\u2713 colElts (", length(colElts), " \u00e9l\u00e9ments)")
	message("\u2713 alphaThemes (", length(alphaThemes), " th\u00e8mes)")

	# Internal data (colRef + colElts)
	suppressMessages(usethis::use_data(
		colRef
		, colElts
		, internal = TRUE
		, overwrite = TRUE
	))
	message("\u2713 R/sysdata.rda saved (colRef, colElts)")

	# Exported dataset (alphaThemes)
	suppressMessages(usethis::use_data(
		alphaThemes
		, overwrite = TRUE
	))
	message("\u2713 data/alphaThemes.rda saved")
}

# Execution directe (Ctrl+Enter dans RStudio)
if(0){
	build_theme_data()
}
