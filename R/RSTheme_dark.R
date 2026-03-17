#' @title Apply R.AlphA dark theme to RStudio
#' @description Generates and applies a dark RStudio editor theme using the
#' R.AlphA color palette. Creates a temporary .rstheme file, installs it
#' via rstudioapi, then cleans up.
#'
#' @param ... Named color overrides to apply. Names should match elements in colElts
#'   (e.g., `accent_blue = "#7eb8da"`, `text_primary = "#e0e0e0"`).
#' @param thName Character. Name of the theme file (without extension). Default: "_custom"
#'
#' @return Invisible NULL. Creates a temporary .rstheme file and applies the theme to RStudio.
#'
#' @examples
#' \dontrun{
#' RSTheme_dark()
#' RSTheme_dark(accent_blue = "#00FF00")
#' RSTheme_dark(thName = "my_theme")
#' }
#'
#' @export

RSTheme_dark <- function(..., thName = "_custom"){
	rgbpc <- function(...) rgb(..., maxColorValue = 100) # rgb mais en %
	{
		call_args <- list(...)
		notHere <- setdiff(names(call_args), names(colElts))
		if(length(notHere)) message("not here : ", notHere)
		colElts <- modifyList(colElts, call_args)
	} # implement user adjustments
	{
		color_ace <- c(
			NULL
			, "/* rs-theme-name: ", thName," */"
			, "/* rs-theme-is-dark: TRUE */"
			," "
			,"/* elt_1 colonne des nums de lignes à gauche */"
			,".ace_gutter {"
			# linear gradient ra_bleu_fonce -> bleu_semi
			,"background: linear-gradient(to right, #2e3861, #1c2249, #2e3861);"
			,paste0("color: ", colElts$accent_blue, ";")
			,paste0("border-right: 1px solid ", colElts$accent_blue, "40;")
			,"padding-right: 1px;"
			,"}"
			," "
			,"/* marge à gauche dans les fenêtres d'éditon */"
			,".ace_scroller {"
			,"padding-left: 0px;"
			,"}"
			," "
			,"/* ligne de marquage à 80 */"
			,".ace_print-margin {"
			,"width: 1px;"
			,paste0("background: ", colElts$accent_blue, "40;")
			,"}"
			," "
			,"/* fond d'écran général + text */"
			,".ace_editor, .ace_editor_theme .profvis-flamegraph, .ace_editor_theme {"
			# linear gradient ra_bleu_fonce -> bleu_semi
			,"background-image: linear-gradient(135deg, #1c2249f0 0%, #2e3861fb 100%),"
			,"url('data:image/svg+xml,%3C%3Fxml%20version%3D%221.0%22%20encoding%3D%22UTF-8%22%20standalone%3D%22no%22%3F%3E%3C!--%20Created%20with%20Inkscape%20(http%3A%2F%2Fwww.inkscape.org%2F)%20--%3E%3Csvg%20%20%20width%3D%221833.3226mm%22%20%20%20height%3D%222002.3464mm%22%20%20%20viewBox%3D%220%200%201833.3227%202002.3465%22%20%20%20version%3D%221.1%22%20%20%20id%3D%22svg1%22%20%20%20xml%3Aspace%3D%22preserve%22%20%20%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20%20%20xmlns%3Asvg%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%3E%3Cdefs%20%20%20%20%20id%3D%22defs1%22%20%2F%3E%3Cg%20%20%20%20%20id%3D%22layer1%22%20%20%20%20%20transform%3D%22translate(-33913.155%2C-8325.276)%22%3E%3Cg%20%20%20%20%20%20%20id%3D%22g18-3-4-9-0-7-5-4-5-0-3%22%20%20%20%20%20%20%20style%3D%22display%3Ainline%3Bstroke%3A%23010101%3Bstroke-width%3A2.7%3Bstroke-dasharray%3Anone%3Bstroke-opacity%3A1%3Bpaint-order%3Astroke%20fill%20markers%22%20%20%20%20%20%20%20transform%3D%22translate(32869.573%2C12250.782)%22%3E%3Cpath%20%20%20%20%20%20%20%20%20id%3D%22path51-0-6-7-7-9-7-3-5-9-86-41-9-1%22%20%20%20%20%20%20%20%20%20style%3D%22fill%3A%233b755f%3Bfill-opacity%3A1%3Bstroke%3A%23010101%3Bstroke-width%3A2.7%3Bstroke-dasharray%3Anone%3Bstroke-opacity%3A1%3Bpaint-order%3Astroke%20fill%20markers%22%20%20%20%20%20%20%20%20%20d%3D%22m%201409.4478%2C-3209.8732%20c%20-147.1208%2C2.1265%20-256.2474%2C78.8828%20-315.4195%2C181.2453%20-62.1082%2C107.4417%20-66.0542%2C241.1029%20-7.4301%2C348.6635%2089.7145%2C164.6038%20244.4527%2C192.7602%20317.6106%2C192.7602%201.4819%2C0%203.3545%2C1e-4%205.2396%2C1e-4%20v%20-91.3057%20c%20-1.5578%2C0%20-3.1959%2C-0.049%20-4.6944%2C-0.049%20-50.4726%2C0%20-126.4203%2C-22.8778%20-165.4054%2C-56.5118%20-67.1447%2C-57.9284%20-101.2461%2C-128.1531%20-104.8205%2C-200.0333%20-3.5743%2C-71.8802%2024.0561%2C-142.916%2076.507%2C-201.1862%2072.4193%2C-80.4542%20183.8409%2C-82.2616%20193.7044%2C-82.2616%202.0505%2C0%203.155%2C-0.016%204.7089%2C-0.016%20z%22%20%20%20%20%20%20%20%20%20transform%3D%22translate(4.5833332e-5%2C-5.8333335e-5)%22%20%2F%3E%3Cpath%20%20%20%20%20%20%20%20%20style%3D%22opacity%3A1%3Bfill%3A%23404c7d%3Bfill-opacity%3A1%3Bstroke%3A%23010101%3Bstroke-width%3A2.7%3Bstroke-dasharray%3Anone%3Bstroke-opacity%3A1%3Bpaint-order%3Astroke%20fill%20markers%22%20%20%20%20%20%20%20%20%20d%3D%22m%201421.1515%2C-3924.1557%20c%200%2C0%20164.7233%2C10e-4%20190.5251%2C10e-4%20v%201999.6457%20h%20-190.5251%20z%22%20%20%20%20%20%20%20%20%20id%3D%22path7-87-30-0-1-51-9-4-9-7-9-5-1-6-7-3-1%22%20%20%20%20%20%20%20%20%20transform%3D%22translate(4.5833332e-5%2C-5.8333335e-5)%22%20%2F%3E%3Cpath%20%20%20%20%20%20%20%20%20style%3D%22opacity%3A1%3Bfill%3A%231c2249%3Bfill-opacity%3A1%3Bstroke%3A%23010101%3Bstroke-width%3A2.7%3Bstroke-dasharray%3Anone%3Bstroke-opacity%3A1%3Bpaint-order%3Astroke%20fill%20markers%22%20%20%20%20%20%20%20%20%20d%3D%22m%201903.2786%2C-3924.1558%20c%20644.7566%2C101.6316%20790.2647%2C365.2905%20734.9413%2C597.4467%20-48.7747%2C204.6754%20-233.6325%2C355.5644%20-447.6424%2C466.2458%20-214.0099%2C110.6814%20-339.9567%2C163.7748%20-567.2069%2C244.684%20v%2091.3057%20c%20281.9678%2C-141.2557%20614.9711%2C-281.3141%20857.8056%2C-448.1426%20325.5453%2C-223.6513%20395.4999%2C-360.4272%20394.3678%2C-494.9653%20-1.7628%2C-209.4911%20-234.9455%2C-415.968%20-626.2762%2C-456.5743%20z%22%20%20%20%20%20%20%20%20%20id%3D%22path17-1-4-4-8-8-4-5-8-1-7-4%22%20%2F%3E%3Cpath%20%20%20%20%20%20%20%20%20d%3D%22m%201623.3807%2C-3183.6037%20v%2091.3056%20c%20175.9339%2C52.8088%20309.6018%2C172.9979%20417.931%2C316.6948%208.248%2C10.9408%2016.1988%2C22.1514%2024.1624%2C33.3499%20249.111%2C350.3022%20363.0102%2C817.1626%20574.3019%2C817.7387%20153.6516%2C0.4189%20231.5437%2C-204.0347%20235.7189%2C-233.1424%20v%20-268.0916%20c%20-65.8513%2C350.4741%20-285.421%2C399.7033%20-409.5435%2C251.0157%20-155.2037%2C-185.9201%20-245.6728%2C-403.2758%20-354.2838%2C-590.7712%20-7.1337%2C-12.315%20-14.6837%2C-24.0182%20-21.9976%2C-36.0578%20-108.0344%2C-177.8374%20-238.1732%2C-323.9735%20-466.2893%2C-382.0417%20z%22%20%20%20%20%20%20%20%20%20style%3D%22fill%3A%23778bc0%3Bstroke%3A%23010101%3Bstroke-width%3A2.7%3Bstroke-dasharray%3Anone%3Bstroke-opacity%3A1%3Bpaint-order%3Astroke%20fill%20markers%22%20%20%20%20%20%20%20%20%20id%3D%22path9-3-7-6-6-1-0-1-3-5-9%22%20%20%20%20%20%20%20%20%20transform%3D%22translate(-2.0833359e-6%2C1.4583331e-5)%22%20%2F%3E%3C%2Fg%3E%3C%2Fg%3E%3C%2Fsvg%3E');"
			,"background-size: auto 120%;"
			,"background-repeat: no-repeat;"
			,"background-position: center;"
			,paste0("color: ", colElts$text_primary, ";")
			,"}"
			," "
			,".ace_cursor {"
			,paste0("color: ", colElts$cursor_color, ";")
			,"}"
			," "
			,"/* mise en évidence des mots séléctionnés */"
			,".ace_marker-layer .ace_selection {"
			,paste0("background: ", colElts$selection_background, ";")
			,"}"
			," "
			,"/* mots identiques mis en évidence */"
			,".ace_marker-layer .ace_selected-word {"
			,paste0("border: 1.5px solid ", colElts$selection_border, ";")
			,"border-radius: 2px;"
			,"}"
			," "
			,".ace_marker-layer .ace_active-line {"
			,paste0("background: ", colElts$line_highlight, ";")
			,"}"
			," "
			,".ace_gutter-active-line {"
			,paste0("background-color: ", colElts$line_highlight, ";")
			,"}"
			," "
			,"/* === SYNTAXE PRINCIPALE === */"
			,".ace_keyword,"
			,".ace_meta {"
			,paste0("color: ", colElts$syntax_keyword, ";")
			,"}"
			,".ace_keyword.ace_operator {"
			,paste0("color: ", colElts$syntax_symbol, ";")
			,"}"
			," "
			,".ace_constant,"
			,".ace_constant.ace_character,"
			,".ace_constant.ace_character.ace_escape,"
			,".ace_constant.ace_other {"
			,paste0("color: ", colElts$accent_red, ";")
			,"}"
			," "
			,".ace_support {"
			,paste0("color: ", colElts$accent_green, ";")
			,"}"
			," "
			,".ace_support.ace_constant {"
			,paste0("color: ", colElts$accent_red, ";")
			,"}"
			," "
			,".ace_support.ace_function {"
			,paste0("color: ", colElts$syntax_function, ";")
			,"}"
			," "
			,".ace_storage {"
			,paste0("color: ", colElts$syntax_keyword, ";")
			,"}"
			," "
			,".ace_entity {"
			,paste0("color: ", colElts$accent_yellow, ";")
			,"}"
			," "
			,".ace_string {"
			,paste0("color: ", colElts$syntax_string, ";")
			,"}"
			," "
			,".ace_string.ace_regexp {"
			,paste0("color: ", colElts$syntax_string, ";")
			,"font-style: italic;"
			,"}"
			," "
			,".ace_comment {"
			,"font-style: italic;"
			,paste0("color: ", colElts$syntax_comment, ";")
			,"}"
			," "
			,".ace_variable {"
			,paste0("color: ", colElts$text_primary, ";")
			,"}"
			," "
			,".ace_variable.ace_language {"
			,paste0("color: ", colElts$accent_blue, ";")
			,"font-weight: bold;"
			,"}"
			," "
			,".ace_meta.ace_tag {"
			,paste0("color: ", colElts$accent_blue, ";")
			,"}"
			," "
			,"/* === ERREURS ET VALIDATION === */"
			,".ace_invalid {"
			,paste0("color: ", colElts$text_primary, ";")
			,paste0("background-color: ", colElts$invalid_background, ";")
			,"}"
			," "
			,".ace_error {"
			,paste0("border-bottom: 1.5px solid ", colElts$accent_red, ";")
			,paste0("background-color: ", colElts$accent_red, "20;")
			,"}"
			," "
			,".ace_warning {"
			,paste0("border-bottom: 1.5px solid ", colElts$accent_yellow, ";")
			,paste0("background-color: ", colElts$accent_yellow, "20;")
			,"}"
			," "
			,".ace_info {"
			,paste0("border-bottom: 1.5px solid ", colElts$accent_blue, ";")
			,paste0("background-color: ", colElts$accent_blue, "15;")
			,"}"
			," "
			,"/* === AUTOCOMPLÉTION === */"
			,".ace_autocomplete {"
			,paste0("background-color: ", colElts$gutter_bg, ";")
			,paste0("border: 1px solid ", colElts$accent_blue, "60;")
			,"border-radius: 4px;"
			,"box-shadow: 0 2px 8px rgba(0,0,0,0.3);"
			,"}"
			," "
			,".ace_autocomplete .ace_completion-highlight {"
			,paste0("color: ", colElts$accent_yellow, ";")
			,"font-weight: bold;"
			,"}"
			," "
			,".ace_autocomplete .ace_completion-meta {"
			,paste0("color: ", colElts$syntax_comment, ";")
			,"font-style: italic;"
			,"}"
			," "
			,".ace_autocomplete .ace_line {"
			,paste0("color: ", colElts$text_primary, ";")
			,"}"
			," "
			,".ace_autocomplete .ace_line.ace_selected {"
			,paste0("background-color: ", colElts$selection_background, ";")
			,"}"
			," "
			,"/* === RECHERCHE ET REMPLACEMENT === */"
			,".ace_search_form, .ace_replace_form {"
			,paste0("background-color: ", colElts$gutter_bg, ";")
			,paste0("border: 1px solid ", colElts$accent_blue, "60;")
			,paste0("color: ", colElts$text_primary, ";")
			,"border-radius: 4px;"
			,"}"
			," "
			,".ace_searchbtn, .ace_replacebtn {"
			,paste0("background-color: ", colElts$accent_blue, ";")
			,paste0("color: ", colElts$bg_editor, ";")
			,"border: none;"
			,"border-radius: 8px;"
			,"}"
			," "
			,".ace_searchbtn:hover, .ace_replacebtn:hover {"
			,paste0("background-color: ", colElts$accent_blue, "CC;")
			,"}"
			," "
			,"/* === FOLDING ET NAVIGATION === */"
			,".ace_fold {"
			,paste0("background-color: ", colElts$accent_sombre, ";")
			,"}"
			," "
			,".ace_fold:hover {"
			,paste0("background-color: ", colElts$accent_blue, "80;")
			,"}"
			,".ace_fold-widget {"
			,"background-color: transparent;"
			,"border: none;"
			,"}"
			," "
			,".ace_fold-widget:hover {"
			,paste0("background-color: ", colElts$accent_blue, "80;")
			,"}"
			," "
			,"/* === SCROLLBARS === */"
			,".ace_scrollbar::-webkit-scrollbar {"
			,"width: 5px;"
			,"height: 5px;"
			,"}"
			," "
			,".ace_scrollbar::-webkit-scrollbar-track {"
			,paste0("background: ", colElts$bg_editor, ";")
			,"}"
			," "
			,".ace_scrollbar::-webkit-scrollbar-thumb {"
			,paste0("background: ", colElts$accent_blue, "60;")
			,"border-radius: 4px;"
			,"}"
			," "
			,".ace_scrollbar::-webkit-scrollbar-thumb:hover {"
			,paste0("background: ", colElts$accent_blue, "80;")
			,"}"
			," "
			,"/* === BRACKETS ET CORRESPONDANCES === */"
			,".ace_bracket {"
			,"margin: 0 !important;"
			,"border: 0 !important;"
			,paste0("background-color: ", colElts$bracket_background, ";")
			,"}"
			," "
			,".ace_marker-layer .ace_bracket {"
			,"margin: -1px 0 0 -1px;"
			,paste0("border: 1px solid ", colElts$accent_yellow, "80;")
			,"border-radius: 2px;"
			,"}"
			," "
			,"/* === CONSOLE === */"
			,".ace_console_error {"
			,paste0("background-color: ", colElts$console_error_bg, ";")
			,paste0("color: ", colElts$console_error, ";")
			,paste0("border-left: 2px solid ", colElts$console_error, " !important;")
			,"}"
			," "
			,"/* === MARKDOWN (pour R Markdown) === */"
			,".ace_heading,"
			,".ace_markup.ace_heading {"
			,paste0("color: ", colElts$accent_blue, ";")
			,paste0("background-color: ", colElts$bg_editor, ";")
			,"font-weight: bold;"
			,"}"
			," "
			,".ace_list,"
			,".ace_markup.ace_list {"
			,paste0("background-color: ", colElts$gutter_bg, ";")
			,"}"
			," "
			,"/* === DEBUGGING === */"
			,".ace_marker-layer .ace_step {"
			,paste0("background: ", colElts$console_warning, "40;")
			,"}"
			," "
			,".ace_marker-layer .ace_active_debug_line {"
			,"position: absolute;"
			,"z-index: -1;"
			,paste0("background-color: ", colElts$accent_green, "40;")
			,"}"
			," "
			,".ace_marker-layer .ace_find_line {"
			,"position: absolute;"
			,"z-index: -1;"
			,paste0("background-color: ", colElts$accent_blue, "40;")
			,"}"
			," "
			,".ace_marker-layer .ace_foreign_line {"
			,"position: absolute;"
			,"z-index: -1;"
			,paste0("background-color: ", colElts$accent_blue, "30;")
			,"}"
			," "
			,"/* === ÉLÉMENTS TECHNIQUES === */"
			,".ace_invisible {"
			,paste0("color: ", colElts$syntax_comment, "60;")
			,"}"
			," "
			,".ace_indent-guide {"
			,"background: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAACCAYAAACZgbYnAAAAEklEQVQImWNgYGBgYHCLSvkPAAP3AgSDTRd4AAAAAElFTkSuQmCC) right repeat-y;"
			,"}"
			," "
			,".nocolor.ace_editor .ace_line span {"
			,paste0("color: ", colElts$syntax_keyword, " !important;")
			,"}"
			," "
			,".ace_node-selector {"
			,paste0("background-color: ", colElts$accent_green, ";")
			,"}"
			," "
			,".ace_comment-highlight {"
			,paste0("color: ", colElts$bg_editor, ";")
			,paste0("background-color: ", colElts$accent_yellow, ";")
			,"}"
		)
	} # color_ace
	{
		color_others <- c(
			NULL
			, "/* bord jaune en haute de la fenêtre active */"
			, ".rstudio-themes-flat .rstudio-themes-dark .gwt-TabLayoutPanelTab.gwt-TabLayoutPanelTab-selected {"
			,paste0("border-top: 2px solid ", colElts$accent_yellow, " !important;")
			,"border-radius: 4px !important;"
			, "}"
			," "
		)
	} # bord jaune --> color_others
	{
		color_term <- c(
			NULL
			, "/* === TERMINAL - Configuration minimale === */"
			, ".terminal {"
			, paste0("background-color: ", colElts$bg_editor, ";")
			, paste0("color: ", colElts$text_primary, ";")
			, "font-feature-settings: \"liga\" 0;"
			, "user-select: none;"
			, "}"
			, " "
			, "/* Curseur */"
			, ".terminal.xterm-cursor-style-block.focus:not(.xterm-cursor-blink-on) .terminal-cursor {"
			, paste0("background-color: ", colElts$cursor_color, ";")
			, paste0("color: ", colElts$bg_editor, ";")
			, "}"
			, " "
			, ".terminal:not(.focus) .terminal-cursor {"
			, paste0("outline: 1px solid ", colElts$cursor_color, ";")
			, "outline-offset: -1px;"
			, "}"
			, " "
			, "/* Sélection */"
			, ".terminal .xterm-selection div {"
			, "position: absolute;"
			, paste0("background-color: ", colElts$selection_background, ";")
			, "}"
			, " "
			, "/* Viewport */"
			, ".terminal .xterm-viewport {"
			, paste0("background-color: ", colElts$bg_editor, ";")
			, "overflow-y: scroll;"
			, "}"
			, " "
			, "/* === COULEURS TERMINAL (8 couleurs de base) === */"
			, "/* Noir */"
			, paste0(".xtermColor0 { color: ", colElts$bg_editor, " !important; }")
			, paste0(".xtermBgColor0 { background-color: ", colElts$bg_editor, "; }")
			, " "
			, "/* Rouge */"
			, paste0(".xtermColor1 { color: ", colElts$accent_red, " !important; }")
			, paste0(".xtermBgColor1 { background-color: ", colElts$accent_red, "; }")
			, " "
			, "/* Vert */"
			, paste0(".xtermColor2 { color: ", colElts$accent_green, " !important; }")
			, paste0(".xtermBgColor2 { background-color: ", colElts$accent_green, "; }")
			, " "
			, "/* Jaune */"
			, paste0(".xtermColor3 { color: ", colElts$accent_yellow, " !important; }")
			, paste0(".xtermBgColor3 { background-color: ", colElts$accent_yellow, "; }")
			, " "
			, "/* Bleu */"
			, paste0(".xtermColor4 { color: ", colElts$accent_blue, " !important; }")
			, paste0(".xtermBgColor4 { background-color: ", colElts$accent_blue, "; }")
			, " "
			, "/* Magenta */"
			, paste0(".xtermColor5 { color: ", colElts$syntax_function, " !important; }")  # Bleu ciel pour magenta
			, paste0(".xtermBgColor5 { background-color: ", colElts$syntax_function, "; }")
			, " "
			, "/* Cyan */"
			, paste0(".xtermColor6 { color: ", colElts$syntax_string, " !important; }")  # Turquoise pour cyan
			, paste0(".xtermBgColor6 { background-color: ", colElts$syntax_string, "; }")
			, " "
			, "/* Blanc */"
			, paste0(".xtermColor7 { color: ", colElts$text_primary, " !important; }")
			, paste0(".xtermBgColor7 { background-color: ", colElts$text_primary, "; }")
			, " "
			, "/* === COULEURS TERMINAL (8 couleurs vives) === */"
			, "/* Gris foncé */"
			, paste0(".xtermColor8 { color: ", colElts$syntax_comment, " !important; }")
			, paste0(".xtermBgColor8 { background-color: ", colElts$syntax_comment, "; }")
			, " "
			, "/* Rouge vif */"
			, paste0(".xtermColor9 { color: ", colElts$console_error, " !important; }")
			, paste0(".xtermBgColor9 { background-color: ", colElts$console_error, "; }")
			, " "
			, "/* Vert vif */"
			, paste0(".xtermColor10 { color: ", colElts$accent_green, " !important; }")
			, paste0(".xtermBgColor10 { background-color: ", colElts$accent_green, "; }")
			, " "
			, "/* Jaune vif */"
			, paste0(".xtermColor11 { color: ", colElts$syntax_keyword, " !important; }")
			, paste0(".xtermBgColor11 { background-color: ", colElts$syntax_keyword, "; }")
			, " "
			, "/* Bleu vif */"
			, paste0(".xtermColor12 { color: ", colElts$accent_blue, " !important; }")
			, paste0(".xtermBgColor12 { background-color: ", colElts$accent_blue, "; }")
			, " "
			, "/* Magenta vif */"
			, paste0(".xtermColor13 { color: ", colElts$syntax_function, " !important; }")
			, paste0(".xtermBgColor13 { background-color: ", colElts$syntax_function, "; }")
			, " "
			, "/* Cyan vif */"
			, paste0(".xtermColor14 { color: ", colElts$syntax_string, " !important; }")
			, paste0(".xtermBgColor14 { background-color: ", colElts$syntax_string, "; }")
			, " "
			, "/* Blanc vif */"
			, paste0(".xtermColor15 { color: ", colElts$text_primary, " !important; }")
			, paste0(".xtermBgColor15 { background-color: ", colElts$text_primary, "; }")
		)
	} # color_term
	{
		color_rainbow <- c(
			NULL
			,"/* === RAINBOW INDENT - Lignes colorées === */"
			,paste0(".rstudio_rainbow_indent_guides .ace_line .ace_indent-guide:nth-child(7n+1) { background: linear-gradient(to left, ", colElts$accent_red, " 1px, transparent 1px); }")
			,paste0(".rstudio_rainbow_indent_guides .ace_line .ace_indent-guide:nth-child(7n+2) { background: linear-gradient(to left, ", colElts$accent_yellow, " 1px, transparent 1px); }")
			,paste0(".rstudio_rainbow_indent_guides .ace_line .ace_indent-guide:nth-child(7n+3) { background: linear-gradient(to left, ", colElts$accent_green, " 1px, transparent 1px); }")
			,paste0(".rstudio_rainbow_indent_guides .ace_line .ace_indent-guide:nth-child(7n+4) { background: linear-gradient(to left, ", colElts$accent_blue, " 1px, transparent 1px); }")
			,paste0(".rstudio_rainbow_indent_guides .ace_line .ace_indent-guide:nth-child(7n+5) { background: linear-gradient(to left, ", colElts$syntax_string, " 1px, transparent 1px); }")
			,paste0(".rstudio_rainbow_indent_guides .ace_line .ace_indent-guide:nth-child(7n+6) { background: linear-gradient(to left, ", colElts$syntax_function, " 1px, transparent 1px); }")
			,paste0(".rstudio_rainbow_indent_guides .ace_line .ace_indent-guide:nth-child(7n+7) { background: linear-gradient(to left, ", colElts$syntax_comment, " 1px, transparent 1px); }")
			," "
			,"/* === RAINBOW INDENT - Zones remplies === */"
			,paste0(".rstudio_rainbow_indent_fills .ace_line .ace_indent-guide:nth-child(7n+1) { background: linear-gradient(to left, ", colElts$accent_red, " 1px, ", colElts$accent_red, "30 1px); }")
			,paste0(".rstudio_rainbow_indent_fills .ace_line .ace_indent-guide:nth-child(7n+2) { background: linear-gradient(to left, ", colElts$accent_yellow, " 1px, ", colElts$accent_yellow, "30 1px); }")
			,paste0(".rstudio_rainbow_indent_fills .ace_line .ace_indent-guide:nth-child(7n+3) { background: linear-gradient(to left, ", colElts$accent_green, " 1px, ", colElts$accent_green, "30 1px); }")
			,paste0(".rstudio_rainbow_indent_fills .ace_line .ace_indent-guide:nth-child(7n+4) { background: linear-gradient(to left, ", colElts$accent_blue, " 1px, ", colElts$accent_blue, "30 1px); }")
			,paste0(".rstudio_rainbow_indent_fills .ace_line .ace_indent-guide:nth-child(7n+5) { background: linear-gradient(to left, ", colElts$syntax_string, " 1px, ", colElts$syntax_string, "30 1px); }")
			,paste0(".rstudio_rainbow_indent_fills .ace_line .ace_indent-guide:nth-child(7n+6) { background: linear-gradient(to left, ", colElts$syntax_function, " 1px, ", colElts$syntax_function, "30 1px); }")
			,paste0(".rstudio_rainbow_indent_fills .ace_line .ace_indent-guide:nth-child(7n+7) { background: linear-gradient(to left, ", colElts$syntax_comment, " 1px, ", colElts$syntax_comment, "30 1px); }")
		)
	} # color_rainbow
	editorText <- c(
		NULL
		, color_ace
		, color_term
		, color_others
		, color_rainbow
	)
	{
		tmpDir <- tempdir()
		themeFile <- file.path(tmpDir, paste0(thName, ".rstheme"))
		writeLines(con = themeFile, text = editorText)
		on.exit(unlink(c(themeFile, switchFile), force = TRUE), add = TRUE)

		# Buffer theme : même couleurs, nom différent → pas de flash pendant le swap
		switchName <- paste0(thName, "_switch")
		switchFile <- file.path(tmpDir, paste0(switchName, ".rstheme"))
		switchText <- sub(
			paste0("/* rs-theme-name: ", thName, " */")
			, paste0("/* rs-theme-name: ", switchName, " */")
			, editorText
			, fixed = TRUE
		)
		writeLines(con = switchFile, text = switchText)
		suppressWarnings(rstudioapi::addTheme(switchFile, force = TRUE, apply = TRUE))
		suppressWarnings(rstudioapi::addTheme(themeFile, force = TRUE, apply = TRUE))
		invisible(NULL)
	} # write theme file, apply it
} # RSTheme
