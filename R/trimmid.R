#' Tronquer une chaîne au milieu
#'
#' Garde le début et la fin, remplace le milieu par "...".
#' Vectorisé.
#'
#' @param x Vecteur character.
#' @param n Longueur max (défaut 20).
#' @return Character tronqué si nécessaire.
#' @export
trimmid <- function(x, n = 20) {
	ifelse(
		nchar(x) <= n
		, x
		, {
			side <- (n - 3) %/% 2
			extra <- (n - 3) %% 2
			paste0(substr(x, 1, side + extra), "...", substr(x, nchar(x) - side + 1, nchar(x)))
		}
	)
}
