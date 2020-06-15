#' Compute SUCRA estimates
#'
#' Computes surface under the cumulative ranking curve (SUCRA) for each treatment in the network.
#' The ranking curve and rankogram are synonyms. DISLCAIMER: Not sure this deserves its own
#' function. Let's see down the road.
#'
#' @param rankProbabilities output of \code{computeRankProbabilities}
#'
#' @importFrom magrittr %>%
#'
#' @export

computeSucra <- function(rankProbabilities) {
	dplyr::group_by(rankProbabilities, exposureId) %>%
		dplyr::arrange(rank, .by_group = TRUE) %>%
		dplyr::summarise(sucra = sum(cumsum(probability[-dplyr::n()])) / (dplyr::n() - 1)) %>%
		dplyr::arrange(dplyr::desc(sucra)) # cosmetics
}
