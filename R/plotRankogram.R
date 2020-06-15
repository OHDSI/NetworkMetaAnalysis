#' Rank treatments by effictiveness
#' 
#' @param rankProbabilities output of \code{computeRankProbabilities}
#' @inherit plotTraces
#' 
#' @export

plotRankogram <- function(rankProbabilities, lineSize = 1, lineAlpha = 0.9) {
	# Order colouring according to SUCRA (informative) instead of alphabetical (arbitrary)
	sucraFactorLevels <- dplyr::arrange(computeSucra(rankProbabilities), sucra)$exposureId
	
	dplyr::mutate_at(rankProbabilities, "exposureId", factor, levels = sucraFactorLevels) %>%
		dplyr::group_by(exposureId) %>%
		dplyr::arrange(rank, .by_group = TRUE) %>%
		dplyr::mutate(cumulativeProbability = cumsum(probability)) %>%
		ggplot2::ggplot(ggplot2::aes(x = rank, y = cumulativeProbability, colour = exposureId)) +
			ggplot2::geom_line(size = lineSize, alpha = lineAlpha) +
			ggplot2::labs(x = "Rank", y = "Cumulative probability of having rank or better") +
			ggplot2::scale_x_continuous(breaks = 0:1000) + # show all ranks
			ggplot2::scale_colour_discrete(guide = ggplot2::guide_legend(reverse = TRUE)) + # correct order in legend
			packageTheme
}
