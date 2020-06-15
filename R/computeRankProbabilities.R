#' Compute the rank probabilities
#' 
#' To be used for, i.a., rankograms (that use the cumulative probability).
#' 
#' @inheritParams runAnalyses
#' 
#' @return Tidy data frame.
#' 
#' @export

computeRankProbabilities <- function(fit, preferredDirection = 1) {
	if (class(fit) == "mtc.nodesplit") {
		fit <- fit$consistency
	}

	rank_probs <- gemtc::rank.probability(fit, preferredDirection = preferredDirection)
	class(rank_probs) <- "matrix" # needed to make rank_probs behave
	t(rank_probs) %>%
		tibble::as_tibble() %>%
		dplyr::mutate(rank = seq(dplyr::n())) %>%
		tidyr::gather(exposureId, probability, -rank) %>%
		dplyr::select(exposureId, rank, probability)
}
