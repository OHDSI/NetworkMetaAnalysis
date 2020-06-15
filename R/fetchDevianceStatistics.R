#' Fetch deviance stats for a fit
#'
#' Internal function. Will give appropriate results also for a node-splitting analysis. 
#'
#' @param fit output from \code{fitNetwork}
#'
#' @noRd

fetchDevianceStatistics <- function(fit) {
	if (class(fit) == "mtc.nodesplit") {
		return(plyr::ldply(fit, fetchDevianceStatistics, .id = NULL))
	}

	with(fit$deviance, tibble::tibble(modelName = deriveModelName(fit),
									  devianceInformationCriterion = DIC,
									  sumOfLeverage = pD, 
									  meanSumOfResidualDeviance = Dbar))
}
