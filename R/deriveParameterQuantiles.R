#' Get the quantiles of parameters of a model
#'
#' Does nothing to beautify the parameter names and keeps the parameters on their original scale,
#' just wrangles the data to give them a proper format.
#'
#' @param fit output from \code{fitNetwork}
#'
#' @noRd

deriveParameterQuantiles <- function(fit) {
	if (class(fit) == "mtc.nodesplit") {
		return(plyr::ldply(fit, deriveParameterQuantiles, .id = NULL))
	}
	
	gemtc:::summary.mtc.result(fit)$summaries$quantiles %>%
		tibble::as_tibble(rownames = "parameterName") %>%
		dplyr::mutate(modelName = deriveModelName(fit)) %>%
		dplyr::select(modelName, parameterName, 
					  median = "50%", cri95Lb = "2.5%", cri95Ub = "97.5%",
					  cri50Lb = "25%", cri50Ub = "75%")
}
