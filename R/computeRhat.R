#' Compute the Rhat statistic for a given fit
#'
#' Returns a tidy tibble with the Rhat statistic for each parameter. Also handles node-splitting
#' models.
#'
#' @param fit an object of class \code{"mtc.result"} or \code{"mtc.nodesplit"}.
#'
#' @return Tidy tibble. If a node-splitting result is supplied, the first columns called
#'   \code{modelName} indicates the comparison that was split in the analysis.
#'   
#' @export

computeRhat <- function(fit) {
	# FIX: Get rid of :::
	
	if (class(fit) == "mtc.nodesplit") {
		return(dplyr::bind_rows(lapply(fit, computeRhat)))
	}
	
	gemtc:::as.mcmc.list.mtc.result(fit) %>% 
		coda::gelman.diag(autoburnin = FALSE, multivariate = FALSE) %>%
		with(psrf) %>%
		tibble::as_tibble(rownames = "parameter") %>%
		stats::setNames(c("parameterName", "rhatValue", "rhatUpperCI")) %>%
		dplyr::mutate(modelName = deriveModelName(fit)) %>%
		dplyr::select(modelName, everything())
}
