#' Run model on network to estimate parameters
#'
#' Runs a GeMTC-powered network meta-analysis on a subset of the results of calling
#' \code{fetchTcoEstimates} (i.e., setting analysisId and outcomeId) and
#'
#' @param networkData a \code{dataframe} holding the subset of a target-comparator-outcome estimates
#'   from the result of \code{fetchTcoEstimates}, with specified \code{outcomeId} and
#'   \code{analysisId}.
#' @inheritParams runAnalyses
#'
#' @export

fitNetwork <- function(networkData, includeNodesplittingAnalysis = FALSE, modelType = "random",
					   nWarmup = 1000, nIter = 3000, fitThin = 1, nChains = 4) {
	
	# FIX: Remove the sampling of comparisons to do in node-splitting analysis
	
	modelArgs <- list(network = createGemtcNetwork(networkData), likelihood = "poisson", link = "log", 
					  linearModel = modelType, dic = TRUE, n.chain = nChains)
	fitArgs <- list(n.adapt = nWarmup, n.iter = nIter, thin = fitThin)
	
	# captue.output silences GeMTC fitting process
	if (isTRUE(includeNodesplittingAnalysis)) {
		# fitArgs$comparisons <- gemtc::mtc.nodesplit.comparisons(createGemtcNetwork(networkData)) %>%
		# 	dplyr::sample_n(min(nrow(.), 2)) # keep runtime down during devel
		utils::capture.output(fit <- do.call(gemtc::mtc.nodesplit, c(modelArgs, fitArgs)))
	} else {
		fitArgs$model <- do.call(gemtc::mtc.model, c(modelArgs, type = "consistency"))
		utils::capture.output(fit <- do.call(gemtc::mtc.run, fitArgs))
	}
	
	return(fit)
}