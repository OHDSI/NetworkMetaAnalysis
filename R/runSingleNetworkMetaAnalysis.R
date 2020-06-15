#' Run a single Network Meta Analysis
#'
#' Workhorse of the analysis pipeline. This is an internal function invoked either for sequential
#' (\code{nCores = 1}) or parallel processing (\code{nCores > 1}). Probably shouldn't be called by
#' the user.
#'
#' @inheritParams runAnalyses

runSingleNetworkMetaAnalysis <- function(nmaId, aggregatedResults, includeNodesplittingAnalysis = FALSE, 
										 nWarmup = 1000, nIter = 3000, nChains = 4, fitThin = 1, 
										 referenceTreatmentId = NULL, preferredDirection = 1, 
										 modelType = "random", drawsThin = 10) {
	# FIX: Add exposure names to networkMetaAnalysisData object
	# FIX: Get rid of gemtc:::
	
	o <- list() # output container
	
	networkData <- dplyr::filter(aggregatedResults, networkMetaAnalysisId == nmaId)
	
	o$graphNodes <- deriveNodes(networkData, labelOrientation = labelOrientation)
	o$graphEdges <- deriveEdges(networkData, nodes = o$graphNodes)
		# Test that data can be used for downstream functions
		# plotNetwork(graph_edges, graph_nodes) 
	
	## Fit network
	networkFit <- fitNetwork(networkData, includeNodesplittingAnalysis = includeNodesplittingAnalysis, 
							 nWarmup = nWarmup, nIter = nIter, fitThin = fitThin, nChains = nChains, 
							 modelType = modelType) 
	
	## Main results
	o$modelParameterSummaries <- deriveParameterQuantiles(networkFit)
	
	o$pairwiseEstimates <- computePairwiseEstimates(networkFit)
		# Test that data can be used for downstream functions
		# plotForest(relativeEffects, reference = referenceTreatmentId) 	
		
	
	o$rankProbabilities <- computeRankProbabilities(networkFit, preferredDirection = preferredDirection)
		# Test that data can be used for downstream functions
		# computeSucra(rankProbabilities)
		# plotRankogram(rankProbabilities)
	
	## Diagnostics
	o$posteriorSamples <- fetchPosteriorDraws(networkFit, drawsThin = drawsThin)
		# Test that data can be used for downstream functions
		# plotPosteriorDensities(posteriorDraws)
		# plotTraces(posteriorDraws)
	
	if (isTRUE(includeNodesplittingAnalysis)) { 
		o$nodesplitResults <- fetchTidyNodesplitResults(networkFit)
		consistencyFit <- networkFit$consistency # for deviance stats of main model
	} else {
		consistencyFit <- networkFit # for deviance stats of main model
	}
	
	o$potentialScaleReductionFactor <- computeRhat(networkFit)
		# Test that data can be used for downstream functions
		# plotRhat(rhatValues)
	
	o$networkMetaAnalysisDetails <- networkData %>%
		dplyr::distinct(networkMetaAnalysisId, outcomeId, analysisId) %>%
		dplyr::mutate(modelType,
					  hasNodesplitAnalysis = 1 * includeNodesplittingAnalysis, # make numeric
					  preferredDirection, 
					  nWarmupIterations = nWarmup, 
					  nSampleIterations = nIter, 
					  thin = fitThin,
					  devianceInformationCriterion = consistencyFit$deviance$DIC, # DIC at residual
					  sumOfLeverage = consistencyFit$deviance$pD, # = pD, effective number of parameters
					  meanSumOfResidualDeviance = consistencyFit$deviance$Dbar) # = Dbar
	
	o$devianceStatistics <- fetchDevianceStatistics(networkFit)
	
	o$networkMetaAnalysisData <- networkData
		# FIX: Consider removing the studyId column (it's a string and join a combination of the other columns)
	
	return(o)
}