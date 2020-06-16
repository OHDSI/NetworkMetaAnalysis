#' Runs the analysis and returns results ready to be stored in database or used directly
#'
#' @param aggregatedResults tidy data frame, result of calling \code{fetchTcoEstimates}.
#' @param includeNodesplittingAnalysis boolean, should also node-splitting analyses be run? NB: This
#'   can be quite time-consuming, so is by default set to \code{FALSE}.
#' @param nWarmup scalar, the number of iterations the sampler should do before it starts sampling
#'   from the posterior. This should be sufficient to the chains to have converged.
#' @param nIter scalar, the number of posterior samples to draw per chain.
#' @param nChains scalar, the number of JAGS chains to run
#' @param nCores scalar, how many cores to use when running the analyses. If \code{> 1},
#'   \pkg{foreach} is used to parallel processing.
#' @param fitThin scalar, thinning factor. \code{1} (default) keeps all samples whereas e.g.
#'   \code{10} would keep only every tenth sample.
#' @param modelType string defining whether to run a random-effects (\code{"random"}, default) or a
#'   fixed-effects (\code{"fixed"}) mode.
#' @param drawsThin, scalar, thinning factor used when fetching posterior draws to keep in the
#'   database. Used to produce further derived quantities, density plots of posterior distributions
#'   and trace plots.
#' @param referenceTreatmentId string, the name of the exposure to use as the reference when
#'   computing pairwise estimates. If none given, the first in alphabetical order will be used.
#' @param preferredDirection scalar, preferential direction of the outcome. \code{1} means higher
#'   values are preferred, \code{-1} means lower values are preferred.
#' @param nmaIds scalar vector, if only specific network meta analyses should be run, their id's are
#'   supplied here. If none supplied, all will be run.
#'
#' @export

runAnalyses <- function(aggregatedResults, includeNodesplittingAnalysis = FALSE, nCores = 4, 
						nWarmup = 1000, nIter = 3000, nChains = 4, fitThin = 1, 
						referenceTreatmentId = NULL, preferredDirection = 1, 
						modelType = "random", drawsThin = 10, nmaIds = NULL) {

	# Reconcile argument defaults and user-specified arguments values	
	userArgs <- as.list(match.call()[-1])
	args <- as.list(formals())
	for (userArgName in names(userArgs)) {
		args[[userArgName]] <- userArgs[[userArgName]] # replace default
	}
	args$nmaIds <- nmaIds %||% unique(aggregatedResults$networkMetaAnalysisId)
	args$referenceTreatmentId <- args$referenceTreatmentId %||% paste(sort(aggregatedResults$treatment)[1])
	
	# Invoke appropriate function
	if (nCores > 1) {
		do.call(runParallelAnalyses, args)
	} else {
		args$nCores <- NULL # not used in sequential case
		do.call(runSequentialAnalyses, args)
	}
}

# ===

#' Run analyses in parallel
#' 
#' @importFrom foreach foreach %dopar%
#' 
#' @noRd

runParallelAnalyses <- function(nmaIds, aggregatedResults, nCores, ...) {
	# Capture all argument values in a list
	args <- as.list(match.call()[-1])
	args$nmaIds <- args$nCores <- args$aggregatedResults <- NULL # remove from args list
	
	cl <- snow::makeCluster(nCores)
	doSNOW::registerDoSNOW(cl)
	progBar <- utils::txtProgressBar(max = length(nmaIds), style = 3)
	output <- foreach(nmaId = nmaIds, 
					  .options.snow = list(progress = function(n) utils::setTxtProgressBar(progBar, n)), 
					  .packages = "NetworkMetaAnalysis", 
					  .inorder = FALSE) %dopar% {
					  	
		# Must re-assign these two within the call
		args$nmaId <- nmaId
		args$aggregatedResults <- aggregatedResults
		
		do.call(runSingleNetworkMetaAnalysis, args)
   	}
	close(progBar)
	snow::stopCluster(cl)
	return(output)
}

# ===

#' Run analyses sequentially
#' 
#' @noRd

runSequentialAnalyses <- function(nmaIds, aggregatedResults, ...) {
	args <- as.list(match.call()[-1])
	args <- plyr::rename(args, c("nmaIds" = ".data"))
	args$.progress <- "text"
	args$.fun <- runSingleNetworkMetaAnalysis
	do.call(plyr::llply, args)
}