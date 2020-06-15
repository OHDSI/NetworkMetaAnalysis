#' Puts the results together
#' 
#' Gives the data a format appropriate for the database, but could also be used in R directly.
#'
#' @param networkMetaAnalysisResults the output of \code{runAnalyses}.
#' @param discardPosteriorDraws boolean, should posterior draws be kept? These can take up quite
#'   some space, so there might be use cases in which one doesn't want to keep them. They are
#'   useful, however, for sampling diagnostics so are kept by default.
#'
#' @export

reconcileResults <- function(networkMetaAnalysisResults, discardPosteriorDraws = FALSE) {
	# FIX: networkMetaAnalysisId should really go directly in as the first column directly
	
	# Output container: list of lists (one for each table to reconcile)
	o <- unique(unlist(lapply(networkMetaAnalysisResults, names))) %>%
		sapply(function(.) list(), simplify = FALSE)
	
	# Reassign the tables from each study in the output (+ add networkMetaAnalysisId)
	for (nma in networkMetaAnalysisResults) {
		nmaId <- nma$networkMetaAnalysisDetails$networkMetaAnalysisId
		for (t in names(o)) {
			# Shouldn't be necessary check, as all analyses should have the same tables, but just in case
			if (!is.null(nma[[t]])) {
				o[[t]][[nmaId]] <- dplyr::mutate(nma[[t]], networkMetaAnalysisId = nmaId) %>%
					dplyr::select(networkMetaAnalysisId, dplyr::everything()) 
			} else {
				o[[t]][[nmaId]] <- NULL
			}
		}
	}
	
	if (isTRUE(discardPosteriorDraws)) {
		o$posteriorDraws <- NULL # remove list element altogether
	}
	
	return(structure(lapply(o, dplyr::bind_rows),
					 class = "ReconciledOhdsiNetworkMetaAnalysis"))
}
