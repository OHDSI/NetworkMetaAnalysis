#' Get the node-splitting analyses results in tidy format
#'
#' Returns a tidy data frame with direct, indirect and consistency (network) effect estimates for each
#' target-comparator pair along with the probability of inconsistency between the direct and
#' indirect estimates.
#' 
#' @param fit the result of calling \code{fitNetwork} with \code{includeNodesplittingAnalysis = TRUE}.
#'
#' @return Tidy data frame.
#'
#' @export

fetchTidyNodesplitResults <- function(fit) {
	# FIX: Must figure our why summary(nodesplitFit) invoked summary.default and not 
	#      summary.mtc.nodesplit. Using ::: isn't sustainable
	
	if (class(fit) != "mtc.nodesplit") {
		stop("The supplied fit must be the result of running a node-splitting analysis. If you didn't call this function, please contact the package maintainer.", 
			 call. = FALSE)
	}
	
	# Helpers
	effectLookup <- c("cons" = "network", "dir" = "direct", "ind" = "indirect")
	
	reverseColnameComponents <- function(x) { # input: c("z_y", "t_u") => output: c("y.z", "u.t")
		lapply(stringr::str_split(x, "_"), rev) %>% 
			sapply(paste, collapse = "")
	} 
	
	wrangleNodesplitResults <- function(summaryOfNodesplitResults) {
		# tibble:::repaired_names (v3.0.1) gives new names with message()
		# so wrapping this inside a function to be called with messages supressed.
		# Has been fixed in Github version but didn't make it to CRAN as of 11 May 2020
		
		summaryOfNodesplitResults[c("dir.effect", "ind.effect", "cons.effect")] %>%
			dplyr::bind_rows(.id = "effect") %>%
			dplyr::left_join(tibble::as_tibble(summaryOfNodesplitResults$p.value), by = c("t1", "t2")) %>%
			dplyr::transmute(modelName = sprintf("d.%s.%s", t1, t2), # temporary, to facilitate joining below
							 splitExposureId1 = as.character(t1), # don't use as.numeric (t1 and t2 are factors)
							 splitExposureId2 = as.character(t2), # idem
							 effect = effectLookup[stringr::str_remove(effect, ".effect$")],
							 Hr = exp(pe),
							 Lb = exp(ci.l),
							 Ub = exp(ci.u),
							 pValue = p) %>%
			tidyr::pivot_wider(names_from = effect, 
							   values_from = c(Hr, Lb, Ub), 
							   names_repair = reverseColnameComponents) %>%
			dplyr::select(modelName, splitExposureId1, splitExposureId2, pValue, sort(tidyselect::peek_vars())) 
	}
	
	suppressMessages(wrangleNodesplitResults(gemtc:::summary.mtc.nodesplit(fit))) %>%
		dplyr::mutate(modelName = stringr::str_replace_all(modelName, c("d" = "nodesplit", "\\." = "_")))
}
