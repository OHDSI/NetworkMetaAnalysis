#' Compute relative-effect estimates compared to a reference exposure
#'
#' NOT USED, AS THE RESULT IS JUST A SUBSET OF computetePairwiseEstimates anyway.
#'
#' @param fit output from \code{fitNetwork}
#' @param referenceTreatment character, giving the name of the treatment to use as
#'   reference/baseline. The choice is arbitrary as the effects are relative, but placebo would be
#'   an obvious choice if available.
#'
#' @noRd
# @export

computeRelativeEffects <- function(fit, referenceTreatmentId) {
	if (class(fit) == "mtc.nodesplit") {
		fit <- fit$consistency
	}
	
	fetchRelativeEffects <- function(fit, referenceTreatmentId) {
		# tibble:::repaired_names (v3.0.1) gives new names with message()
		# so wrapping this inside a function to be called with messages supressed.
		# Has been fixed in Github version but didn't make it to CRAN (last checked 11 May 2020)
		
		gemtc::relative.effect(result = fit, t1 = referenceTreatmentId, preserve.extra = FALSE) %>%
			summary() %>%
			with(exp(summaries$quantiles)) %>%  # exp() to get HRs
			tibble::as_tibble(rownames = "parameter", 
							  .name_repair = ~ paste0("p", stringr::str_remove_all(., "\\%$"))) %>%
			dplyr::mutate_at("parameter", stringr::str_remove, pattern = "^d.") %>%
			tidyr::separate(col = parameter, into = c("ref", "target"), sep = "\\.") %>%
			dplyr::select(targetId = target,
						  comparatorId = ref, 
						  hr = p50, cri95Lb = p2.5, cri95Ub = p97.5, cri50Lb = p25, cri50Ub = p75)
	}
	
	suppressMessages(fetchRelativeEffects(fit, referenceTreatmentId))
}
