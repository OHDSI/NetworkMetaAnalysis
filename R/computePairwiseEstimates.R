#' Compute all pairwise estimates
#' 
#' After fitting the model, this function pulls out all pairwise network effect (direct and indirect
#' combined) in a tidy format, on the hazard rate scale.
#'
#' @param fit the output of \code{fitNetwork}
#' 
#' @importFrom magrittr %>%
#' 
#' @export

computePairwiseEstimates <- function(fit) {
	if (class(fit) == "mtc.nodesplit") {
		fit <- fit$consistency
	}
	
	gemtc::relative.effect.table(fit) %>%
		tibble::as_tibble(rownames = "comparatorId") %>%
		tidyr::gather(targetId, estimate, -comparatorId) %>%
		dplyr::filter(comparatorId != targetId) %>%
		dplyr::mutate_at("estimate", stringr::str_remove_all, pattern = "[(),]") %>%
		tidyr::separate(estimate, into = c("hr", "cri95Lb", "cri95Ub"), sep = " ", remove = TRUE) %>%
		dplyr::mutate_at(dplyr::vars(hr, cri95Lb, cri95Ub), function(.) exp(as.numeric(.))) %>%
		dplyr::select(targetId, comparatorId, dplyr::everything())
}
	