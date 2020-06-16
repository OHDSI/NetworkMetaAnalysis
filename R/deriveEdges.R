#' Used to prepare data needed to draw the edges of the network
#' 
#' @param networkData subset of the original network study data.
#' @param nodes output from \code{deriveNodes}
#' 
#' @return A ggplot object.
#' 
#' @noRd

deriveEdges <- function(networkData, nodes) {
	dplyr::select(networkData, studyId, cohortType, treatment, databaseId) %>%
		dplyr::left_join(dplyr::select(nodes, exposureId, label), 
						 by = c("treatment" = "exposureId")) %>% 
		tidyr::pivot_wider(names_from = cohortType, values_from = c(treatment, label)) %>%
		dplyr::rename_all(stringr::str_replace_all, pattern = c("_targ" = "Targ", "_comp" = "Comp")) %>%
		dplyr::mutate(cohortsCompared = sortCohortIds(paste(treatmentTarget, treatmentComparator, sep = "-"))) %>%
		dplyr::group_by(cohortsCompared) %>%
		dplyr::left_join(summarise(., nStudies = dplyr::n_distinct(databaseId)), by = "cohortsCompared") %>%
		dplyr::distinct(cohortsCompared, .keep_all = TRUE) %>%
		dplyr::select(labelTarget, labelComparator, -cohortsCompared, dplyr::everything())
}
