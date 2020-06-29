#' Putting original data into appropriate format
#'
#' Whether data were loaded from a database o a file, they need some wrangling to play well with the
#' rest of the workflow in the package. This function does just that.
#'
#' @param tcoEstimates data frame/tibble, in the original LEGEND format.
#' 
#' @export

wrangleTcoEstimates <- function(tcoEstimates) {
	dplyr::rename_all(tcoEstimates, SqlRender::snakeCaseToCamelCase) %>%
		dplyr::transmute(analysisId, 
						 databaseId, 
						 outcomeId,
						 networkMetaAnalysisId = as.numeric(factor(paste(outcomeId, analysisId))),
						 studyId = paste(outcomeId, analysisId, databaseId, targetId, comparatorId, sep = "-"),
						 targetTreatment = targetId, # change of suffix to play well with gemtc
						 comparatorTreatment = comparatorId, # idem
						 targetResponders = targetOutcomes, # idem
						 comparatorResponders = comparatorOutcomes, # idem
						 targetExposure = targetDays / 365.25, # idem, exposure in approx. years
						 comparatorExposure = comparatorDays / 365.25) %>% # idem
		tidyr::pivot_longer(matches("^(target|comparator)")) %>%
		dplyr::mutate(cohortType = dplyr::case_when(grepl("^target", name) ~ "target",
													grepl("^comparator", name) ~ "comparator",
													TRUE ~ NA_character_),
					  name = tolower(stringr::str_remove_all(name, "^(target|comparator)"))) %>%
		tidyr::pivot_wider(names_from = name, values_from = value)
}