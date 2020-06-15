#' Load target-comparator-outcome estimates from server
#'
#' Load TCO estimates from the server and wrangle them into a format appropriate for network
#' visualisation and analysis.
#'
#' @param conn a connection to a database, e.g., the result of calling
#'   \code{DatabaseConnector::connect}.
#' @param resultsCdm string, the name of the schema in which the table lives.
#' @param resultsTable string, the name of the table holding the estimates.
#' @param excludedDatabases character vector, names of the databases whose results should be
#'   ignored. The default (\code{"Meta-analysis"}) is obvious and should be kept if other databases
#'   are added.
#'
#' @export

fetchTcoEstimates <- function(conn, resultsCdm, resultsTable, excludedDatabases = "Meta-analysis") {
	# FIX: Maybe some of this wrangling should happen database-side
	
	DatabaseConnector::renderTranslateQuerySql(conn,
											   "SELECT * 
											   FROM @cdm_results.@results_table
											   WHERE database_id NOT IN (@excludedDatabases)",
											   cdm_results = resultsCdm,
											   results_table = resultsTable,
											   excludedDatabases = sprintf("'%s'", excludedDatabases)) %>%
												
		dplyr::rename_all(SqlRender::snakeCaseToCamelCase) %>%
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