#' Load target-comparator-outcome estimates from server
#'
#' Load TCO estimates from the server. They need some processing (\code{wrangleTcoEstimates}) before
#' they are ready.
#'
#' @param conn a connection to a database, e.g., the result of calling
#'   \code{DatabaseConnector::connect}.
#' @param resultsCdm string, the name of the schema in which the table lives.
#' @param resultsTable string, the name of the table holding the estimates.
#'
#' @export

fetchTcoEstimates <- function(conn, resultsCdm, resultsTable) {
	# FIX: Maybe some of this wrangling should happen database-side
	
	DatabaseConnector::renderTranslateQuerySql(conn,
											   "SELECT * FROM @cdm_results.@results_table",
											   cdm_results = resultsCdm,
											   results_table = resultsTable) %>%
		dplyr::mutate_if(is.factor, as.character) # avoid unexpected behaviour
}