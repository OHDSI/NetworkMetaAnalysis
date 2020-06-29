#' Load target-comparator-outcome estimates from server
#'
#' Load TCO estimates from the server. They need some processing (\code{wrangleTcoEstimates}) before
#' they are ready.
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
		dplyr::mutate_if(is.factor, as.character) # avoid unexpected behaviour
}