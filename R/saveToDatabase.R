#' Saves the reconciled results to database
#'
#' If desired, this function takes the reconciled results and saves them to the database for
#' downstream use.
#'
#' @param conn connection object.
#' @param reconciledResults output of \code{reconcileResults}.
#' @param schema string, the name of the schema where the tables will live. Should probably be a
#'   dedicated schema to prevent conflicting names.
#' @param overwriteExistingTables boolean, should tables be overridden? This is a bit dangerous and
#'   so set to \code{FALSE} by default. See note on dedicated schema above.
#'
#' @export

saveToDatabase <- function(conn, reconciledResults, schema, overwriteExistingTables = FALSE) {
	for (t in names(reconciledResults)) {
		# FIX: Add indices after creating the tables?
		
		DatabaseConnector::dbWriteTable(
			conn = conn, 
			name = sprintf("%s.%s", schema, SqlRender::camelCaseToSnakeCase(t)),
			value = as.data.frame(dplyr::rename_all(reconciledResults[[t]], 
													SqlRender::camelCaseToSnakeCase)),
			overwrite = overwriteExistingTables)
	}
}
