#' Loads results stored in a database following its data model
#' 
#' @param conn connection to database, e.g. result of calling \code{DatabaseConnector::connect}
#' @param schema string, the name of the schema where results tables live. 
#' 
#' @export

loadFromDb <- function(conn, schema) {
	fetchTable <- function(tableName, conn, schema) {
		tableName <- SqlRender::camelCaseToSnakeCase(tableName)
		tableExists <- DatabaseConnector::dbExistsTable(conn, name = tableName, schema = schema)
		
		# Must check because node-splitting results might not be available
		if (isTRUE(tableExists)) {
			tablePath <- sprintf("%s.%s", resultsCdm, tableName)
			DatabaseConnector::dbReadTable(conn, name = tablePath, row.names = FALSE) %>%
				dplyr::mutate_if(is.factor, as.character) %>%
				dplyr::rename_all(SqlRender::snakeCaseToCamelCase) %>%
				tibble::as_tibble()
		} 
	}
	
	tablesToLoad <- c("networkMetaAnalysisDetails", "graphNodes", "graphEdges", "pairwiseEstimates",
					  "relativeEffects", "rankProbabilities", "posteriorSamples", 
					  "potentialScaleReductionFactor", "nodesplitResults", "networkMetaAnalysisData",
					  "modelParameterSummaries", "devianceStatistics")
	
	sapply(tablesToLoad, fetchTable, conn = conn, schema = schema, simplify = FALSE) %>%
		structure(class = "ReconciledOhdsiNetworkMetaAnalysis")
}