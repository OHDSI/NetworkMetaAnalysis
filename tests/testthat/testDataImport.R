context("Data import")
library(NetworkMetaAnalysis)

# Local instance of LEGEND sample for testing
cdm_results <- "ohdsi_network_meta_analysis"
results_table <- "legend_sample"
dbms <- "postgresql"
connectionString <- "jdbc:postgresql://localhost:5432/masterbasse"
user <- "masterbasse"
password <- ""

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms, user = user, password = password,
																connectionString = connectionString)
conn <- suppressMessages(DatabaseConnector::connect(connectionDetails))

# FIX: Use the OHDSI Jenkins server to testing instead (https://ohdsi.github.io/MethodsLibrary/developerGuidelines.html#when_unit_tests_are_performed)

test_that("must load a valid data frame", {
	expect_is(fetchTcoEstimates(conn, "ohdsi_network_meta_analysis", "legend_sample"), "data.frame")
	expect_named(fetchTcoEstimates(conn, "ohdsi_network_meta_analysis", "legend_sample"),
				 c("analysisId", "databaseId", "outcomeId", "studyId", "cohortType", "treatment", "responders", "exposure"))
})