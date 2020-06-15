# Used in Shiny app
.nmaEnv <- new.env(parent = emptyenv())

# For harmonised plotting appearnace
packageTheme <- ggplot2::theme_minimal() +
	ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey97", size = 0),
				   panel.grid = ggplot2::element_line(colour = "white"),
				   strip.background = ggplot2::element_rect(fill = "grey95", size = 0))

.onLoad <- function(libname = find.package("NetworkMetaAnalysis"), pkgname = "NetworkMetaAnalysis") {
	# Needed to satisfy CRAN criteria on undefined global variables
	if (getRversion() >= "2.15.1") {
		
	# To format the list, use the following (x = the sometimes long list given by R CMD check: 
	# stringr::str_squish(x) %>%
	# 	stringr::str_split(" ") %>% 
	# 		unlist() %>% 
	#		unique() %>%
	# 		{ paste0("\"", ., "\"", collapse = ", ") } %>% 
	#		stringr::str_wrap() %>%
	# 		cat()
		
		utils::globalVariables(c(
			"Chain", "HR", "Iter", "LB", "P", "UB", "aes", "analysisId", "averageIncidence",
			"ci.l", "ci.u", "+", "cohortType", "cohortsCompared", "comparator",
			"comparatorDays", "comparatorId", "comparatorOutcomes", "comparatorTreatment",
			"cumProb", "databaseId", "edges", "effect", "estimate", "exposure",
			"exposureId", "matches", "median", "mtc.comparisons", "nStudies", "name",
			"non.edges", "outcomeId", "p", "p2.5", "p25", "p50", "p75", "p97.5",
			"packageTheme", "parameter", "pe", "radius", "responders", "studyId",
			"summaries", "t1", "t2", "target", "targetDays", "targetId", "targetOutcomes",
			"targetTreatment", "treatment", "treatmentComparator", "treatmentName",
			"treatmentNameComparator", "treatmentNameTarget", "treatmentTarget", "value",
			"vars", "x", "y", "."
		)) # see: https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
	}
}