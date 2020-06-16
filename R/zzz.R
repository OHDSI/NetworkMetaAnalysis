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
	# 	unique() %>%
	# 		{ paste0("\"", ., "\"", collapse = ", ") } %>%
	# 	stringr::str_wrap() %>%
	# 		cat()
		
		utils::globalVariables(c(
			".", ".RESULTS", "Hr", "Lb", "Ub", "analysisId", "averageIncidence",
			"chain", "ci.l", "ci.u", "cohortType", "cohortsCompared", "comparatorDays",
			"comparatorId", "comparatorOutcomes", "consistency", "cri95Lb",
			"cri95Ub", "cumulativeProbability", "databaseId", "effect", "estimate",
			"exposure", "exposureId", "hr", "i", "iter", "label", "labelComparator",
			"labelOrientation", "labelTarget", "matches", "mcmcIterationId", "median",
			"median_int", "modelLabel", "modelLabelNum", "modelName", "nStudies", "name",
			"networkMetaAnalysisId", "nmaId", "outcomeId", "p", "p2.5", "p25", "p50",
			"p75", "p97.5", "pValue", "parameter", "parameterName", "parameterValue", "pe",
			"probability", "psrf", "radius", "ref", "responders", "resultsCdm", "rhatValue",
			"splitExposureId1", "splitExposureId2", "studyId", "sucra", "summaries",
			"t1", "t2", "target", "targetDays", "targetId", "targetOutcomes", "treatment",
			"treatmentComparator", "treatmentTarget", "value", "x", "y", "ymax", "ymin"
		)) # see: https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
	}
}