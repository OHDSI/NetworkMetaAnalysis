#' Table of parameter estimates and deviance statistics for reconciled results
#'
#' @param parameterSummaries output of \code{deriveParameterQuantiles}.
#' @param devianceStatistics output of \code{fetchDevianceStatistics}.
#' @param networkMetaAnalysisId scalar, what analysis to use. If \code{NULL} (default), the function
#'   assumes pre-filtered data to be supplied in the first two arguments.
#' @param nDigits scalar, how many digits to show for parameter estimates. Won't affect deviance
#'   statistics.
#'   
#' @export

fetchParameterSummaries <- function(parameterSummaries, devianceStatistics, networkMetaAnalysisId = NULL, 
									nDigits = 1) {
	for (x in c("parameterSummaries", "devianceStatistics")) {
		if (!is.null(networkMetaAnalysisId)) {
			assign(x, dplyr::filter(get(x), networkMetaAnalysisId == !!networkMetaAnalysisId))
		}
		
		# Safety measure: keeping the nma ID breaks code below (shiny app removes this before invoking function)
		if (exists("networkMetaAnalysisId", get(x))) {
			assign(x, dplyr::select(get(x), -"networkMetaAnalysisId"))
		}
	}
	
	devianceLookup <- c("devianceInformationCriterion" = "DIC",
						"meanSumOfResidualDeviance" = "Dbar",
						"sumOfLeverage" = "pD")
	
	devianceStats <- devianceStatistics %>%
		tidyr::pivot_longer(-modelName, names_to = "parameterName", values_to = "estimate") %>%
		dplyr::mutate(parameterName = devianceLookup[parameterName],
					  estimate = sprintf("%.2f", estimate))
	
	parameterSummaryFormat <- stringr::str_replace_all("%.xf (%.xf, %.xf)", "x", paste(nDigits))
	dplyr::transmute(parameterSummaries, 
					 modelName, 
					 parameterName, 
					 estimate = sprintf(parameterSummaryFormat, median, cri95Lb, cri95Ub)) %>%
		dplyr::bind_rows(devianceStats) %>%
		tidyr::pivot_wider(names_from = modelName, values_from = estimate) %>%
		# i column used to yield appropriate order of parameters
		dplyr::mutate(i = dplyr::case_when(parameterName == "sd.d" ~ 10,
										   grepl("d\\.(direct|indirect)", parameterName) ~ 5,
										   grepl("d\\.\\d+\\.\\d+", parameterName) ~ 0,
										   TRUE ~ 15)) %>%
		dplyr::arrange(i, parameterName) %>%
		dplyr::select(parameterName, consistency, dplyr::everything(), -i)
}
