#' Deriving node data for drawing the network
#' 
#' @param networkData tidy data frame, subset of the full data frame with aggregate results at study arm level for specific network meta-analysis.
#' @inheritParams plotNetwork
#' 
#' @return A tidy data frame.
#' 
#' @export

deriveNodes <- function(networkData, labelOrientation) {
	# FIX: node names should come from CDM.[single]/[combi]_exposure_of_interest.exposure_name
	# FIX: actually use the labelOrientation argument + ensure geom_text correct hjust/vjust
	
	dplyr::select(networkData, treatment, responders, exposure) %>%
		dplyr::group_by(treatment) %>%
		dplyr::summarise(averageIncidence = mean(responders / exposure)) %>%
		dplyr::transmute(label = paste("  Exposure", treatment, " "),
						 averageIncidence,
						 radius = sqrt(averageIncidence / pi), 
						 exposureId = treatment) # for joining into edges
}