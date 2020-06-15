#' Produces density plots of parameter estimates from the fitted models
#'
#' Produces a \pkg{ggplot2} object so can be further customised as desired. If the results of a
#' node-splitting analysis is supplied, the output is a list of plots, one for each analysis and one
#' for the consistency fit.
#'
#' @param modelFit an object containing the fitted results.
#' @param densityResolution scalar, the number of grid points over which to estimate the densities.
#'   Higher values yield smoother curves but produces a larger object.
#' @inheritParams plotTraces
#'
#' @return A ggplot object, or a list of ggplot objects.
#'
#' @export

plotPosteriorDensities <- function(posteriorDraws, separateChains = TRUE, lineAlpha = 0.7, 
								   lineSize = 0.2, densityResolution = 256, wrapByModelName = FALSE) {
	aesthetics <- ggplot2::aes(x = parameterValue)
	if (isTRUE(separateChains)) {
		posteriorDraws$chainId <- as.character(posteriorDraws$chainId) 
			# colour-coding should use chainId as a categorial variable, not numeric
		aesthetics$colour <- as.symbol("chainId") # slight hack for turning on/off colour-coding
	}
	
	if (isTRUE(wrapByModelName)) {
		wrap <- ggplot2::facet_wrap(~ modelName + parameterName, scales = "free")
	} else {
		wrap <- ggplot2::facet_wrap(~ parameterName, scales = "free") 
	}
	
	ggplot2::ggplot(posteriorDraws) +
		ggplot2::stat_density(aesthetics, geom = "line", size = lineSize, alpha = lineAlpha, 
							  position = "identity", n = densityResolution) +
		ggplot2::labs(x = "Parameter value", y = "Density") +
		wrap +
		packageTheme
}
