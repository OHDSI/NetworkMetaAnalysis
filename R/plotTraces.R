#' Produces density plots of parameter estimates from the fitted models
#'
#' Produces a \pkg{ggplot2} object so can be further customised as desired. If the results of a
#' node-splitting analysis is supplied, the output is a list of plots, one for each analysis and one
#' for the consistency fit.
#'
#' @param posteriorDraws tidy data frame, all posterior draws with (at least) the following columns:
#'   \code{modelName}, \code{chainId}, \code{mcmcIterationId}, \code{parameterName},
#'   \code{parameterValue}.
#' @param separateChains boolean, should chains be plotted as separate distributions (\code{TRUE},
#'   default) or as one (\code{FALSE})? Not plotting chains separately somewhat defeats the purpose
#'   of the visualisation but is anyway left to the user.
#' @param lineAlpha the opacity of drawn lines. If \code{alpha = 1}, lines will be entirely opague
#'   masking overlain lines, so the default is \code{0.7}.
#' @param lineSize scalar, thickness of the plot lines.
#' @param wrapByModelName boolean, if the \code{modelName} column contains more than one distinct
#'   value, this should be set to \code{TRUE} to show separate plots for each model. Otherwise, the
#'   plot will combine samples for the same parameter from different models, which will give
#'   misleading results.
#'
#' @return A ggplot object, or a list of ggplot objects.
#'
#' @export

plotTraces <- function(posteriorDraws, separateChains = TRUE, lineAlpha = 0.7, lineSize = 0.2, wrapByModelName = FALSE) {
	aesthetics <- ggplot2::aes(x = mcmcIterationId, y = parameterValue)
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
		ggplot2::geom_line(aesthetics, alpha = lineAlpha, size = lineSize) + 
		ggplot2::labs(x = "Iteration", y = "Parameter value") +
		wrap +
		ggplot2::scale_x_continuous(labels = scales::number_format(big.mark = ",")) +
		packageTheme
}
