#' Creates a heatmap plot of all pairwise estimates
#'
#' Some customisation possible, but this is still pretty basic.
#'
#' @param tidyEstimates data frame/tibble with all pairwise combinations to feature in the heatmap.
#' @param nDigits scalar, how many digits should the shown estimates have? Default is \code{1}
#' @param colour_low,colour_high valid colour specifications for the lower and higher bounds of the
#'   diverging colour scale
#' @param contrastThreshold scalar, absolute values on the log-scale above this threshold will be
#'   white, otherwise they will be black
#' @param mid,lo,hi strings, the names of the columns in the \code{tidyEstimates} data frame holding
#'   the point estimate (usually median) and lower and upper bounds of the credibility intervals
#'   (usually 2.5 and 97.5 percentiles)
#' @param sucraEstimates data frame with two columns: exposureId and SUCRA estimate as numeric
#'   value. Output from \code{computeSucra}. Default: \code{NULL}. If valid data supplied, the SUCRA
#'   estimates will be shown in the diagonal, and the exposures will be ordered by descending SUCRA
#'   estimate.
#' @param textSize scalar.
#'
#' @importFrom rlang sym !!
#'
#' @export

plotPairwiseEstimates <- function(tidyEstimates, nDigits = 1, colour_low = "blue", colour_high = "red", 
								  contrastThreshold = 0.5, mid = "hr", lo = "cri95Lb", hi = "cri95Ub", 
								  textSize = 12, sucraEstimates = NULL) {
	transformGuideLabels <- function(x) {
		round(exp(x), nDigits)
	}
	
	d <- dplyr::mutate(tidyEstimates,
					   median_int = sprintf(stringr::str_replace_all("%.xf\n(%.xf-%.xf)", c("x" = paste(nDigits))),
					   					    !!sym(mid), !!sym(lo), !!sym(hi))) 
	
	# Order by SUCRA estimates if supplied
	if (!is.null(sucraEstimates)) {
		sucraFactorLevels <- dplyr::arrange(sucraEstimates, dplyr::desc(sucra))$exposureId
		d <- dplyr::mutate_at(d, dplyr::vars(targetId, comparatorId), factor, levels = sucraFactorLevels)
	}
	
	blank <- ggplot2::element_blank()
	p <- ggplot2::ggplot(d, ggplot2::aes(x = targetId, y = comparatorId)) +
		ggplot2::geom_raster(ggplot2::aes(fill = log(!!sym(mid))), show.legend = FALSE) +
		ggplot2::scale_fill_gradient2(low = colour_low, high = colour_high, labels = transformGuideLabels) +
		# Different text colours to ensure adequate contrast
		ggplot2::geom_text(ggplot2::aes(label = median_int), dplyr::filter(d, abs(log(hr)) > contrastThreshold), 
						   colour = "white") +
		ggplot2::geom_text(ggplot2::aes(label = median_int), dplyr::filter(d, abs(log(hr)) <= contrastThreshold)) +
		ggplot2::scale_x_discrete(position = "top") +
		ggplot2::scale_y_discrete(limits = rev) +
		ggplot2::theme(panel.background = blank, panel.grid = blank, axis.ticks = blank)
	
	# Add SUCRA labels if supplied
	if (!is.null(sucraEstimates)) {
		sucraLabels <- dplyr::mutate(sucraEstimates,
									 targetId = factor(exposureId, levels = sucraFactorLevels),
									 comparatorId = targetId,
									 label = scales::percent(sucra))
		p <- p + 
			ggplot2::geom_text(ggplot2::aes(label = label), sucraLabels)
	}
	
	return(p)
}
