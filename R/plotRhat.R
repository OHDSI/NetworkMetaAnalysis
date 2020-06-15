#' Plot the Rhat values of the fitted model
#'
#' Instead of looking at the Rhat values in table format, it's usually easier to quickly scan a
#' plot, and this function allows you to do just that.
#'
#' @param rhat output from \code{computeRhat}
#' @param threshold scalar, what is the threshold below which you want all Rhat values to lie?
#'   Default is \code{1.05}. Set to \code{NULL} to remove the indicator in the plot.
#' @param showParameters boolean, should parameters be distinguishable with colours? Potentially
#'   useful if any are above the threshold, so by default set to \code{FALSE}.
#' @param pointSize scalar, size of points representing Rhat values.
#' @param lineSize scalar, thickness of vertical indicator of threshold, if shown.
#'
#' @return A \code{ggplot} object.
#'
#' @export

plotRhat <- function(rhat, threshold = 1.05, showParameters = FALSE, pointSize = 1, lineSize = 0.5) {
	makePrettyLabels <- function(labels) {
		helperFun <- function(label) {
			temp <- as.character(stringr::str_split(label, "_", simplify = TRUE))
			if (temp[1] == "nodesplit") {
				label <- sprintf("%s, %s vs. %s", temp[1], temp[2], temp[3])
			} 
			return(stringr::str_to_sentence(label))
		}
		
		sapply(labels, helperFun, USE.NAMES = FALSE)
	}
	
	d <- dplyr::mutate(rhat, 
					   modelLabel = makePrettyLabels(modelName),
					   modelLabelNum = as.numeric(factor(modelLabel))) # needed for pretty axis

	# Pre-compute data for the y axis and the horisontal areas	
	axisLabels <- dplyr::distinct(d, modelLabel, modelLabelNum)
	ribbonCoordinates <- dplyr::filter(axisLabels, modelLabelNum %% 2 == 0) %>%
		dplyr::mutate_at("modelLabelNum", list(ymin = ~ . - 0.5, ymax = ~ . + 0.5))
	
	aesthetics <- ggplot2::aes(y = modelLabelNum, x = rhatValue)
	if (isTRUE(showParameters)) {
		aesthetics$colour <- as.symbol("parameterName")
	}
	
	# Build plot, start with empty plot
	p <- ggplot2::ggplot()
	
	# One-model plots shouldn't have the ribbons
	if (nrow(ribbonCoordinates) > 0) {
		p <- p + ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), 
									ribbonCoordinates, fill = "black", alpha = 0.05)
	}
	
	blank <- ggplot2::element_blank() # helper for shorter code below
	p <- p +
		ggplot2::geom_jitter(aesthetics, d, size = pointSize, height = 0.3) +
		ggplot2::scale_y_reverse(breaks = axisLabels$modelLabelNum, labels = axisLabels$modelLabel,
								 limits = rev(range(d$modelLabelNum) + c(-0.5, 0.5))) +
		ggplot2::labs(x = "Potential scale reduction factor (Rhat)") +
		packageTheme +
		ggplot2::theme(panel.grid.major.y = blank, axis.title.y = blank)
	
	if (!is.null(threshold)) {
		p <- p + ggplot2::geom_vline(xintercept = threshold, linetype = 2, size = lineSize)
	}
	
	return(p)
}
