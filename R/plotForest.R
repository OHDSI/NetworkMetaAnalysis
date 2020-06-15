#' Draw forest plot of total network estimates
#'
#' Using a \code{fitNetwork} object, this functions draws a classic forest plot of the combined
#' (direct + indirect) effects estimated.
#'
#' @param relativeEffectsEstimates the output of \code{computeRelativeEffects}.
#' @param reference string, which exposure to use as the reference. Choice is arbitrary. If nothing
#'   is supplied, the first value in the data frame is used.
#' @param lineSize,pointSize scalars
#'
#' @export

plotForest <- function(relativeEffectsEstimates, reference = NULL, lineSize = 0.5, pointSize = 1) {
	# FIX: Join in the treatment names
	
	if (is.null(reference)) {
		reference <- relativeEffectsEstimates$comparatorId[[1]]
	}
	
	# Allow for handling multiple comparisons (e.g. when plotting all analyses together)
	aes_linerange <- ggplot2::aes(xmin = cri95Lb, xmax = cri95Ub)
	aes_points <- ggplot2::aes(x = hr)
	if (exists("analysisId", relativeEffectsEstimates)) {
		relativeEffectsEstimates <- dplyr::mutate_at(relativeEffectsEstimates, "analysisId", paste)
		aes_linerange$colour <- aes_points$colour <- as.symbol("analysisId")
		position <- ggplot2::position_dodge2(width = 0.7)
	} else {
		position <- ggplot2::position_identity()
	}
	
	ggplot2::ggplot(relativeEffectsEstimates, ggplot2::aes(y = targetId)) +
		ggplot2::geom_vline(xintercept = 1, size = lineSize, colour = "grey80", linetype = 2) +
		ggplot2::geom_linerange(aes_linerange, alpha = 0.5, size = lineSize, position = position) +
		ggplot2::geom_point(aes_points, size = pointSize, position = position) +
		ggplot2::scale_x_log10() +
		ggplot2::theme_minimal() +
		ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
		ggplot2::labs(x = paste("Hazard ratio compared to treatment", reference),
					  y = "Target treatment") +
		packageTheme
}
