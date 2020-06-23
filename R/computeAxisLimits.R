#' Expand plot area to include node labels
#' 
#' Internal utility function, exported to transparency reasons. 
#' 
#' @param nodeNames string vector of node names from which to pick the one with most characeters. This is much faster than using grid::unit.pmax and sufficiently accurate to work just fine.
#' @param axis \code{"x"} (default) or \code{"y"}
#' @param relExpand a factor which is multiplied to the axis limit. The default (\code{1.05}) add 5% in each end. 
#' 
#' @importFrom grid unit convertX convertY
#' 
#' @noRd

computeAxisLimits <- function(nodeNames, axis, maxNodeRadius, relExpand = 1.05) {
	# FIX: Project on the y axis, to account for situations without vertical labels
	
	longestNodeName <- nodeNames[which.max(nchar(nodeNames))[1]] # index in case of ties
	convertFun <- switch(axis, "x" = convertX, "y" = convertY)
	if (!axis %in% c("x", "y")) {
		stop("Can't define convertFun in computeAxisLimits. Please, contact package maintainer.")
	}
	
	c(convertFun(unit(-relExpand - maxNodeRadius, "npc") - unit(1, "strwidth", longestNodeName), "npc"),
	  convertFun(unit(relExpand + maxNodeRadius, "npc") + unit(1, "strwidth", longestNodeName), "npc"))
}