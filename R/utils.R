#' Create gemtc network object
#' 
#' Internal function. Needed in several places, doesn't need to be exposed to the user
#' 
#' @inheritParams fitNetwork
#' 
#' @noRd

createGemtcNetwork <- function(networkData) {
	dplyr::rename(networkData, study = studyId) %>%
		as.data.frame() %>%
		gemtc::mtc.network()
}

# ===


#' Sort concatenated targetId and comparatorId to yield unique id for each target-comparator comparison pair
#'
#' Internal function.
#'
#' @param x vector of concatenated targetId's and compartorId's, on the form targetId\<sep\>comparatorId.
#' @param sep separator between targetId and comparatorId (default: \code{"-"})
#' 
#' @return A character vector with sorted elements and the same format as the input
#' 
#' @noRd

sortCohortIds <- function(x, sep  = "-") {
	stringr::str_split(x, sep) %>%
		lapply(sort) %>%
		vapply(paste, collapse = sep, FUN.VALUE = "x-y")
}

# ===

#' Compute angle of node to align label
#' 
#' @param x,y coordinates of the node(s)
#' 
#' @noRd
 
computeAngle <- function(x, y) {
	angles <-  atan2(y, x) * 180 / pi
	dplyr::if_else(x >= 0, angles, angles - 180)
}

# ===

#' Offset labels in network plot
#' 
#' To avoid overlapping node labels and points, this functions offsets the labels away from the center of the network, based on their angle. 
#' 
#' @param offsetArea a numeric value of the area of the point (the offset will need to use the derived radius)
#' @param axis string giving the axis on which the function is doing the offsetting.
#' @param x,y vectors of x and y coordinates.
#' 
#' @noRd

offsetLabels <- function(nodeRadius, axis, x, y) {
	if (!axis %in% c("x", "y")) {
		stop("Can't define convertFun and trigoFun in offsetLabels. Please, contact package maintainer.")
	}
	trigoFun <- switch(axis, "x" = cospi, "y" = sinpi) # trigonometric function
	convertFun <- switch(axis, "x" = grid::convertX, "y" = grid::convertY)
	startValues <- get(axis)
	
	startValues + sign(x) * trigoFun(computeAngle(x, y) / 180) * nodeRadius
}

# ===

#' Replace NULL by value in a vector
#' 
#' Snatched from tidyverse.
#' 
#' @param a vector of values that should be given a value (\code{b}) if NULL.
#' @param b the replacement value, either scale of same length as \code{a}.
#' 
#' @noRd

`%||%` <- function(a, b) {
	if (!is.null(a)) a else b
}

# ===

#' Make first letter upper case, "_" => " ", and "__" => ", "
#' 
#' @param x vector of strings to beautify.
#' 
#' @return Modified version of \code{x} with modifications.
#' 
#' @noRd

beautifyStrings <- function(x) {
	stringr::str_replace_all(stringr::str_to_sentence(x), pattern = c("__" = ", ", "_" = " "))
}

# ===

#' Derive model name from a fit object
#' 
#' Used to create appropriate model names.
#' 
#' @param fit
#' 
#' @noRd

deriveModelName <- function(fit) {
	modelName <- fit$model$type
	if (modelName == "nodesplit") {
		modelName <- paste0(modelName, "_", paste(fit$model$split, collapse = "_"))
	}
	return(modelName)
}
