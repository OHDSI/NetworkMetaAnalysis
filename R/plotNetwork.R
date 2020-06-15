#' Visualise network
#'
#' Draw the head-to-head comparisons as a network. The function relies on \pkg{ggraph} to do the
#' heavy lifting.
#'
#' @param treatmentMetaData NOT IN USE a \code{dataframe} with information about each node in the
#'   network, e.g., name and databases with data for this treatment.
#' @param edgeMetaData NOT IN USE a \code{dataframe} with information about the head-to-head
#'   comparisons. Not that the number of head-to-head comparisons is computed so doesn't need to be
#'   supplied.
#' @param maxNodeRadius the radius of the largest node in the network on the original
#'   coordinate scales (= \[-1, \[1). Default = 0.1.
#' @param labelOrientation string giving how to rotate the label. There are two allowed settings:
#'   \code{"radial"} (default) and \code{"horisontal"}/\code{"horizontal"}.
#' @param edgeColour,nodeColour,labelColour strings given the colours of edges, nodes and label
#'   text.
#' @param edgeAlpha scalar in \[0, 1\] giving the opacity of the edges (default: 0.2). 1 = complete
#'   opague, 0 = completely transparent.
#' @inheritParams fitNetwork
#'
#' @importFrom dplyr select left_join rename_all mutate group_by distinct n_distinct everything
#'   summarise transmute
#' @importFrom magrittr %>%
#' 
#' @export

## FIX: Make plotly object with more node and edge metadata available when hovering?

plotNetwork <- function(edges, nodes, maxNodeRadius = 0.1, nodeColour = "dodgerblue",
						labelOrientation = "radial", labelColour = "black",
						edgeColour = "dodgerblue", edgeAlpha = 0.2) {
	
	# FIX: maxNodeRadius based on number of nodes
	# FIX: Expansion of plot area must be handled better
	# FIX: Use labelOrientation argument (for now, only radial uses)
	
	igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes) %>%
		ggraph::ggraph(layout = "linear", circular = TRUE, sort.by = name) +
		ggraph::geom_edge_arc2(ggplot2::aes(edge_width = nStudies), colour = edgeColour, alpha = 0.2) +
		ggraph::geom_node_circle(ggplot2::aes(r = radius), size = 0, fill = nodeColour) +
		ggraph::geom_node_text(ggplot2::aes(label = name, hjust = x < 0,
											angle = computeAngle(x, y),
											x = offsetLabels(radius, "x", x, y),
											y = offsetLabels(radius, "y", x, y)),
							   colour = labelColour) +
		ggraph::scale_edge_width_continuous(range = range(0, 3), limits = c(0, max(edges$nStudies))) +
		ggplot2::coord_fixed(xlim = computeAxisLimits(nodes$label, "x", maxNodeRadius = maxNodeRadius),
							 ylim = computeAxisLimits(nodes$label, "y", maxNodeRadius = maxNodeRadius)) +
		ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA, colour = "grey50"))
}
