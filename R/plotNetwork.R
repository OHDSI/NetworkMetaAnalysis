#' Visualise network
#'
#' Draw the head-to-head comparisons as a network. The function relies on \pkg{ggraph} to do the
#' heavy lifting.
#'
#' @param edges,nodes data frames with data on edges and nodes in the study graph.
#' @param maxNodeRadius the radius of the largest node in the network on the original coordinate
#'   scales (= \[-1, \[1). Default = 0.1.
#' @param labelOrientation string giving how to rotate the label. There are two allowed settings:
#'   \code{"radial"} (default) and \code{"horisontal"}/\code{"horizontal"}.
#' @param labelSize scalar, size of the text
#' @param edgeColour,nodeColour,labelColour strings given the colours of edges, nodes and label
#'   text.
#' @param maxEdgeSize,maxNodeSize scalars, the size of the largest node and thickness of widest edge
#'   in the graph. Set to reasonable defaults but this will likely need adjustment in a number of
#'   situations.
#' @param edgeAlpha scalar in \[0, 1\] giving the opacity of the edges (default: 0.2). 1 = complete
#'   opaque, 0 = completely transparent.
#' @param relExpandX,relExpandY scalars, how much to expand the plot area beyond the node labels.
#'   Because text in \code{ggplot2} doesn't have a well-defined width, it's tricky to set perfect
#'   defaults. The default (\code{1.05}) for these work fine, but you might need to tweak them to
#'   get a perfect publication-ready plot.
#' @param treatmentMetaData NOT IN USE a \code{dataframe} with information about each node in the
#'   network, e.g., name and databases with data for this treatment.
#' @param edgeMetaData NOT IN USE a \code{dataframe} with information about the head-to-head
#'   comparisons. Note that the number of head-to-head comparisons is computed so doesn't need to be
#'   supplied.
#'
#' @importFrom dplyr select left_join rename_all mutate group_by distinct n_distinct everything
#'   summarise transmute
#' @importFrom magrittr %>%
#'
#' @export

## FIX: Make plotly object with more node and edge metadata available when hovering?

plotNetwork <- function(edges, nodes, nodeColour = "dodgerblue",
						labelOrientation = "radial", labelColour = "black", labelSize = 10,
						edgeColour = "dodgerblue", edgeAlpha = 0.2, treatmentMetaData = NULL,
						maxEdgeSize = 3, maxNodeSize = 10,
						relExpandX = 1.05, relExpandY = 1.05,
						edgeMetaData = NULL) {
	
	# FIX: maxNodeRadius based on number of nodes
	# FIX: Expansion of plot area must be handled better -- THAT REQUIRES A NEW GEOM TO textGrob sizes are evaluated within plot area only
	# FIX: Use labelOrientation argument (for now, only radial uses)
	# FIX: Don't enforce coord_fixed but use relative height (to width) and use this to adjust angle of labels with orientation = "radial"
	
	# WHAT SHOULD PROBABLY BE DONE:
	#- Create new geom based on geom_text
	#- Find bounding box of all labels (akin to https://github.com/slowkow/ggrepel/blob/master/R/geom-text-repel.R#L353)
	#- Add transparent rectangles behind all labels (grid::polygonGrob)
	#- Add labels (grid::textGrob)
	
	findMaxLabelWidth <- function(labels) {
		lapply(labels, grid::textGrob, gp = grid::gpar(fontsize = labelSize * ggplot2::.pt, family = "Arial")) %>%
			sapply(function(.) grid::convertX(grid::grobX(., "east") - grid::grobX(., "west"), "native", TRUE)) %>%
			max() 
	}
	
	igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes) %>%
		ggraph::ggraph(layout = "linear", circular = TRUE, sort.by = name) +
		ggraph::geom_edge_arc2(ggplot2::aes(edge_width = nStudies), colour = edgeColour, alpha = 0.2) +
		ggraph::geom_node_point(ggplot2::aes(size = averageIncidence), colour = nodeColour) +
		ggraph::geom_node_text(ggplot2::aes(label = name, hjust = x < 0,
											angle = computeAngle(x, y),
											x = offsetLabels(radius, "x", x, y),
											y = offsetLabels(radius, "y", x, y)),
							   colour = labelColour, size = labelSize / ggplot2::.pt, family = "Arial") +
		ggplot2::scale_size(limits = c(0, max(nodes$averageIncidence)), range = c(0, maxNodeSize)) +
		ggraph::scale_edge_width_continuous(range = range(0, maxEdgeSize), limits = c(0, max(edges$nStudies))) +
		ggplot2::coord_cartesian(xlim = computeAxisLimits(nodes$label, "x", maxNodeSize = maxNodeSize, relExpand = relExpandX),
								 ylim = computeAxisLimits(nodes$label, "y", maxNodeSize = maxNodeSize, relExpand = relExpandY)) +
		ggplot2::theme(panel.background = ggplot2::element_rect(fill = "grey98", colour = NA),
					   legend.background = ggplot2::element_blank(),
					   legend.key = ggplot2::element_blank()) 
}
