#' Launch a Shiny app to explore the results
#'
#' @param results the output of \code{reconcileResults} directly or \code{loadFromDb} if the results
#'   live on a server.
#' @param rstudio boolean, should the app run in the Viewer window (\code{TRUE}, default) or in an
#'   internet browser (\code{FALSE})?
#'
#' @export

launchShinyApp <- function(results, rstudio = TRUE, ...) {
	if (class(results) != "ReconciledOhdsiNetworkMetaAnalysis") {
		stop("results must be the output of either reconcileResults() or loadFromDb().", call. = FALSE)
	}
	
	if (isTRUE(rstudio)) {
		launchBrowser <- getOption("shiny.launch.browser", interactive())
	} else {
		launchBrowser <- TRUE
	}

	.nmaEnv$.RESULTS <- results
	on.exit(rm(.RESULTS, pos = .nmaEnv), add = TRUE) # only needed while running shiny app
	
	shiny::runApp(system.file("shinyApp", package = "NetworkMetaAnalysis"),
				  launch.browser = launchBrowser, ...)
}
