# PREAMP START
# For devel only: assign(".RESULTS", loadFromDb(conn, resultsCdm), NetworkMetaAnalysis:::.nmaEnv)
# PREAMP END

for (p in c("shiny", "shinydashboard", "shinythemes", "magrittr", "DT", "ggplot2", "dplyr", "tidyr", "forcats")) {
	capture.output(library(p, character.only = TRUE)) # load silently
}

.RESULTS <- get(".RESULTS", envir = NetworkMetaAnalysis:::.nmaEnv)

if (is.null(.RESULTS$nodesplitResults)) {
	hasNodesplitResults <- FALSE
} else {
	hasNodesplitResults <- TRUE
}

packageTheme <- NetworkMetaAnalysis:::packageTheme

# Prepare adaptive content of input field ----
# FIX: Should be informative names and not just the ID's
outcomeIds <- unique(.RESULTS$networkMetaAnalysisDetails$outcomeId) %>%
	setNames(nm = .) %>%
	as.list()

# SIDE BAR ----
sideBar <- dashboardSidebar(
	sidebarMenu(
		selectInput("outcomeId", label = "Outcome", choices = outcomeIds),
		uiOutput("analysisIdSelector"), # adapts to current outcomeId
		menuItem("Graph of studies", tabName = "graphPlot", icon = icon("project-diagram")),
		menuItem("Forest plot", tabName = "forestPlot", icon = icon("align-center")),
		menuItem("Pairwise estimates", tabName = "pairwiseEstimates", icon = icon("th")),
		menuItem("Rankogram", tabName = "rankogram", icon = icon("chart-area")),
		if (isTRUE(hasNodesplitResults)) menuItem("Node-splitting results", tabName = "nodesplitResults", icon = icon("code-branch")),
		menuItem("Parameter summaries", tabName = "modelParameterSummaries", icon = icon("table")),
		menuItem("Diagnostic plots", tabName = "diagnostics", icon = icon("stethoscope")),
		menuItem("Plot apperance", tabName = "plotSettings", icon = icon("pencil-ruler")),
		menuItem("About", tabName = "about", icon = icon("circle"), selected = TRUE)
	)
)

# CONTENT ----
body <- dashboardBody(
	tabItems(
		# > About ----
		tabItem(tabName = "about", 
				h3("Welcome to the OHDSI Network Network Metaanalysis viewer"),
				p("This shiny app allows you to interactively explore the results of the network metaanalysis of 
				  target-comparator-outcomes conducted using the CohortMethod package."),
				p("The package was meant to be used to synthesise results from OHDSI network studies, but can also be used with single-database studies as long as you have several target-comparison pairs for a given outcome."),
				p("This interface combines functionalities in the NetworkMetaAnalysis package, but all results presented in the interface--in plot or table form--can be obtained directly from R using the package functions."),
				h4("What's a network meta analysis?"),
				p("A normal metaanalysis allows you to get an overall estimate of the relative effectiveness between two exposure on a given outcome. Network metaanalyses take this notion further, and allows you to estimate the relative effectiveness between more than 2 exposures and a given outcome, and the OHDSI Network Metaanalysis package leverages Bayesian parameter estimation due to its flexibility. This provides a range of benefits:"),
				tags$ol(
					tags$li("Because the network parameters are estimated together, the estimated effect sizes for weaker head-to-head comparisons will borrow information from the rest of the network. Classic head-to-head meta-analyses can't do that."),
					tags$li("The co-estimation of relative effects not only borrow information from the rest of the network, it also allows to compare pairs of exposures even in the absence of any direct head-to-head comparisons in the primary analyses."),
					tags$li("Network metaanalyses formalise the evaluation of whether head-to-head comparisons make sense and agree. If results conflict, methods exist to aid finding out why (e.g. node-splitting analyses).")
				),
				h4("Links and references"),
				p("The approach to network metaanalyses used in this package builds on the work of the NICE Decision Support Unit (DSU), and implemented in the GeMTC package:"),
				tags$ol(
					tags$li(a("NICE DSU technical support documents", href = "http://nicedsu.org.uk/technical-support-documents/technical-support-documents/", target = "_blank")),
					tags$li(a("GeMTC", href = "https://github.com/gertvv/gemtc", target = "_blank"))
				)
		),
		
		# > Graph ----
		tabItem(tabName = "graphPlot",
				fluidRow(
					box(width = 12, 
						plotOutput("graph", height = "700") 
					)
				)
		),
		
		# > Forest plot ----
		tabItem(tabName = "forestPlot",
				fluidRow(box(title = "Forest plot",
							 width = 12, 
							 plotOutput("forestPlot")
						 )
				),
				fluidRow(
					box(title = "Plot customisation",
						width = 6,
						collapsed = FALSE,
						collapsible = TRUE,
						fluidRow(column(width = 6,
										checkboxInput("forestPlotAllAnalyses", label = "Plot all analyses?", value = TRUE)),
								 column(width = 6,
								 	   uiOutput("referenceTreatmentSelector")
								 )
							)
						)
				)
		),
		
		# > Pretty parameter summaries ----
		tabItem(tabName = "modelParameterSummaries",
				fluidRow(
					box(width = 12,
						dataTableOutput("modelParameterSummaries")
					)
				),
				fluidRow(
					box(title = "No. digits",
						width = 3,
						collapsible = TRUE,
						collapsed = FALSE,
						fluidRow(
							column(width = 12,
								   numericInput("parameterSummariesNdigits", label = NULL, value = 1, min = 0, step = 1)
							)
						)
					)
				)
		),
		
		# > Rankogram ----
		tabItem(tabName = "rankogram",
				fluidRow(box(title = "Rankogram", width = 12, plotOutput("rankogram")))
		),
		
		# > Pairwise estimates ----
		tabItem(tabName = "pairwiseEstimates", 
				fluidRow(
					tabBox(width = 12,
						   id = "pairwiseEstimates",
						   tabPanel(title = "Heatmap", 
						   		 value = "heatmap",
						   		 p("Low values favour exposures on the vertical axis; high values those on the horisontal axis. SUCRA (surface under the cumulative rankogram) values are shown in the diagonal."),
						   		 plotOutput("pairwiseEstimatesHeatmap", height = "700")
						   ),
						   tabPanel(title = "Tidy format",
						   		 value = "tidyFormat",
						   		 p("Pairwise estimates in tidy format"),
						   		 br(),
						   		 dataTableOutput("pairwiseEstimatesTidy")
						   )
					),
					conditionalPanel(
						condition = "input.pairwiseEstimates == 'heatmap'",
						uiOutput("pairwiseEstimatesPlotCustomisationBox")
					),
					conditionalPanel(
						condition = "input.pairwiseEstimates == 'tidyFormat'",
						box(title = "No. digits",
							width = 3,
							collapsible = TRUE,
							collapsed = FALSE,
							fluidRow(
								column(width = 12,
									   numericInput("pairwiseEstimatesTidyNdigits", label = NULL, value = 1, min = 0, step = 1)
								)
							)
						)
					)
				)
		),
		
		# > Diagnostic plots ----
		tabItem(tabName = "diagnostics",
				fluidRow(
					tabBox(width = 12,
						   tabPanel("Density plots",
						   		 br(),
						   		 plotOutput("densityPlot")
						   ),
						   tabPanel("Trace plots",
						   		 br(),
						   		 plotOutput("tracePlot")
						   ),
						   tabPanel("Potential scale reduction factor (Rhat)",
						   		 br(),
						   		 plotOutput("rhatPlot")
						   )
						)
				),
				fluidRow(
					box(title = "Choose model",
						width = 4,
						collapsible = TRUE,
						collapsed = FALSE,
						fluidRow(
							column(width = 12,
								   uiOutput("diagnosticsModelSelector")
							)
						)
					)
				)
		),
		
		# > Node-splitting results ----
		tabItem(tabName = "nodesplitResults",
				fluidRow(
					box(
						width = 12,
						dataTableOutput("nodesplitResults")
					)
				),
				fluidRow(
					box(title = "No. digits",
						width = 3,
						collapsible = TRUE,
						collapsed = FALSE,
						fluidRow(
							column(width = 12,
								   numericInput("nodesplitNdigits", label = NULL, value = 1, min = 0, step = 1)
							)
						)
					)
				)
		),
		
		# > Plot appearance ----
		# FIX: Consider turning this into a widget that can be included on all plot pages
		tabItem(tabName = "plotSettings",
				fluidRow(
					box(title = "Settings",
						width = 12,
						fluidRow(
							column(width = 3, numericInput("plotTextSize", "Text size", value = 14, min = 1, step = 1)),
							column(width = 3, numericInput("plotTitleSize", "Title size", value = 14, min = 1, step = 1)),
							column(width = 3, numericInput("plotLineSize", "Line size", value = 0.75, min = 0, step = 0.25)),
							column(width = 3, sliderInput("plotAlpha", "Opacity", value = 70, min = 0, max = 100, step = 5, ticks = FALSE, post = "%"))
						),
						fluidRow(
							column(width = 3, numericInput("plotPointSize", "Point size", value = 2, min = 0, step = 1))
						)
					)
				),
				fluidRow(
					box(
						width = 12,
						title = "Example with settings",
						plotOutput("plotSettingsExamplePlot")
					)
				)
		)
	)
)

# BUILD USER INTERFACE ----
ui <- dashboardPage(
	dashboardHeader(title = "OHDSI NMA viewer"),
	sideBar,
	body,
	skin = "blue"
)

# SERVER LOGIC ----
server <- function(input, output) {
	# > REACTIVE data objects ----
	nmaId <- reactive({
		req(input$analysisId)
		
		dplyr::filter(.RESULTS$networkMetaAnalysisDetails, 
					  outcomeId == input$outcomeId, analysisId == input$analysisId)$networkMetaAnalysisId
	})
	
	d <- reactive({
		purrr::keep(.RESULTS, ~ !isTRUE(is.null(.))) %>%
			sapply(function(.) select(filter(., networkMetaAnalysisId == nmaId()), -networkMetaAnalysisId),
				   simplify = FALSE)
	})
	
	posteriorSamples <- reactive({
		req(input$diagnosticsModelToPlot)
		dplyr::filter(d()$posteriorSamples, modelName == input$diagnosticsModelToPlot)
	})
	
	# > REACTIVE input fields ----
	# FIX: Should be informative names, not just ID's
	output$analysisIdSelector <- renderUI({
		analysisIds <- dplyr::filter(.RESULTS$networkMetaAnalysisDetails, outcomeId == input$outcomeId)$analysisId %>%
			setNames(nm = .) %>%
			as.list()
		
		selectInput("analysisId", label = "Analysis", choices = analysisIds)
	})
	
	output$referenceTreatmentSelector <- renderUI({
		exposureIds <- dplyr::filter(.RESULTS$networkMetaAnalysisData, outcomeId == input$outcomeId)$treatment %>%
			unique() %>%
			sort() %>%
			setNames(nm = .) %>%
			as.list()
		
		selectInput("referenceTreatmentId", label = "Reference exposure", choices = exposureIds)
	})
	
	output$diagnosticsModelSelector <- renderUI({
		availableModels <- unique(d()$posteriorSamples$modelName)
		selectInput("diagnosticsModelToPlot", label = NULL, availableModels, selected = "consistency")
	})
	
	output$heatmapThresholdInput <- renderUI({
		defaultThreshold <- round((abs(log(max(d()$pairwiseEstimates$hr))) / 2) / 0.05) * 0.05
			# round to 0.05
		numericInput("heatmapContrastThreshold", "Contrast threshold", value = defaultThreshold, min = 0, step = 0.05)
	})
	
	output$pairwiseEstimatesPlotCustomisationBox <- renderUI({
		box(title = "Plot customisation",
			width = 12,
			collapsible = TRUE,
			collapsed = FALSE,
			fluidRow(
				column(width = 3,
					   textInput("heatmapColourLow", "Colour, low values", value = "blue")
				),
				column(width = 3,
					   textInput("heatmapColourHigh", "Colour, high values", value = "red")
				),
				column(width = 3,
					   uiOutput("heatmapThresholdInput")
				),
				column(width = 3,
					   numericInput("heatmapNdigits", "No. digits", value = 1, min = 0, step = 1)
				)
			)
		)
	})
	
	
	# > REACTIVE helpers ----
	shinyTheme <- reactive({
		textElement <- ggplot2::element_text(size = input$plotTextSize)
		titleElement <- ggplot2::element_text(size = input$plotTitleSize)
		ggplot2::theme(axis.text = textElement,
					   axis.title = titleElement,
					   legend.title = titleElement,
					   legend.text = textElement,
					   strip.text = titleElement)
	})
	
	plotAlpha <- reactive({
		input$plotAlpha / 100
	})
	
	# > STATIC helpers ----
	combineMedianIqr <- function(median, lb, ub, nDigits = 1) { 
		parameterSummaryFormat <- stringr::str_replace_all("%.xf (%.xf, %.xf)", "x", paste(nDigits))
		sprintf(parameterSummaryFormat, median, lb, ub)
	}
	
	# > Pretty parameter summaries ----
	output$modelParameterSummaries <- renderDataTable({
		fetchParameterSummaries(d()$modelParameterSummaries, d()$devianceStatistics, nDigits = input$parameterSummariesNdigits) %>%
			datatable(extensions = "Responsive")
	})
	
	# > Graph ----
	output$graph <- renderPlot({
		ggplot2::update_geom_defaults("text", list(size = input$plotTextSize / ggplot2::.pt))
		
		plotNetwork(d()$graphEdges, d()$graphNodes) + 
			shinyTheme() + 
			theme(panel.background = element_blank(),
				  axis.title = element_blank(),
				  axis.text = element_blank()) 
	})
	
	# > Forest plot ----
	output$forestPlot <- renderCachedPlot(
		{
			ggplot2::update_geom_defaults("text", list(size = input$plotTextSize / ggplot2::.pt))
			nmaIds <- dplyr::filter(.RESULTS$networkMetaAnalysisDetails, outcomeId == input$outcomeId)$networkMetaAnalysisId
			
			if (isTRUE(input$forestPlotAllAnalyses)) {
				dta <- filter(.RESULTS$pairwiseEstimates, networkMetaAnalysisId %in% nmaIds) %>%
					left_join(select(.RESULTS$networkMetaAnalysisDetails, networkMetaAnalysisId, analysisId),
							  by = "networkMetaAnalysisId") %>%
					select(-networkMetaAnalysisId)
			} else {
				dta <- d()$pairwiseEstimates
			}
			
			req(input$referenceTreatmentId)
			dplyr::filter(dta, comparatorId == input$referenceTreatmentId) %>%
				plotForest(lineSize = input$plotLineSize, pointSize = input$plotPointSize) +
				shinyTheme()
		}, 
		cacheKeyExpr = { 
			list(input$forestPlotAllAnalyses, input$referenceTreatmentId, input$outcomeId,
				 input$plotLineSize, input$plotPointSize, plotAlpha())
		}
	)
	
	# > Pairwise estimates ----
	output$pairwiseEstimatesTidy <- renderDataTable({
		mutate_if(d()$pairwiseEstimates, is.numeric, round, input$pairwiseEstimatesTidyNdigits) %>%
			datatable(extensions = "Responsive")
	})
	
	output$pairwiseEstimatesHeatmap <- renderPlot( 
		# caching plot deactives because changes in input$heatmapNdigits only recognised when value > 4
		{
			ggplot2::update_geom_defaults("text", list(size = input$plotTextSize / ggplot2::.pt))
			
			req(input$heatmapContrastThreshold)
			plotPairwiseEstimates(d()$pairwiseEstimates, mid = "hr", lo = "cri95Lb", hi = "cri95Ub",
								  colour_low = input$heatmapColourLow, colour_high = input$heatmapColourHigh,
								  contrastThreshold = input$heatmapContrastThreshold, nDigits = input$heatmapNdigits,
								  sucraEstimates = computeSucra(d()$rankProbabilities)) +
				shinyTheme()
		}#,
		# cacheKeyExpr = { 
		# 	list(nmaId(), input$plotTextSize, input$heatmapContrastThreshold, input$heatmapColourLow,
		# 		 input$heatmapColourHigh, input$heatmapNdigits)
		# }
	)
	
	# > Rankogram ----
	output$rankogram <- renderCachedPlot(
		{
			plotRankogram(d()$rankProbabilities, lineSize = input$plotLineSize, lineAlpha = plotAlpha())+
				shinyTheme()
		},
		cacheKeyExpr = { 
			list(nmaId(), input$plotLineSize, input$plotTextSize, plotAlpha())
		}
	)
		
	# > Diagnostic plots ----
	output$rhatPlot <- renderCachedPlot(
		{
			plotRhat(d()$potentialScaleReductionFactor, pointSize = input$plotPointSize, 
					 lineSize = input$plotLineSize) +
				shinyTheme()
		},
		cacheKeyExpr = { 
			list(nmaId(), input$plotLineSize, input$plotPointSize, input$plotTextSize)
		}
	)
	
	output$densityPlot <- renderCachedPlot(
		{
			plotPosteriorDensities(posteriorSamples(), lineSize = input$plotLineSize, 
								   lineAlpha = plotAlpha()) +
				shinyTheme()
		},
		cacheKeyExpr = { 
			list(nmaId(), input$diagnosticsModelToPlot, input$plotLineSize, plotAlpha(), 
				 input$plotTextSize)
		}
	)
	
	output$tracePlot <- renderCachedPlot(
		{
			plotTraces(posteriorSamples(), lineSize = input$plotLineSize, lineAlpha = plotAlpha()) +
				shinyTheme()
		},
		cacheKeyExpr = { 
			list(nmaId(), input$diagnosticsModelToPlot, input$plotLineSize, plotAlpha(), 
				 input$plotTextSize)
		}
	)
	
	# > Node-splitting results ----
	output$nodesplitResults <- renderDataTable({
		deriveSummary <- function(modelType) {
			stringr::str_replace_all("combineMedianIqr(xHr, xLb, xUb, nDigits = input$nodesplitNdigits)", "x", modelType) %>%
				rlang::parse_expr()
		}
		
		print(deriveSummary("network"))
		
		dplyr::select(d()$nodesplitResults, -modelName) %>%
			# dplyr::mutate_if(is.numeric, round, digits = input$nodesplitNdigits) %>%
			dplyr::rename_all(stringr::str_replace_all, pattern = c("splitE" = "e")) %>%
			dplyr::transmute(exposureId1,
							 exposureId2,
							 pValue = round(pValue, 3),
							 network = !!deriveSummary("network"),
							 direct = !!deriveSummary("direct"),
							 indirect = !!deriveSummary("indirect")) %>% 
			datatable(extensions = "Responsive")
	})
	
	# > Plot appearance ----
	output$plotSettingsExamplePlot <- renderPlot({
		# FIX: Consider allowing relative text size differences between titles and text (e.g. axes)
		
		ggplot2::update_geom_defaults("text", list(size = input$plotTextSize / ggplot2::.pt))
		x <- 0:10 / 10
		dta <- data.frame(x = c(x, x), 
						  y = c(exp(x) - 1, exp(1) - exp(x)), 
						  Group = rep(c("a", "b"), each = length(x)),
						  facet = "Facet title")
		
		ggplot(dta, aes(x, y, colour = Group)) +
			geom_line(size = input$plotLineSize, alpha = plotAlpha()) +
			geom_point(data = ~ filter(., x == 0.2), size = input$plotPointSize, alpha = plotAlpha()) +
			annotate(x = 0.25, y = (exp(1) - 1) / 2, label = "OHDSI", geom = "text") +
			annotate(x = 0.9, y = (exp(1) - 1) / 2, label = deparse(substitute(pi %~~% x, list(x = 3.14))), geom = "text", parse = TRUE) +
			labs(x = "X axis", y = "Y axis") +
			facet_wrap(~ facet) +
			packageTheme +
			shinyTheme()
	})
}

# RUN APPLICAITON ----
shinyApp(ui = ui, server = server)

