#' Fetches and tidies the posterior draws
#'
#' Potentially, with some thinning to keep the amount of space required down.
#'
#' @param fit the result of \code{fitNetwork}.
#' @param thin scalar, thinning factor. The default value (\code{10}) keeps every tenth observation.
#'   This is mainly used when producing output to be saved to the server, to keep the storage
#'   requirements at a reasonable level.
#'
#' @return Tidy \code{tibble} with columns for chain and MCMC iteration indicators and one column
#'   per parameter.
#'
#' @export

fetchPosteriorDraws <- function(fit, drawsThin = 10) {
	# FIX: Give actual names in comparison parameters
	# FIX: Get rid of :::
	
	if (drawsThin < 1 | drawsThin %% 1 != 0) {
		stop("drawsThin must be an integer >= 1.", call. = FALSE)
	}
	
	if (class(fit) == "mtc.nodesplit") {
		return(dplyr::bind_rows(lapply(fit, fetchPosteriorDraws, drawsThin = drawsThin)))
	}
	
	# Start wrangling
	o <- gemtc:::as.mcmc.list.mtc.result(fit) %>% 
		as.matrix(iters = TRUE, chains = TRUE) %>%
		tibble::as_tibble(.name_repair = tolower) 
	
	# Interrupt to apply thinning before pivoting to long format
	if (drawsThin > 1) {
		o <- dplyr::filter(o, iter %% drawsThin == 0)
	} 
	
	# Pivot, finish wrangling and return result
	tidyr::gather(o, parameterName, parameterValue, -chain, -iter) %>%
		dplyr::mutate_at(dplyr::vars(chain, iter), as.integer) %>%
		dplyr::mutate(modelName = deriveModelName(fit)) %>%
		dplyr::select(modelName, chainId = chain, mcmcIterationId = iter, dplyr::everything())
}
