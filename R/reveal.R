#' @include 00-tomba-class.R
NULL

#' Companies Search
#'
#' Search companies using natural language queries or structured filters.
#' This endpoint uses a POST request.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param data A named list of search parameters (e.g., \code{list(query = "Real Estate in Europe", page = 1)}).
#' @return A list with matching company data.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- companies_search(cl, data = list(query = "Real Estate in Europe", page = 1))
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/reveal#companies-search}
#' @rdname companies_search
#' @export
setGeneric(
  name = "companies_search",
  def  = function(obj, data = list()) standardGeneric("companies_search")
)

#' @rdname companies_search
setMethod(
  f = "companies_search",
  signature = "Tomba",
  definition = function(obj, data = list()) {
    client_post(obj, REVEAL_PATH, data)
  }
)
