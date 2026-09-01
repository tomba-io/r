#' @include 00-tomba-class.R
NULL

#' API Usage
#'
#' Check your monthly API request usage.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @return A list with usage data broken down by service (domain search,
#'   finder, verifier, sources) and origin (website, add-on, API, etc.).
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- usage(cl)
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/account#retrieve-api-usage}
#' @rdname usage
#' @export
setGeneric(
  name = "usage",
  def  = function(obj) standardGeneric("usage")
)

#' @rdname usage
setMethod(
  f = "usage",
  signature = "Tomba",
  definition = function(obj) {
    client(obj, USAGE_PATH, NULL)
  }
)
