#' @include 00-tomba-class.R
NULL

#' API Logs
#'
#' Returns the last 1,000 requests made during the last 3 months.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param page Integer. Page number for pagination (optional).
#' @param limit Integer. Number of results per page (optional).
#' @return A list with request log entries containing URL, user-agent,
#'   IP address, cost, date, and country code.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- logs(cl)
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/account#retrieve-api-logs}
#' @rdname logs
#' @export
setGeneric(
  name = "logs",
  def  = function(obj, page = NULL, limit = NULL) standardGeneric("logs")
)

#' @rdname logs
setMethod(
  f = "logs",
  signature = "Tomba",
  definition = function(obj, page = NULL, limit = NULL) {
    query <- list()
    if (!is.null(page))  query$page  <- page
    if (!is.null(limit)) query$limit <- limit
    client(obj, LOGS_PATH, query)
  }
)
