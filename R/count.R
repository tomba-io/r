#' @include 00-tomba-class.R
NULL

#' Email Count
#'
#' Returns the total email addresses we have for a domain.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name (e.g., \code{"stripe.com"}).
#' @return A list with total emails, personal, generic, and per-department counts.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- count(cl, domain = "tomba.io")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/finder#email-count#email-count}
#' @rdname count
#' @export
setGeneric(
  name = "count",
  def  = function(obj, domain) standardGeneric("count")
)

#' @rdname count
setMethod(
  f = "count",
  signature = "Tomba",
  definition = function(obj, domain) {
    query <- list(domain = domain)
    client(obj, COUNT_PATH, query)
  }
)
