#' @include 00-tomba-class.R
NULL

#' Technology Check
#'
#' Retrieve the technologies used by a domain.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name (e.g., \code{"tomba.io"}).
#' @return A list with technologies used by the domain.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- technology_check(cl, domain = "tomba.io")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/domain#technology#technology}
#' @rdname technology_check
#' @export
setGeneric(
  name = "technology_check",
  def  = function(obj, domain) standardGeneric("technology_check")
)

#' @rdname technology_check
setMethod(
  f = "technology_check",
  signature = "Tomba",
  definition = function(obj, domain) {
    query <- list(domain = domain)
    client(obj, TECHNOLOGY_PATH, query)
  }
)
