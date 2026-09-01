#' @include 00-tomba-class.R
NULL

#' Similar Domains
#'
#' Retrieve domains similar to the given domain.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name (e.g., \code{"tomba.io"}).
#' @return A list with similar domain information.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- similar_domains(cl, domain = "tomba.io")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/similar#similar-websites}
#' @rdname similar_domains
#' @export
setGeneric(
  name = "similar_domains",
  def  = function(obj, domain) standardGeneric("similar_domains")
)

#' @rdname similar_domains
setMethod(
  f = "similar_domains",
  signature = "Tomba",
  definition = function(obj, domain) {
    query <- list(domain = domain)
    client(obj, SIMILAR_PATH, query)
  }
)
