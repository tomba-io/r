#' @include 00-tomba-class.R
NULL

#' Get Location
#'
#' Get the location of a domain.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name (e.g., \code{"tomba.io"}).
#' @return A list with the location data for the domain.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- get_location(cl, domain = "tomba.io")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/finder#location#location}
#' @rdname get_location
#' @export
setGeneric(
  name = "get_location",
  def  = function(obj, domain) standardGeneric("get_location")
)

#' @rdname get_location
setMethod(
  f = "get_location",
  signature = "Tomba",
  definition = function(obj, domain) {
    query <- list(domain = domain)
    client(obj, LOCATION_PATH, query)
  }
)
