#' @include 00-tomba-class.R
NULL

#' Email Format
#'
#' Get the email format used by a domain.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name (e.g., \code{"tomba.io"}).
#' @return A list with the email format information for the domain.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- email_format(cl, domain = "tomba.io")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/finder#email-format#email-format}
#' @rdname email_format
#' @export
setGeneric(
  name = "email_format",
  def  = function(obj, domain) standardGeneric("email_format")
)

#' @rdname email_format
setMethod(
  f = "email_format",
  signature = "Tomba",
  definition = function(obj, domain) {
    query <- list(domain = domain)
    client(obj, FORMAT_PATH, query)
  }
)
