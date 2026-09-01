#' @include 00-tomba-class.R
NULL

#' Email Sources
#'
#' Find the web sources where an email address has been found.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param email Character. The email address to find sources for.
#' @return A list with source data including URL, extracted-on date,
#'   last-seen date, and whether the URL is still on-page.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- email_sources(cl, email = "info@tomba.io")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/email#email-sources#email-sources}
#' @rdname email_sources
#' @export
setGeneric(
  name = "email_sources",
  def  = function(obj, email) standardGeneric("email_sources")
)

#' @rdname email_sources
setMethod(
  f = "email_sources",
  signature = "Tomba",
  definition = function(obj, email) {
    query <- list(email = email)
    client(obj, SOURCES_PATH, query)
  }
)
