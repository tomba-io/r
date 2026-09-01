#' @include 00-tomba-class.R
NULL

#' Domain Status
#'
#' Returns whether a domain is a webmail or disposable email service.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name (e.g., \code{"gmail.com"}).
#' @return A list indicating webmail and disposable status.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- status(cl, domain = "gmail.com")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/domain#domain-status#domain-status}
#' @rdname status
#' @export
setGeneric(
  name = "status",
  def  = function(obj, domain) standardGeneric("status")
)

#' @rdname status
setMethod(
  f = "status",
  signature = "Tomba",
  definition = function(obj, domain) {
    query <- list(domain = domain)
    client(obj, STATUS_PATH, query)
  }
)

#' Company Autocomplete
#'
#' Auto-complete company names and retrieve logo and domain information.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param search Character. Company name or website to search.
#' @return A list with autocomplete suggestions including company names,
#'   domains, logos, and email counts.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- autocomplete(cl, search = "google")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/domain#domain-status#company-autocomplete}
#' @rdname autocomplete
#' @export
setGeneric(
  name = "autocomplete",
  def  = function(obj, search) standardGeneric("autocomplete")
)

#' @rdname autocomplete
setMethod(
  f = "autocomplete",
  signature = "Tomba",
  definition = function(obj, search) {
    query <- list(query = search)
    client(obj, AUTOCOMPLETE_PATH, query)
  }
)
