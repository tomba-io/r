#' @include 00-tomba-class.R
NULL

#' Person Find (Enrichment)
#'
#' Get person data from an email address.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param email Character. The email address to look up.
#' @return A list with person enrichment data.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- person_find(cl, email = "user@example.com")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/enrichment#person}
#' @rdname person_find
#' @export
setGeneric(
  name = "person_find",
  def  = function(obj, email) standardGeneric("person_find")
)

#' @rdname person_find
setMethod(
  f = "person_find",
  signature = "Tomba",
  definition = function(obj, email) {
    query <- list(email = email)
    client(obj, PERSON_PATH, query)
  }
)

#' Company Find (Enrichment)
#'
#' Get company data from a domain.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name (e.g., \code{"example.com"}).
#' @return A list with company enrichment data.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- company_find(cl, domain = "tomba.io")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/enrichment#company}
#' @rdname company_find
#' @export
setGeneric(
  name = "company_find",
  def  = function(obj, domain) standardGeneric("company_find")
)

#' @rdname company_find
setMethod(
  f = "company_find",
  signature = "Tomba",
  definition = function(obj, domain) {
    query <- list(domain = domain)
    client(obj, COMPANY_PATH, query)
  }
)

#' Combined Find (Enrichment)
#'
#' Get combined person and company data from an email address.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param email Character. The email address to look up.
#' @return A list with combined person and company enrichment data.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- combined_find(cl, email = "user@example.com")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/enrichment#combined}
#' @rdname combined_find
#' @export
setGeneric(
  name = "combined_find",
  def  = function(obj, email) standardGeneric("combined_find")
)

#' @rdname combined_find
setMethod(
  f = "combined_find",
  signature = "Tomba",
  definition = function(obj, email) {
    query <- list(email = email)
    client(obj, COMBINED_PATH, query)
  }
)
