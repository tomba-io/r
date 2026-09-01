#' @include 00-tomba-class.R
NULL

#' Domain Search
#'
#' Search for email addresses associated with a domain. Returns all email
#' addresses found on the internet for the given domain.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name to search (e.g., \code{"stripe.com"}).
#' @param enrich_mobile Logical. Enrich with mobile phone data (optional, default \code{NULL}).
#' @param webhook_url Character. Webhook URL for async results (optional, default \code{NULL}).
#' @return A list with organisation name, country, social links, and emails.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- domain_search(cl, domain = "stripe.com")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/finder#domain-search}
#' @rdname domain_search
#' @export
setGeneric(
  name = "domain_search",
  def  = function(obj, domain, enrich_mobile = NULL, webhook_url = NULL) standardGeneric("domain_search")
)

#' @rdname domain_search
setMethod(
  f = "domain_search",
  signature = "Tomba",
  definition = function(obj, domain, enrich_mobile = NULL, webhook_url = NULL) {
    query <- list(domain = domain)
    if (!is.null(enrich_mobile)) query$enrich_mobile <- enrich_mobile
    if (!is.null(webhook_url))   query$webhook_url   <- webhook_url
    client(obj, SEARCH_PATH, query)
  }
)
