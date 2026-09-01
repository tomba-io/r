#' @include 00-tomba-class.R
NULL

#' Email Finder
#'
#' Generates or retrieves the most likely email address from a domain name,
#' a first name, and a last name.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param domain Character. The domain name (e.g., \code{"asana.com"}).
#' @param fname Character. The person's first name.
#' @param lname Character. The person's last name.
#' @param webhook_url Character. Webhook URL for async results (optional, default \code{NULL}).
#' @return A list with person data including email, score, department, etc.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- email_finder(cl, domain = "asana.com",
#'                        fname = "John", lname = "Doe")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/finder#email-finder}
#' @rdname email_finder
#' @export
setGeneric(
  name = "email_finder",
  def  = function(obj, domain, fname, lname, webhook_url = NULL) standardGeneric("email_finder")
)

#' @rdname email_finder
setMethod(
  f = "email_finder",
  signature = "Tomba",
  definition = function(obj, domain, fname, lname, webhook_url = NULL) {
    query <- list(
      domain     = domain,
      first_name = fname,
      last_name  = lname
    )
    if (!is.null(webhook_url)) query$webhook_url <- webhook_url
    client(obj, FINDER_PATH, query)
  }
)

#' Author Finder
#'
#' Generates or retrieves the most likely email address from a blog post URL.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param url Character. The URL of the article.
#' @param webhook_url Character. Webhook URL for async results (optional, default \code{NULL}).
#' @return A list with person data including email, score, department, etc.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- author_finder(cl,
#'   url = "https://clearbit.com/blog/company-name-to-domain-api")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/finder#author-finder}
#' @rdname author_finder
#' @export
setGeneric(
  name = "author_finder",
  def  = function(obj, url, webhook_url = NULL) standardGeneric("author_finder")
)

#' @rdname author_finder
setMethod(
  f = "author_finder",
  signature = "Tomba",
  definition = function(obj, url, webhook_url = NULL) {
    query <- list(url = url)
    if (!is.null(webhook_url)) query$webhook_url <- webhook_url
    client(obj, AUTHOR_PATH, query)
  }
)

#' LinkedIn Finder
#'
#' Generates or retrieves the most likely email address from a LinkedIn URL.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param url Character. The LinkedIn profile URL.
#' @param webhook_url Character. Webhook URL for async results (optional, default \code{NULL}).
#' @return A list with person data including email, score, department, etc.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- linkedin_finder(cl,
#'   url = "https://www.linkedin.com/in/alex-maccaw-ab592978")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/finder#linkedin-finder}
#' @rdname linkedin_finder
#' @export
setGeneric(
  name = "linkedin_finder",
  def  = function(obj, url, webhook_url = NULL) standardGeneric("linkedin_finder")
)

#' @rdname linkedin_finder
setMethod(
  f = "linkedin_finder",
  signature = "Tomba",
  definition = function(obj, url, webhook_url = NULL) {
    query <- list(url = url)
    if (!is.null(webhook_url)) query$webhook_url <- webhook_url
    client(obj, LINKEDIN_PATH, query)
  }
)
