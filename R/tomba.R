

#' DEFAULT BASE URL
DEFAULT_BASE_URL <- "https://api.tomba.io/v1/"
#' Account path
ACCOUNT_PATH <- "me"
#' Uage path
USAGE_PATH <- "usage"
#' Logs path
LOGS_PATH <- "logs"
#' Search path
SEARCH_PATH <- "domain-search/"
#' Finder path
FINDER_PATH <- "email-finder/"
#' Verifier path
VERIFIER_PATH <- "email-verifier/"
#' Email Sources path
SOURCES_PATH <- "email-sources/"
#' Email Count path
COUNT_PATH <- "email-count/"
#' Domain status path
STATUS_PATH <- "domain-status/"
#' Autocomplete path
AUTOCOMPLETE_PATH <- "domains-suggestion/"

#' The Tomba API Constructor
#' @export Tomba
#' @exportClass Tomba
Tomba <- setClass(
  "Tomba",
  # Define the slots
  slots = c(
    key = "character",
    secret   = "character"
  ),
  # Set the default values for the slots. (optional)
)

setGeneric(name="client",
             def=function(obj, path, query = NULL)
             {
               standardGeneric("client")
             }
)

#' Tomba Client
#'
#' @md
#' @param obj Tomba class
#' @param path      specific path.
#' @param query      query list
#' @export
setMethod(f="client",
          signature="Tomba",
          definition=function(obj, path, query = NULL)
          {
            headers = c(
              `User-Agent` = 'Tomba R-client',
              `Content-Type` = 'application/json; charset=UTF-8',
              `X-Tomba-Key` = obj@key,
              `X-Tomba-Secret` = obj@secret
            )

             data = query

            res <- httr::GET(url = paste0(DEFAULT_BASE_URL,path),query = data,httr::add_headers(.headers=headers), encode = "json")

            if (httr::status_code(res) != 200) stop(res, call.=FALSE)

            res <- httr::content(res, as="text", encoding="UTF-8")

            res <- jsonlite::fromJSON(res)

            return(res)

          }
)

setGeneric(name="account",
           def=function(obj)
           {
             standardGeneric("account")
           }
)

#' Returns information about the current account.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#account-information>
#' @return your account data
#' @export
setMethod(f="account",
          signature="Tomba",
          definition=function(obj)
          {
            return (client(obj,ACCOUNT_PATH,NULL))
          }
)


setGeneric(name="domain_search",
           def=function(obj,domain)
           {
             standardGeneric("domain_search")
           }
)

#' Search emails are based on the website You give one domain name and it returns all the email addresses found on the internet.
#'
#' @md
#' @param obj Tomba class
#' @param domain Domain name from which you want to find the email addresses. For example, "stripe.com".
#' @references <https://developer.tomba.io/#domain-search>
#' @return domain info and emails

#' @export
setMethod(f="domain_search",
          signature="Tomba",
          definition=function(obj,domain)
          {
            return (client(obj,paste0(SEARCH_PATH,domain),NULL))
          }
)


setGeneric(name="email_finder",
           def=function(obj,domain,fname,lname)
           {
             standardGeneric("email_finder")
           }
)

#' Generates or retrieves the most likely email address from a domain name, a first name and a last name.
#'
#' @md
#' @param obj Tomba class
#' @param domain The domain name of the company, used for emails. For example, "asana.com".
#' @param fname The person's first name. It doesn't need to be in lowercase..
#' @param lname The person's last name. It doesn't need to be in lowercase..
#' @references <https://developer.tomba.io/#email-finder>
#' @return persons data
#' @export
setMethod(f="email_finder",
          signature="Tomba",
          definition=function(obj,domain,fname,lname)
          {
            query = list(
              `first_name` = fname,
              `last_name` = lname
            )
            return (client(obj,paste0(FINDER_PATH,domain),query))
          }
)

setGeneric(name="email_verifier",
           def=function(obj,email)
           {
             standardGeneric("email_verifier")
           }
)

#' Verify the deliverability of an email address.
#'
#' @md
#' @param obj Tomba class
#' @param email The email address you want to verify.
#' @references <https://developer.tomba.io/#email-verifier>
#' @return email data
#' @export
setMethod(f="email_verifier",
          signature="Tomba",
          definition=function(obj,email)
          {
            return (client(obj,paste0(VERIFIER_PATH,email),NULL))
          }
)


setGeneric(name="email_sources",
           def=function(obj,email)
           {
             standardGeneric("email_sources")
           }
)

#' Find email address source somewhere on the web.
#'
#' @md
#' @param obj Tomba class
#' @param email The email address you want to find sources.
#' @references <https://developer.tomba.io/#email-sources>
#' @return sources data
#' @export
setMethod(f="email_sources",
          signature="Tomba",
          definition=function(obj,email)
          {
            return (client(obj,paste0(SOURCES_PATH,email),NULL))
          }
)

setGeneric(name="usage",
           def=function(obj)
           {
             standardGeneric("usage")
           }
)

#' Returns a your monthly requests.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#usage>
#' @return requests data
#' @export
setMethod(f="usage",
          signature="Tomba",
          definition=function(obj)
          {
            return (client(obj,USAGE_PATH,NULL))
          }
)

setGeneric(name="logs",
           def=function(obj,email)
           {
             standardGeneric("logs")
           }
)

#' Returns a your last 1,000 requests you made during the last 3 months.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#logs>
#' @return requests data
#' @export
setMethod(f="logs",
          signature="Tomba",
          definition=function(obj)
          {
            return (client(obj,LOGS_PATH,NULL))
          }
)

setGeneric(name="count",
           def=function(obj,domain)
           {
             standardGeneric("count")
           }
)


#' Returns total email addresses we have for one domain.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#email-count>
#' @return domain count data
#' @export
setMethod(f="count",
          signature="Tomba",
          definition=function(obj,domain)
          {
            query = list(
              `domain` = domain
            )
            return (client(obj,COUNT_PATH,query))
          }
)

setGeneric(name="status",
           def=function(obj,domain)
           {
             standardGeneric("status")
           }
)
#' Returns domain status if is webmail or disposable.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#domain-status>
#' @return domain status data
#' @export
setMethod(f="status",
          signature="Tomba",
          definition=function(obj,domain)
          {
            query = list(
              `domain` = domain
            )
            return (client(obj,STATUS_PATH,query))
          }
)

setGeneric(name="autocomplete",
           def=function(obj,search)
           {
             standardGeneric("autocomplete")
           }
)

#' Company Autocomplete is an API that lets you auto-complete company names and retreive logo and domain information.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#autocomplete>
#' @return autocomplete data
#' @export
setMethod(f="autocomplete",
          signature="Tomba",
          definition=function(obj,search)
          {
            query = list(
              `query` = search
            )
            return (client(obj,AUTOCOMPLETE_PATH,query))
          }
)
