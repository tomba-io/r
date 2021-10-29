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

#' @title Tomba init
#' @description An S4 class The Tomba API Constructor
#' @slot key A character Tomba API KEY
#' @slot secret A character Tomba SECRET KEY
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

#' @title Tomba Client
#' @description \code{client} Tomba http Client
#'
#' @param obj Tomba class
#' @param path a character specific path.
#' @param query a list for httr request query
#' @return A list of http response
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- client(obj,"https://api.tomba.io/v1/me",null)
#' }
#' @rdname client
#' @export
setGeneric(name="client",
             def=function(obj, path, query = NULL)
             {
               standardGeneric("client")
             }
)

#' @rdname client
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

#' @title Account
#' @description \code{account} Returns information about the current account.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#account-information>
#' @return A list your account data containing your pricing,first_name,last_name,email,country,requests used on domain search and email finder, and email verifier.
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- account(obj)
#' }
#' @rdname account
#' @export
setGeneric(name="account",
           def=function(obj)
           {
             standardGeneric("account")
           }
)

#' @rdname account
setMethod(f="account",
          signature="Tomba",
          definition=function(obj)
          {
            return (client(obj,ACCOUNT_PATH,NULL))
          }
)

#' @title Domain search
#' @description \code{domain_search} Search emails are based on the website You give one domain name and it returns all the email addresses found on the internet.
#'
#' @md
#' @param obj Tomba class
#' @param domain Domain name from which you want to find the email addresses. For example, "stripe.com".
#' @references <https://developer.tomba.io/#domain-search>
#' @return A list domain data containing the organization name,country,social links, and list of emails.
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- domain_search(obj,domain="stripe")
#' }
#' @rdname domain_search
#' @export
setGeneric(name="domain_search",
           def=function(obj,domain)
           {
             standardGeneric("domain_search")
           }
)

#' @rdname domain_search
setMethod(f="domain_search",
          signature="Tomba",
          definition=function(obj,domain)
          {
            return (client(obj,paste0(SEARCH_PATH,domain),NULL))
          }
)

#' @title Email Finder
#' @description \code{email_finder} Generates or retrieves the most likely email address from a domain name, a first name and a last name.
#'
#' @md
#' @param obj Tomba class
#' @param domain a character domain name of the company, used for emails. For example, "asana.com".
#' @param fname The person's first name. It doesn't need to be in lowercase..
#' @param lname The person's last name. It doesn't need to be in lowercase..
#' @references <https://developer.tomba.io/#email-finder>
#' @return A list persons data containing the: first_name,last_name,email,score,department,last_updated.
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- email_finder(obj,fname="FIRST_NAME",lname="LASST_NAME")
#' }
#' @rdname email_finder
#' @export
setGeneric(name="email_finder",
           def=function(obj,domain,fname,lname)
           {
             standardGeneric("email_finder")
           }
)

#' @rdname email_finder
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

#' @title Email Verifier
#' @description \code{email_verifier} Verify the deliverability of an email address.
#'
#'
#' @md
#' @param obj Tomba class
#' @param email a character email address you want to verify.
#' @references <https://developer.tomba.io/#email-verifier>
#' @return A list email data containing the: MX records,SMTP server accepts all,SMTP check,deliverability score,status of the email address,status of the verification.
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- email_verifier(obj,email="info@tomba.io")
#' }
#' @rdname email_verifier
#' @export
setGeneric(name="email_verifier",
           def=function(obj,email)
           {
             standardGeneric("email_verifier")
           }
)

#' @rdname email_verifier
setMethod(f="email_verifier",
          signature="Tomba",
          definition=function(obj,email)
          {
            return (client(obj,paste0(VERIFIER_PATH,email),NULL))
          }
)

#' @title Email Sources
#' @description \code{email_sources} Find email address source somewhere on the web.
#'
#' @md
#' @param obj Tomba class
#' @param email a character email address you want to find sources.
#' @references <https://developer.tomba.io/#email-sources>
#' @return A list sources data data containing the: URL, URL extracted on and last seen on, check if the URL still on page (TRUE|FALSE).
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- email_sources(obj,email="info@tomba.io")
#' }
#' @rdname email_sources
#' @export
setGeneric(name="email_sources",
           def=function(obj,email)
           {
             standardGeneric("email_sources")
           }
)

#' @rdname email_sources
setMethod(f="email_sources",
          signature="Tomba",
          definition=function(obj,email)
          {
            return (client(obj,paste0(SOURCES_PATH,email),NULL))
          }
)

#' @title Usage
#' @description \code{usage} Check your monthly requests.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#usage>
#' @return A list requests data containing the: usage of the domain,finder,verifier and source from: Website, Google Sheets add-on,api, browser extension, bulk tasks.
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- usage(client)
#' }
#' @rdname usage
#' @export
setGeneric(name="usage",
           def=function(obj)
           {
             standardGeneric("usage")
           }
)

#' @rdname usage
setMethod(f="usage",
          signature="Tomba",
          definition=function(obj)
          {
            return (client(obj,USAGE_PATH,NULL))
          }
)

#' @title Logs
#' @description \code{logs} Returns a your last 1,000 requests you made during the last 3 months.
#'
#' @md
#' @param obj Tomba class
#' @references <https://developer.tomba.io/#logs>
#' @return A list requests data containing the: url and The User Agent and IP address associated with the Request,The cost false Free true 1 request,The date,The ISO 3166-1 alpha-2 country code.
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- logs(client)
#' }
#' @rdname logs
#' @export
setGeneric(name="logs",
           def=function(obj)
           {
             standardGeneric("logs")
           }
)

#' @rdname logs
setMethod(f="logs",
          signature="Tomba",
          definition=function(obj)
          {
            return (client(obj,LOGS_PATH,NULL))
          }
)

#' @title Email Count
#' @description \code{count} Returns total email addresses we have for one domain.
#'
#' @md
#' @param obj Tomba class
#' @param domain a character Domain name from which you want to find the email addresses. For example, "stripe.com".
#' @references <https://developer.tomba.io/#email-count>
#' @return A list domain count data containing the: Total email,Total personal email,Total generic email,Total email on department _key_name_.
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- count(client,domain="tomba.io")
#' }
#' @rdname count
#' @export
setGeneric(name="count",
           def=function(obj,domain)
           {
             standardGeneric("count")
           }
)


#' @rdname count
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

#' @title Domain status
#' @description \code{status} Returns domain status if is webmail or disposable.
#'
#'
#' @md
#' @param obj Tomba class
#' @param domain a character Domain name from which you want to check. For example, "gmail.com".
#' @references <https://developer.tomba.io/#domain-status>
#' @return A list domain status data containing the: is webmail email service or is disposable email service
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- status(client,domain="gmail.com")
#' }
#' @rdname status
#' @export
setGeneric(name="status",
           def=function(obj,domain)
           {
             standardGeneric("status")
           }
)

#' @rdname status
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

#' @title Company Autocomplete
#' @description \code{autocomplete} Company Autocomplete is an API that lets you auto-complete company names and retreive logo and domain information.
#'
#' @md
#' @param obj Tomba class
#' @param search a character name company or website.
#' @references <https://developer.tomba.io/#autocomplete>
#' @return A list autocomplete data containing the: Total email on company,company website and name and logo.
#' @examples
#' \dontrun{
#' client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
#' result <- autocomplete(obj,search="google")
#' }
#' @rdname autocomplete
#' @export
setGeneric(name="autocomplete",
           def=function(obj,search)
           {
             standardGeneric("autocomplete")
           }
)

#' @rdname autocomplete
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
