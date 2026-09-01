#' @title The official R client library for Tomba.io
#' @description Provides access to the Tomba Email Finder API for domain search,
#'   email finding, verification, enrichment, leads management, and more.
#'
#' @md
#' @name tomba
#' @docType package
#' @author Abedrahim Ben rebia <b.abedrahim@tomba.io>
#' @import httr
#' @import jsonlite
#' @import methods
#' @keywords internal
"_PACKAGE"

# ---------------------------------------------------------------------------
# Constants -- API paths
# ---------------------------------------------------------------------------

#' Default base URL for the Tomba API.
#' @export
DEFAULT_BASE_URL <- "https://api.tomba.io/v1/"

#' Account endpoint path.
#' @keywords internal
ACCOUNT_PATH <- "me"

#' Usage endpoint path.
#' @keywords internal
USAGE_PATH <- "usage"

#' Logs endpoint path.
#' @keywords internal
LOGS_PATH <- "logs"

#' Domain search endpoint path.
#' @keywords internal
SEARCH_PATH <- "domain-search"

#' Email finder endpoint path.
#' @keywords internal
FINDER_PATH <- "email-finder"

#' Author finder endpoint path.
#' @keywords internal
AUTHOR_PATH <- "author-finder"

#' LinkedIn finder endpoint path.
#' @keywords internal
LINKEDIN_PATH <- "linkedin"

#' Phone finder endpoint path.
#' @keywords internal
PHONE_PATH <- "phone-finder"

#' Phone validator endpoint path.
#' @keywords internal
PHONE_VALIDATOR_PATH <- "phone-validator"

#' Email verifier endpoint path.
#' @keywords internal
VERIFIER_PATH <- "email-verifier"

#' Email sources endpoint path.
#' @keywords internal
SOURCES_PATH <- "email-sources"

#' Email count endpoint path.
#' @keywords internal
COUNT_PATH <- "email-count"

#' Domain status endpoint path.
#' @keywords internal
STATUS_PATH <- "domain-status"

#' Domain suggestions (autocomplete) endpoint path.
#' @keywords internal
AUTOCOMPLETE_PATH <- "domain-suggestions"

#' Email format endpoint path.
#' @keywords internal
FORMAT_PATH <- "email-format"

#' Location endpoint path.
#' @keywords internal
LOCATION_PATH <- "location"

#' Similar domains endpoint path.
#' @keywords internal
SIMILAR_PATH <- "similar"

#' Technology endpoint path.
#' @keywords internal
TECHNOLOGY_PATH <- "technology"

#' Person enrichment endpoint path.
#' @keywords internal
PERSON_PATH <- "people/find"

#' Company enrichment endpoint path.
#' @keywords internal
COMPANY_PATH <- "companies/find"

#' Combined enrichment endpoint path.
#' @keywords internal
COMBINED_PATH <- "combined/find"

#' Companies search (reveal) endpoint path.
#' @keywords internal
REVEAL_PATH <- "reveal/search"

#' API keys endpoint path.
#' @keywords internal
KEYS_PATH <- "keys"

#' Flags endpoint path.
#' @keywords internal
FLAG_PATH <- "flag"

#' Leads endpoint path.
#' @keywords internal
LEADS_PATH <- "leads"

#' Leads lists endpoint path.
#' @keywords internal
LEADS_LISTS_PATH <- "leads_lists"

#' Leads attributes endpoint path.
#' @keywords internal
LEADS_ATTRIBUTES_PATH <- "attributes"

#' Bulk operations endpoint path.
#' @keywords internal
BULK_PATH <- "bulk"

#' Valid bulk operation types.
#' @keywords internal
VALID_BULK_TYPES <- c("search", "similar", "company", "finder", "enrich", "linkedin", "author", "verifier", "phone-finder", "phone-validator")

# ===========================================================================
# Tomba S4 class
# ===========================================================================

#' Tomba API Client
#'
#' An S4 class representing a connection to the Tomba API.
#'
#' @slot key Character. Your Tomba API key (starts with \code{ta_}).
#' @slot secret Character. Your Tomba secret key (starts with \code{ts_}).
#'
#' @examples
#' \dontrun{
#' client <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' }
#'
#' @seealso \url{https://docs.tomba.io/}
#' @export Tomba
#' @exportClass Tomba
Tomba <- setClass(
  "Tomba",
  slots = c(
    key = "character",
    secret = "character"
  )
)

# ===========================================================================
# Internal HTTP helpers
# ===========================================================================

#' Build request headers
#'
#' @param obj A \code{\link{Tomba}} object.
#' @return Named character vector of HTTP headers.
#' @keywords internal
.tomba_headers <- function(obj) {
  c(
    `User-Agent`    = "Tomba R-client",
    `Content-Type`  = "application/json; charset=UTF-8",
    `X-Tomba-Key`   = obj@key,
    `X-Tomba-Secret` = obj@secret
  )
}

#' Rate limit header names to extract from API responses.
#' @keywords internal
.rate_limit_header_names <- c(
  "x-second-rate-limit",
  "x-minute-rate-limit",
  "x-daily-rate-limit",
  "x-minute-request-left",
  "x-daily-request-left",
  "x-minute-reset-seconds",
  "x-daily-reset-seconds",
  "retry-after",
  "ratelimit-policy",
  "ratelimit"
)

#' Extract rate limit headers from an httr response
#'
#' @param res An \code{httr} response object.
#' @return A named list of rate limit header values.
#' @keywords internal
.tomba_rate_limit <- function(res) {
  hdrs <- httr::headers(res)
  rl <- list()
  for (name in .rate_limit_header_names) {
    val <- hdrs[[name]]
    if (!is.null(val)) {
      rl[[name]] <- val
    }
  }
  rl
}

#' Parse an HTTP response
#'
#' @param res An \code{httr} response object.
#' @return A list with \code{data} (parsed JSON body) and \code{rate_limit}
#'   (named list of rate limit header values).
#' @keywords internal
.tomba_parse <- function(res) {
  if (httr::status_code(res) >= 400) {
    stop(
      sprintf(
        "Tomba API error %s: %s",
        httr::status_code(res),
        httr::content(res, as = "text", encoding = "UTF-8")
      ),
      call. = FALSE
    )
  }
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(txt)
  rate_limit <- .tomba_rate_limit(res)
  list(data = content, rate_limit = rate_limit)
}

# ---------------------------------------------------------------------------
# client (GET) -- kept for backward compatibility
# ---------------------------------------------------------------------------

#' Tomba HTTP GET Client
#'
#' Sends a GET request to the Tomba API. This is the original HTTP helper
#' retained for backward compatibility. New code should prefer the typed
#' helpers (\code{.tomba_get}, \code{.tomba_post}, etc.).
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param path Character. The API path (relative to the base URL).
#' @param query A named list of query parameters (optional).
#' @return A list with the parsed API response.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- client(cl, "me", NULL)
#' }
#'
#' @rdname client
#' @export
setGeneric(
  name = "client",
  def  = function(obj, path, query = NULL) {
    standardGeneric("client")
  }
)

#' @rdname client
setMethod(
  f = "client",
  signature = "Tomba",
  definition = function(obj, path, query = NULL) {
    res <- httr::GET(
      url   = paste0(DEFAULT_BASE_URL, path),
      query = query,
      httr::add_headers(.headers = .tomba_headers(obj)),
      httr::timeout(120),
      encode = "json"
    )
    .tomba_parse(res)
  }
)

# ---------------------------------------------------------------------------
# POST helper
# ---------------------------------------------------------------------------

#' Tomba HTTP POST Client
#'
#' Sends a POST request with a JSON body to the Tomba API.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param path Character. The API path (relative to the base URL).
#' @param data A named list to be sent as the JSON request body.
#' @return A list with the parsed API response.
#'
#' @rdname client_post
#' @export
setGeneric(
  name = "client_post",
  def  = function(obj, path, data = list()) {
    standardGeneric("client_post")
  }
)

#' @rdname client_post
setMethod(
  f = "client_post",
  signature = "Tomba",
  definition = function(obj, path, data = list()) {
    res <- httr::POST(
      url    = paste0(DEFAULT_BASE_URL, path),
      body   = data,
      httr::add_headers(.headers = .tomba_headers(obj)),
      httr::timeout(120),
      encode = "json"
    )
    .tomba_parse(res)
  }
)

# ---------------------------------------------------------------------------
# PUT helper
# ---------------------------------------------------------------------------

#' Tomba HTTP PUT Client
#'
#' Sends a PUT request with a JSON body to the Tomba API.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param path Character. The API path (relative to the base URL).
#' @param data A named list to be sent as the JSON request body.
#' @return A list with the parsed API response.
#'
#' @rdname client_put
#' @export
setGeneric(
  name = "client_put",
  def  = function(obj, path, data = list()) {
    standardGeneric("client_put")
  }
)

#' @rdname client_put
setMethod(
  f = "client_put",
  signature = "Tomba",
  definition = function(obj, path, data = list()) {
    res <- httr::PUT(
      url    = paste0(DEFAULT_BASE_URL, path),
      body   = data,
      httr::add_headers(.headers = .tomba_headers(obj)),
      httr::timeout(120),
      encode = "json"
    )
    .tomba_parse(res)
  }
)

# ---------------------------------------------------------------------------
# DELETE helper
# ---------------------------------------------------------------------------

#' Tomba HTTP DELETE Client
#'
#' Sends a DELETE request to the Tomba API.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param path Character. The API path (relative to the base URL).
#' @return A list with the parsed API response.
#'
#' @rdname client_delete
#' @export
setGeneric(
  name = "client_delete",
  def  = function(obj, path) {
    standardGeneric("client_delete")
  }
)

#' @rdname client_delete
setMethod(
  f = "client_delete",
  signature = "Tomba",
  definition = function(obj, path) {
    res <- httr::DELETE(
      url = paste0(DEFAULT_BASE_URL, path),
      httr::add_headers(.headers = .tomba_headers(obj)),
      httr::timeout(120)
    )
    .tomba_parse(res)
  }
)
