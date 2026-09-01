#' @include 00-tomba-class.R
NULL

#' Phone Finder
#'
#' Find the phone number associated with an email address.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param email Character. The email address to look up.
#' @param webhook_url Character. Webhook URL for async results (optional, default \code{NULL}).
#' @return A list with phone data including local_format, intl_format,
#'   country_code, line_type, carrier, and timezones.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- phone_finder(cl, email = "user@example.com")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/phone#phone-finder}
#' @rdname phone_finder
#' @export
setGeneric(
 name = "phone_finder",
 def  = function(obj, email, webhook_url = NULL) standardGeneric("phone_finder")
)

#' @rdname phone_finder
setMethod(
  f = "phone_finder",
  signature = "Tomba",
  definition = function(obj, email, webhook_url = NULL) {
    query <- list(email = email)
    if (!is.null(webhook_url)) query$webhook_url <- webhook_url
    client(obj, PHONE_PATH, query)
  }
)

#' Phone Validator
#'
#' Validate a phone number and retrieve metadata.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param phone Character. The phone number to validate.
#' @return A list with validation data including local_format, intl_format,
#'   country_code, line_type, carrier, and timezones.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- phone_validator(cl, phone = "+1234567890")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/phone#phone-validator}
#' @rdname phone_validator
#' @export
setGeneric(
  name = "phone_validator",
  def  = function(obj, phone, country_code = NULL) standardGeneric("phone_validator")
)

#' @rdname phone_validator
setMethod(
  f = "phone_validator",
  signature = "Tomba",
  definition = function(obj, phone, country_code = NULL) {
    query <- list(phone = phone)
    if (!is.null(country_code)) query$country_code <- country_code
    client(obj, PHONE_VALIDATOR_PATH, query)
  }
)
