#' @include 00-tomba-class.R
NULL

#' Email Verifier
#'
#' Verify the deliverability of an email address.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param email Character. The email address to verify.
#' @param webhook_url Character. Webhook URL for async results (optional, default \code{NULL}).
#' @return A list with verification data including MX records, SMTP check,
#'   deliverability score, and status information.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- email_verifier(cl, email = "info@tomba.io")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/verifier#email-verifier}
#' @rdname email_verifier
#' @export
setGeneric(
  name = "email_verifier",
  def  = function(obj, email, webhook_url = NULL) standardGeneric("email_verifier")
)

#' @rdname email_verifier
setMethod(
  f = "email_verifier",
  signature = "Tomba",
  definition = function(obj, email, webhook_url = NULL) {
    query <- list(email = email)
    if (!is.null(webhook_url)) query$webhook_url <- webhook_url
    client(obj, VERIFIER_PATH, query)
  }
)
