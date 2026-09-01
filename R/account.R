#' @include 00-tomba-class.R
NULL

#' Account Information
#'
#' Returns information about the current account.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @return A list with account data including pricing, name, email, country,
#'   and request usage for domain search, email finder, and email verifier.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- account(cl)
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/account#get-account}
#' @rdname account
#' @export
setGeneric(
  name = "account",
  def  = function(obj) standardGeneric("account")
)

#' @rdname account
setMethod(
  f = "account",
  signature = "Tomba",
  definition = function(obj) {
    client(obj, ACCOUNT_PATH, NULL)
  }
)
