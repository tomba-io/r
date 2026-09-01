#' @include 00-tomba-class.R
NULL

#' List Flags
#'
#' Get all flagged email addresses.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param page Integer. Page number for pagination (optional).
#' @param limit Integer. Number of results per page (optional).
#' @return A list containing flagged emails.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- list_flags(cl)
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/flag#list-flags}
#' @rdname list_flags
#' @export
setGeneric(
  name = "list_flags",
  def  = function(obj, page = NULL, limit = NULL) standardGeneric("list_flags")
)

#' @rdname list_flags
setMethod(
  f = "list_flags",
  signature = "Tomba",
  definition = function(obj, page = NULL, limit = NULL) {
    query <- list()
    if (!is.null(page))  query$page  <- page
    if (!is.null(limit)) query$limit <- limit
    client(obj, FLAG_PATH, query)
  }
)

#' Create Flag
#'
#' Flag an email address, optionally with a reason.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param email Character. The email address to flag.
#' @param reason Character. Optional reason for flagging (default \code{NULL}).
#' @return A list confirming the flag creation.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- create_flag(cl, email = "spam@example.com", reason = "spam")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/flag#create-flag}
#' @rdname create_flag
#' @export
setGeneric(
  name = "create_flag",
  def  = function(obj, email, reason = NULL) standardGeneric("create_flag")
)

#' @rdname create_flag
setMethod(
  f = "create_flag",
  signature = "Tomba",
  definition = function(obj, email, reason = NULL) {
    data <- list(email = email)
    if (!is.null(reason)) {
      data$reason <- reason
    }
    client_post(obj, FLAG_PATH, data)
  }
)
