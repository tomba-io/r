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
#' Flag an email address with a type, value, and reason.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param flag_type Character. The type of flag (e.g. "email", "domain").
#' @param value Character. The value to flag (e.g. an email address or domain).
#' @param reason Character. The reason for flagging.
#' @param comment Character. Optional comment (default \code{NULL}).
#' @return A list confirming the flag creation.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- create_flag(cl, flag_type = "email", value = "spam@example.com", reason = "spam")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/flag#create-flag}
#' @rdname create_flag
#' @export
setGeneric(
  name = "create_flag",
  def  = function(obj, flag_type, value, reason, comment = NULL) standardGeneric("create_flag")
)

#' @rdname create_flag
setMethod(
  f = "create_flag",
  signature = "Tomba",
  definition = function(obj, flag_type, value, reason, comment = NULL) {
    data <- list(flag_type = flag_type, value = value, reason = reason)
    if (!is.null(comment)) {
      data$comment <- comment
    }
    client_post(obj, FLAG_PATH, data)
  }
)
