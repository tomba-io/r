#' @include 00-tomba-class.R
NULL

#' List API Keys
#'
#' Get all API keys associated with your account.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @return A list containing API keys.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- list_keys(cl)
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/keys#get-keys}
#' @rdname list_keys
#' @export
setGeneric(
  name = "list_keys",
  def  = function(obj) standardGeneric("list_keys")
)

#' @rdname list_keys
setMethod(
  f = "list_keys",
  signature = "Tomba",
  definition = function(obj) {
    client(obj, KEYS_PATH, NULL)
  }
)

#' Get API Key
#'
#' Get a specific API key by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param id Character. The ID of the API key.
#' @return A list containing the API key details.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- get_key(cl, id = "key-id-here")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/keys#get-keys}
#' @rdname get_key
#' @export
setGeneric(
  name = "get_key",
  def  = function(obj, id) standardGeneric("get_key")
)

#' @rdname get_key
setMethod(
  f = "get_key",
  signature = "Tomba",
  definition = function(obj, id) {
    path <- paste0(KEYS_PATH, "/", id)
    client(obj, path, NULL)
  }
)

#' Create API Key
#'
#' Create a new API key.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param data A named list with key data.
#' @return A list containing the created API key.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- create_key(cl, data = list())
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/keys#create-an-api-key}
#' @rdname create_key
#' @export
setGeneric(
  name = "create_key",
  def  = function(obj, data = list()) standardGeneric("create_key")
)

#' @rdname create_key
setMethod(
  f = "create_key",
  signature = "Tomba",
  definition = function(obj, data = list()) {
    client_post(obj, KEYS_PATH, data)
  }
)

#' Delete API Key
#'
#' Delete an API key by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param id Character. The ID of the API key to delete.
#' @return A list confirming deletion.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- delete_key(cl, id = "key-id-here")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/keys#delete-an-api-key}
#' Reset API Key
#'
#' Reset an API key by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param id Character. The ID of the API key to reset.
#' @return A list containing the reset key.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- reset_key(cl, id = "key-id-here")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/keys#reset-an-api-key}
#' @rdname reset_key
#' @export
setGeneric(
  name = "reset_key",
  def  = function(obj, id) standardGeneric("reset_key")
)

#' @rdname reset_key
setMethod(
  f = "reset_key",
  signature = "Tomba",
  definition = function(obj, id) {
    path <- paste0(KEYS_PATH, "/", id)
    client_put(obj, path, list())
  }
)

#' @rdname delete_key
#' @export
setGeneric(
  name = "delete_key",
  def  = function(obj, id) standardGeneric("delete_key")
)

#' @rdname delete_key
setMethod(
  f = "delete_key",
  signature = "Tomba",
  definition = function(obj, id) {
    path <- paste0(KEYS_PATH, "/", id)
    client_delete(obj, path)
  }
)
