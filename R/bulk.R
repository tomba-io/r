#' @include 00-tomba-class.R
NULL

#' List Bulk Operations
#'
#' List all bulk operations of a given type.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation. Must be one of:
#'   \code{"search"}, \code{"similar"}, \code{"company"}, \code{"finder"},
#'   \code{"enrich"}, \code{"linkedin"}, \code{"author"}, \code{"verifier"},
#'   \code{"phone-finder"}, \code{"phone-validator"}.
#' @param page Integer. Page number for pagination (optional).
#' @param limit Integer. Number of results per page (optional).
#' @return A list containing bulk operations.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- list_bulks(cl, bulk_type = "search")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulks}
#' @rdname list_bulks
#' @export
setGeneric(
  name = "list_bulks",
  def  = function(obj, bulk_type, page = NULL, limit = NULL) {
    standardGeneric("list_bulks")
  }
)

#' @rdname list_bulks
setMethod(
  f = "list_bulks",
  signature = "Tomba",
  definition = function(obj, bulk_type, page = NULL, limit = NULL) {
    .validate_bulk_type(bulk_type)
    query <- list()
    if (!is.null(page))  query$page  <- page
    if (!is.null(limit)) query$limit <- limit
    path <- paste0(BULK_PATH, "/", bulk_type)
    client(obj, path, query)
  }
)

#' Get Bulk Operation
#'
#' Get a specific bulk operation by type and ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation.
#' @param bulk_id Character or integer. The ID of the bulk operation.
#' @return A list containing bulk operation details.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- get_bulk(cl, bulk_type = "search", bulk_id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulk#get-bulk}
#' @rdname get_bulk
#' @export
setGeneric(
  name = "get_bulk",
  def  = function(obj, bulk_type, bulk_id) standardGeneric("get_bulk")
)

#' @rdname get_bulk
setMethod(
  f = "get_bulk",
  signature = "Tomba",
  definition = function(obj, bulk_type, bulk_id) {
    .validate_bulk_type(bulk_type)
    path <- paste0(BULK_PATH, "/", bulk_type, "/", bulk_id)
    client(obj, path, NULL)
  }
)

#' Create Bulk Operation
#'
#' Create a new bulk operation.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation.
#' @param data A named list with the bulk operation data.
#' @return A list containing the created bulk operation.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- create_bulk(cl, bulk_type = "finder",
#'   data = list(name = "My Bulk"))
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulk}
#' @rdname create_bulk
#' @export
setGeneric(
  name = "create_bulk",
  def  = function(obj, bulk_type, data = list()) standardGeneric("create_bulk")
)

#' @rdname create_bulk
setMethod(
  f = "create_bulk",
  signature = "Tomba",
  definition = function(obj, bulk_type, data = list()) {
    .validate_bulk_type(bulk_type)
    path <- paste0(BULK_PATH, "/", bulk_type)
    client_post(obj, path, data)
  }
)

#' Launch Bulk Operation
#'
#' Launch a bulk operation to begin processing.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation.
#' @param bulk_id Character or integer. The ID of the bulk operation.
#' @return A list confirming the launch.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- launch_bulk(cl, bulk_type = "finder", bulk_id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulk}
#' @rdname launch_bulk
#' @export
setGeneric(
  name = "launch_bulk",
  def  = function(obj, bulk_type, bulk_id) standardGeneric("launch_bulk")
)

#' @rdname launch_bulk
setMethod(
  f = "launch_bulk",
  signature = "Tomba",
  definition = function(obj, bulk_type, bulk_id) {
    .validate_bulk_type(bulk_type)
    path <- paste0(BULK_PATH, "/", bulk_type, "/", bulk_id)
    client_put(obj, path, list())
  }
)

#' Delete Bulk Operation
#'
#' Delete a bulk operation.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation.
#' @param bulk_id Character or integer. The ID of the bulk operation.
#' @return A list confirming deletion.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- delete_bulk(cl, bulk_type = "finder", bulk_id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulk}
#' @rdname delete_bulk
#' @export
setGeneric(
  name = "delete_bulk",
  def  = function(obj, bulk_type, bulk_id) standardGeneric("delete_bulk")
)

#' @rdname delete_bulk
setMethod(
  f = "delete_bulk",
  signature = "Tomba",
  definition = function(obj, bulk_type, bulk_id) {
    .validate_bulk_type(bulk_type)
    path <- paste0(BULK_PATH, "/", bulk_type, "/", bulk_id, "/delete")
    client_delete(obj, path)
  }
)

#' Archive Bulk Operation
#'
#' Archive a bulk operation.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation.
#' @param bulk_id Character or integer. The ID of the bulk operation.
#' @return A list confirming the archive.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- archive_bulk(cl, bulk_type = "finder", bulk_id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulk}
#' @rdname archive_bulk
#' @export
setGeneric(
  name = "archive_bulk",
  def  = function(obj, bulk_type, bulk_id) standardGeneric("archive_bulk")
)

#' @rdname archive_bulk
setMethod(
  f = "archive_bulk",
  signature = "Tomba",
  definition = function(obj, bulk_type, bulk_id) {
    .validate_bulk_type(bulk_type)
    path <- paste0(BULK_PATH, "/", bulk_type, "/", bulk_id, "/archive")
    client_delete(obj, path)
  }
)

#' Rename Bulk Operation
#'
#' Rename a bulk operation.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation.
#' @param bulk_id Character or integer. The ID of the bulk operation.
#' @param name Character. The new name for the bulk operation.
#' @return A list confirming the rename.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- rename_bulk(cl, bulk_type = "finder",
#'   bulk_id = "123", name = "New Name")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulk}
#' @rdname rename_bulk
#' @export
setGeneric(
  name = "rename_bulk",
  def  = function(obj, bulk_type, bulk_id, name) {
    standardGeneric("rename_bulk")
  }
)

#' @rdname rename_bulk
setMethod(
  f = "rename_bulk",
  signature = "Tomba",
  definition = function(obj, bulk_type, bulk_id, name) {
    .validate_bulk_type(bulk_type)
    path <- paste0(BULK_PATH, "/", bulk_type, "/", bulk_id, "/rename")
    client_put(obj, path, list(name = name))
  }
)

#' Bulk Progress
#'
#' Get the progress of a bulk operation.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation.
#' @param bulk_id Character or integer. The ID of the bulk operation.
#' @return A list with progress information.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- bulk_progress(cl, bulk_type = "finder", bulk_id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulk}
#' @rdname bulk_progress
#' @export
setGeneric(
  name = "bulk_progress",
  def  = function(obj, bulk_type, bulk_id) standardGeneric("bulk_progress")
)

#' @rdname bulk_progress
setMethod(
  f = "bulk_progress",
  signature = "Tomba",
  definition = function(obj, bulk_type, bulk_id) {
    .validate_bulk_type(bulk_type)
    path <- paste0(BULK_PATH, "/", bulk_type, "/", bulk_id, "/progress")
    client(obj, path, NULL)
  }
)

#' Download Bulk Results
#'
#' Download the results of a completed bulk operation.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param bulk_type Character. The type of bulk operation.
#' @param bulk_id Character or integer. The ID of the bulk operation.
#' @return A list with the download URL or data.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- download_bulk(cl, bulk_type = "finder", bulk_id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/bulk}
#' @rdname download_bulk
#' @export
setGeneric(
  name = "download_bulk",
  def  = function(obj, bulk_type, bulk_id) standardGeneric("download_bulk")
)

#' @rdname download_bulk
setMethod(
  f = "download_bulk",
  signature = "Tomba",
  definition = function(obj, bulk_type, bulk_id) {
    .validate_bulk_type(bulk_type)
    path <- paste0(BULK_PATH, "/", bulk_type, "/", bulk_id, "/download")
    client(obj, path, NULL)
  }
)

# ---------------------------------------------------------------------------
# Internal bulk helper
# ---------------------------------------------------------------------------

#' Validate bulk type parameter
#'
#' @param bulk_type Character. The bulk type to validate.
#' @return Invisible NULL. Throws an error if invalid.
#' @keywords internal
.validate_bulk_type <- function(bulk_type) {
  bt <- if (is.null(bulk_type)) "NULL" else as.character(bulk_type)
  if (is.null(bulk_type) || length(bulk_type) == 0 || !bulk_type %in% VALID_BULK_TYPES) {
    stop(
      sprintf(
        'Invalid bulk_type: "%s". Must be one of: %s',
        bt,
        paste(VALID_BULK_TYPES, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}
