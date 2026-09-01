#' @include 00-tomba-class.R
NULL

#' List Leads Lists
#'
#' Get all leads lists.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @return A list containing leads lists.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- list_leads_lists(cl)
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads-lists#get-leads-lists}
#' @rdname list_leads_lists
#' @export
setGeneric(
  name = "list_leads_lists",
  def  = function(obj) standardGeneric("list_leads_lists")
)

#' @rdname list_leads_lists
setMethod(
  f = "list_leads_lists",
  signature = "Tomba",
  definition = function(obj) {
    client(obj, LEADS_LISTS_PATH, NULL)
  }
)

#' Create Leads List
#'
#' Create a new leads list.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param data A named list with list data (e.g., \code{list(name = "My List")}).
#' @return A list containing the created leads list.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- create_leads_list(cl, data = list(name = "Prospects"))
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/lead-lists#create-leads-list}
#' @rdname create_leads_list
#' @export
setGeneric(
  name = "create_leads_list",
  def  = function(obj, data = list()) standardGeneric("create_leads_list")
)

#' @rdname create_leads_list
setMethod(
  f = "create_leads_list",
  signature = "Tomba",
  definition = function(obj, data = list()) {
    client_post(obj, LEADS_LISTS_PATH, data)
  }
)

#' Update Leads List
#'
#' Update a leads list by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param id Character. The ID of the leads list to update.
#' @param data A named list with list data to update.
#' @return A list containing the updated leads list.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- update_leads_list(cl, id = "123",
#'   data = list(name = "Updated List"))
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/lead-lists#update-leads-list}
#' @rdname update_leads_list
#' @export
setGeneric(
  name = "update_leads_list",
  def  = function(obj, id, data = list()) standardGeneric("update_leads_list")
)

#' @rdname update_leads_list
setMethod(
  f = "update_leads_list",
  signature = "Tomba",
  definition = function(obj, id, data = list()) {
    path <- paste0(LEADS_LISTS_PATH, "/", id)
    client_put(obj, path, data)
  }
)

#' Delete Leads List
#'
#' Delete a leads list by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param id Character. The ID of the leads list to delete.
#' @return A list confirming deletion.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- delete_leads_list(cl, id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/lead-lists#delete-leads-list}
#' @rdname delete_leads_list
#' @export
setGeneric(
  name = "delete_leads_list",
  def  = function(obj, id) standardGeneric("delete_leads_list")
)

#' @rdname delete_leads_list
setMethod(
  f = "delete_leads_list",
  signature = "Tomba",
  definition = function(obj, id) {
    path <- paste0(LEADS_LISTS_PATH, "/", id)
    client_delete(obj, path)
  }
)
