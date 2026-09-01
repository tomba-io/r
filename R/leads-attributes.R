#' @include 00-tomba-class.R
NULL

#' List Lead Attributes
#'
#' Get all lead attributes.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @return A list containing lead attributes.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- list_lead_attributes(cl)
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads-attributes#get-lead-attributes}
#' @rdname list_lead_attributes
#' @export
setGeneric(
  name = "list_lead_attributes",
  def  = function(obj) standardGeneric("list_lead_attributes")
)

#' @rdname list_lead_attributes
setMethod(
  f = "list_lead_attributes",
  signature = "Tomba",
  definition = function(obj) {
    client(obj, LEADS_ATTRIBUTES_PATH, NULL)
  }
)

#' Create Lead Attribute
#'
#' Create a new lead attribute.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param data A named list with attribute data.
#' @return A list containing the created attribute.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- create_lead_attribute(cl,
#'   data = list(name = "company_size"))
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads-attributes#create-lead-attribute}
#' @rdname create_lead_attribute
#' @export
setGeneric(
  name = "create_lead_attribute",
  def  = function(obj, data = list()) standardGeneric("create_lead_attribute")
)

#' @rdname create_lead_attribute
setMethod(
  f = "create_lead_attribute",
  signature = "Tomba",
  definition = function(obj, data = list()) {
    client_post(obj, LEADS_ATTRIBUTES_PATH, data)
  }
)

#' Update Lead Attribute
#'
#' Update a lead attribute by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param id Character. The ID of the attribute to update.
#' @param data A named list with attribute data to update.
#' @return A list containing the updated attribute.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- update_lead_attribute(cl, id = "123",
#'   data = list(name = "company_size"))
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads-attributes#update-lead-attribute}
#' @rdname update_lead_attribute
#' @export
setGeneric(
  name = "update_lead_attribute",
  def  = function(obj, id, data = list()) standardGeneric("update_lead_attribute")
)

#' @rdname update_lead_attribute
setMethod(
  f = "update_lead_attribute",
  signature = "Tomba",
  definition = function(obj, id, data = list()) {
    path <- paste0(LEADS_ATTRIBUTES_PATH, "/", id)
    client_put(obj, path, data)
  }
)

#' Delete Lead Attribute
#'
#' Delete a lead attribute by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param id Character. The ID of the attribute to delete.
#' @return A list confirming deletion.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- delete_lead_attribute(cl, id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads-attributes#delete-lead-attribute}
#' @rdname delete_lead_attribute
#' @export
setGeneric(
  name = "delete_lead_attribute",
  def  = function(obj, id) standardGeneric("delete_lead_attribute")
)

#' @rdname delete_lead_attribute
setMethod(
  f = "delete_lead_attribute",
  signature = "Tomba",
  definition = function(obj, id) {
    path <- paste0(LEADS_ATTRIBUTES_PATH, "/", id)
    client_delete(obj, path)
  }
)
