#' @include 00-tomba-class.R
NULL

#' List Leads
#'
#' Get all leads with optional pagination and domain filter.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param page Integer. Page number for pagination (optional).
#' @param limit Integer. Number of results per page (optional).
#' @param domain Character. Filter leads by domain (optional).
#' @return A list containing leads data.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- list_leads(cl, page = 1, limit = 10)
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads}
#' @rdname list_leads
#' @export
setGeneric(
  name = "list_leads",
  def  = function(obj, page = NULL, limit = NULL, domain = NULL) standardGeneric("list_leads")
)

#' @rdname list_leads
setMethod(
  f = "list_leads",
  signature = "Tomba",
  definition = function(obj, page = NULL, limit = NULL, domain = NULL) {
    query <- list()
    if (!is.null(page))   query$page   <- page
    if (!is.null(limit))  query$limit  <- limit
    if (!is.null(domain)) query$domain <- domain
    client(obj, LEADS_PATH, query)
  }
)

#' Get Lead
#'
#' Get a specific lead by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param lead_id Character or integer. The ID of the lead.
#' @return A list containing lead details.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- get_lead(cl, lead_id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads#retrieve-a-single-lead}
#' @rdname get_lead
#' @export
setGeneric(
  name = "get_lead",
  def  = function(obj, lead_id) standardGeneric("get_lead")
)

#' @rdname get_lead
setMethod(
  f = "get_lead",
  signature = "Tomba",
  definition = function(obj, lead_id) {
    path <- paste0(LEADS_PATH, "/", lead_id)
    client(obj, path, NULL)
  }
)

#' Create Lead
#'
#' Create a new lead.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param data A named list with lead data (e.g., email, first_name, last_name).
#' @return A list containing the created lead.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- create_lead(cl, data = list(
#'   email = "user@example.com",
#'   first_name = "John",
#'   last_name = "Doe"
#' ))
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads#create-a-lead}
#' @rdname create_lead
#' @export
setGeneric(
  name = "create_lead",
  def  = function(obj, data = list()) standardGeneric("create_lead")
)

#' @rdname create_lead
setMethod(
  f = "create_lead",
  signature = "Tomba",
  definition = function(obj, data = list()) {
    client_post(obj, LEADS_PATH, data)
  }
)

#' Update Lead
#'
#' Update an existing lead by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param lead_id Character or integer. The ID of the lead to update.
#' @param data A named list with lead data to update.
#' @return A list containing the updated lead.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- update_lead(cl, lead_id = "123",
#'   data = list(first_name = "Jane"))
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads#update-a-lead}
#' @rdname update_lead
#' @export
setGeneric(
  name = "update_lead",
  def  = function(obj, lead_id, data = list()) standardGeneric("update_lead")
)

#' @rdname update_lead
setMethod(
  f = "update_lead",
  signature = "Tomba",
  definition = function(obj, lead_id, data = list()) {
    path <- paste0(LEADS_PATH, "/", lead_id)
    client_put(obj, path, data)
  }
)

#' Delete Lead
#'
#' Delete a lead by its ID.
#'
#' @param obj A \code{\link{Tomba}} object.
#' @param lead_id Character or integer. The ID of the lead to delete.
#' @return A list confirming deletion.
#'
#' @examples
#' \dontrun{
#' cl <- Tomba(key = "ta_xxxx", secret = "ts_xxxx")
#' result <- delete_lead(cl, lead_id = "123")
#' }
#'
#' @seealso \url{https://docs.tomba.io/api/leads#delete-a-lead}
#' @rdname delete_lead
#' @export
setGeneric(
  name = "delete_lead",
  def  = function(obj, lead_id) standardGeneric("delete_lead")
)

#' @rdname delete_lead
setMethod(
  f = "delete_lead",
  signature = "Tomba",
  definition = function(obj, lead_id) {
    path <- paste0(LEADS_PATH, "/", lead_id)
    client_delete(obj, path)
  }
)
