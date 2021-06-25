#' Create a new Mem
#'
#' This function creates a new Mem and adds it to your Inbox.
#'
#' You'll be able to pass a character vector that will be displayed in Mem. This
#' vector can be formatted with markdown styling and this will be interpreted
#' and formatted by Mem. The markdown format supported by Mem can by found on
#' their [documentation](https://docs.mem.ai/docs/general/mem-markdown-format)
#'
#'
#' ## API Token
#' You'll need to create an API token in order to create a Mem. To do this,
#' visit the [API Flow](https://mem.ai/flows/api) within the Mem app. From this page, you'll be able to
#' generate a new API access token for yourself.
#'
#'
#' @param content character string, The contents of the mem. The string should be in a markdown-compatible format. For more details, see the Mem Markdown Format.
#' @param API_KEY an API key
#' @param api_version (optional) The API version you'd like to use, defaults to "v0" which is the only version of the Mem API. Will be expanded as the API develops more
#' @param isRead (optional) logical, Indicates whether the mem should be automatically marked as "read" (unread mems are highlighted within the default views in the product UI). Defaults to FALSE
#' @param isArchived (optional) Indicates whether the mem should be automatically marked as "archived" (archived mems are hidden from the default views in the product UI). Defaults to false.
#' @param scheduledFor (optional) Specify a time that this mem will resurface at (similar to the "snooze" button in the product UI). This is a timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
#' @param createdAt (optional) Specify the time that the mem was created at. Defaults to the current time. This is a timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
#'
#' @return
#' @export
#'
#' @examples
create_mem <- function(content, API_KEY,
                       api_version = "v0",
                       isRead = NULL, isArchived = NULL,
                       scheduledFor = NULL, createdAt = NULL) {

  # check arguments
  if(!is.null(isRead) & !is.logical(isRead)) stop(rlang::abort("`isRead` should be logical"))
  if(!is.null(isArchived) & !is.logical(isArchived)) stop(rlang::abort("`isArchived` should be logical"))

  ua <- httr::user_agent("https://github.com/jimmyday12/memr")

  url <- paste0("https://api.mem.ai/", api_version, "/mems")

  body <- list(
    content = content,
    isRead = isRead,
    isArchived = isArchived,
    scheduledFor = scheduledFor,
    createdAt = createdAt
  )

  resp <- httr::POST(url,
    ua,
    body = body,
    encode = "json",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("ApiAccessToken", API_KEY)
    )
  )

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(
      paste(
        "Mem API request failed:",
        httr::status_code(resp),
        "-",
        parsed$error$message
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      response = resp
    ),
    class = "mem_api"
  )
}
