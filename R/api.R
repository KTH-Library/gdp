gdp_base_url <- function()
  "https://test.api.vinnova.se/gdp/v1"

gdp_config <- function() {
  list(
    token = "113d769b1f0b4f708b69855520588fea"
  )
}

#Sys.setenv(GDP_KEY = gdp_config()$token)

gdp_resources <- function() {
  list(
    proposals = "/ansokningar",
    proposal = "/ansokningar/{id}",
    calls = "/utlysningar",
    call = "/utlysningar/{id}",
    fundings = "/finansieradeaktiviteter",
    funding = "/finansieradeaktiviteter/{id}",
    metadata = "/metadata"
  )
}

gdp_api_key <- function() {

  key <- Sys.getenv("GDP_KEY")

  if (key == "") key <- gdp_config()$token

  if (!identical(key, "")) {
    return(key)
  }

  if (is_testing()) {
    return(testing_key())
  } else {
    stop("No API key found, please supply with `api_key` argument or with GDP_KEY env var")
  }
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

testing_key <- function() {
#  httr2::secret_decrypt("BWO6DHa7HPjDzpnZxAgmSPEfkRwv_b7CbHfKLYOb8Zm0nEPlo7i6MJ6WhOgvPooV", "GDP_KEY")
  httr2::secret_decrypt("oRAyG1FYJml1qabHqKDuFfpDsH4veb5mVipzqZdaijMFES5QgPZVqxCveLRTKlFe", "GDP_KEY")
}

#' @import httr
#' @import lubridate
#' @import jsonlite
#' @import tibble
#' @importFrom glue glue
gdp_request <- function(resource = gdp_resources(), verbosity = 0, id = NULL, filter = NULL, ...) {

  params <- list(
    ...
  )

  dotz <- c(filter, params)

  if (verbosity > 0) {
    if (!is.null(dotz)) message("Filter params: ", print(dotz))
  }

  if (!is.null(id))
    resource <- glue::glue(resource, id = id)

  resp <-
    gdp_base_url() |>
    httr2::request() |>
    httr2::req_url_path_append(resource) |>
    httr2::req_url_query(!!!dotz) |>
    httr2::req_headers(`Authorization` = gdp_api_key()) |>
    httr2::req_user_agent("gdp R client (https://github.com/kth-library/gdp)") |>
    httr2::req_perform(verbosity = verbosity)

  n <- httr2::resp_header(resp, header = "x-totalrecords") |> as.integer()
  invocation <- httr2::resp_header(resp, header = "x-ms-invocation-id")
  np <- httr2::resp_header(resp, header = "x-next")

  if (verbosity > 0) message("Total records: ", n)

  structure(
    resp,
    n = n,
    np = np,
    invocation = invocation
  )
}

#' Request the gdp test endpoint
#'
#' @return string with the test response in JSON
#' @export
gdp_test <- function() {
  gdp_request(gdp_resources()$calls, filter = gdp_filter(limit = 1)) |>
    parse_response()
}

parse_response <- function(resp) {

  n <- attr(resp, "n")
  invocation <- attr(resp, "invocation")

  json <-
    resp |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(simplifyDataFrame = FALSE)

  structure(
    json,
    n = n,
    invocation = invocation
  )
}

gdp_calls <- function(id = NULL, filter = gdp_filter(type = "calls")) {
  if (is.null(id))
    gdp_request(gdp_resources()$calls, filter = filter) |>
    parse_response()
  else
    gdp_request(gdp_resources()$call, id = id, filter= filter) |>
    parse_response()
}

gdp_proposals <- function(id = NULL, filter = gdp_filter(type = "proposals")) {
  if (is.null(id))
    gdp_request(gdp_resources()$proposals, filter = filter) |>
    parse_response()
  else
    gdp_request(gdp_resources()$proposal, id = id, filter= filter) |>
    parse_response()
}

gdp_fundings <- function(id = NULL, filter = gdp_filter(type = "fundings")) {
  if (is.null(id))
    gdp_request(gdp_resources()$fundings, filter = filter) |>
    parse_response()
  else
    gdp_request(gdp_resources()$funding,
      id = id,
      filter = filter) |>
    parse_response()
}

#' @importFrom rlang list2
gdp_filter <- function(type = c("calls", "proposals", "fundings"),
    offset = NULL, limit = NULL,
    program_id = NULL, call_id = NULL, org_id = NULL,
    amount_min = NULL, amount_max = NULL,
    updated_from_ts = NULL, updated_to_ts = NULL,
    decision_from_date = NULL, decision_to_date = NULL,
    status = NULL) {

  # TODO: ts_* needs to be formatted as YYYY-MM-DDTHH:MM:SSZ

  filters <- rlang::list2()

  filter_type <-
    switch(match.arg(type),
      calls = "calls",
      proposals = "proposals",
      fundings = "fundings"
    )

  if (filter_type %in% c("proposals", "fundings")) {
    filters <- rlang::list2(
      programDiarienummer = program_id,
      utlysningDiarienummer = call_id,
      Organisationsnummer = org_id,
      minBeslutadFinansieringsBelopp = amount_min,
      maxBeslutadFinansieringsBelopp = amount_max,
      franBeslutDatum = decision_from_date,
      tillBeslutDatum = decision_to_date,
      franTidpunkt = updated_from_ts,
      tillTidpunkt = updated_to_ts,
      status = status
    ) |> purrr::compact()

  } else {

    uses_valid_call_args <- all(c(
        call_id, org_id, amount_min, amount_max,
        decision_from_date, decision_to_date
      ) |> sapply(is.null))
    stopifnot(uses_valid_call_args)

    filters <- rlang::list2(
      programDiarienummer = program_id,
      franTidpunkt = updated_from_ts,
      tillTidpunkt = updated_to_ts,
      status = status
    ) |> purrr::compact()

  }

  purrr::list_assign(filters, offset = offset, limit = limit) |> purrr::compact()

}

gdp_meta <- function() {
  gdp_request(gdp_resources()$meta) |> parse_response()
}

#' @importFrom utils browseURL
gdp_docs <- function(only_swagger = FALSE, browse = TRUE) {
  url <- "https://nice-sand-09c980403.4.azurestaticapps.net"
  if (!only_swagger) url <- paste0(url, "/documentation.html")
  if (browse && interactive()) url |> browseURL()
  return (url)
}

gdp_signup <- function(browse = TRUE) {
  url <- "https://portal.test.api.vinnova.se/"
  if (browse && interactive()) url |> browseURL()
  return (url)
}

filter_unnested <- function(x)
  x[sapply(x, \(x) !is.list(x))]

#' @importFrom readr type_convert
to_tbl <- function(o)
  o |> purrr::map(filter_unnested) |>
  purrr::map_dfr(dplyr::bind_rows) |>
  readr::type_convert() |>
  suppressMessages()

#' @importFrom purrr map map_dfr
#' @importFrom tidyr unnest
#' @importFrom dplyr bind_rows bind_cols select mutate filter any_of
to_tbls <- function(entity = c("calls", "proposals", "fundings"), o) {

  # TODO: replace with calls to "to_tbls_*" instead?

  e <- match.arg(entity, c("calls", "proposals", "fundings"))
  res <- switch(e,
    calls = {
      list(
        calls = o |> to_tbl(),
        links = o |> purrr::map("lank") |> to_tbl(),
        programs = o |> purrr::map("program") |> to_tbl()
      )
    },
    proposals = {
      list(
        proposals = o |> map(filter_unnested) |> to_tbl(),
        programs = o |> map("program", filter_unnested) |> to_tbl(),
        calls = o |> map("utlysning") |> map(filter_unnested) |> to_tbl(),
        links = o |> map("lank") |> to_tbl(),
        decisions = o |> map("beslut") |> to_tbl(),
        topic = o |> map("forskningsamnen") |> map_dfr(dplyr::bind_rows),
        funder_category = o |> map("kategoriseringFinansiar") |> map_dfr(dplyr::bind_rows)
      )
    },
    fundings = {
      list(
        fundings = o |> map(filter_unnested) |> to_tbl(),
        programs = o |> map("program", filter_unnested) |> to_tbl(),
        calls = o |> map("utlysning") |> map(filter_unnested) |> to_tbl(),
        links = o |> map("lank") |> to_tbl(),
        decisions = o |> map("beslut") |> to_tbl(),
        topic = o |> map("forskningsamnen") |> map_dfr(dplyr::bind_rows),
        funder_category = o |> map("kategoriseringFinansiar") |> map_dfr(dplyr::bind_rows),
        sdgs = o |> map("hallbarhetsmal") |> map_dfr(dplyr::bind_rows),
        funding_decisions =
          o |> map("beslutadFinansiering") |> map_dfr(dplyr::bind_rows) |>
          dplyr::select(-any_of(c("finansieradOrganisation"))),
        funded_orgs = o |> map("beslutadFinansiering") |> map_dfr(dplyr::bind_rows) |>
          dplyr::pull("finansieradOrganisation") |> map_dfr(dplyr::bind_rows) |>
          tidyr::unnest(c("roll")),
        persons = o |> map("personer") |> map_dfr(dplyr::bind_rows) |> tidyr::unnest(c("roll")),
        orgs = o |> map("organisationer") |> map_dfr(dplyr::bind_rows) |> tidyr::unnest(c("roll"))
      )
    }
  )

  return(res)
}

