gdp_base_url <- function()
  "https://utv.api.vinnova.se/ansokningar/v1"

gdp_config <- function() {
  list(
    token = "cbab271c3107463ab77860ae3e688a22"
  )
}

#Sys.setenv(GDP_KEY = gdp_config()$token)

gdp_resources <- function() list(proposals = "/ansokningar")

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
  httr2::secret_decrypt("BWO6DHa7HPjDzpnZxAgmSPEfkRwv_b7CbHfKLYOb8Zm0nEPlo7i6MJ6WhOgvPooV", "GDP_KEY")
}

#' @import httr
#' @import lubridate
#' @import jsonlite
#' @import tibble
gdp_request <- function(resource = gdp_resources(), ...) {

  params <- list(
    ...
  )

  gdp_base_url() |>
    httr2::request() |>
    httr2::req_url_path_append(resource) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`api-nyckel` = gdp_api_key()) |>
    httr2::req_user_agent("gdp R client (https://github.com/kth-library/gdp)") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

#' Request the gdp test endpoint
#'
#' @return string with the test response in JSON
#' @export
gdp_test <- function() {
  gdp_request() |> jsonlite::toJSON(pretty = TRUE)
}

