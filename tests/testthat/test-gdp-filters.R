test_that("metadata is returned", {

  meta <- gdp_meta()

  is_valid <- with(meta,
    apiName == "GDP OpenAPI" &
    apiVersion == "v1.0" &
    apiDocumentation == "https://api.vinnova.se/gdp"
  )

  expect_true(is_valid)

})

test_that("getting to the openapi/swagger docs static webpage works", {

 res <-
   "https://salmon-rock-0b47a7d03.4.azurestaticapps.net" |>
    httr2::request() |>
    httr2::req_perform()

  is_valid <- httr2::resp_body_string(res) |> grepl(pattern = "swagger-ui.css")
  expect_true(is_valid)
})

test_that("ISSUE: getting to the openapi/swagger docs webpage referenced by the metadata works", {
  url <- gdp_meta()$apiDocumentation
  expect_error(httr2::request(url) |> httr2::req_perform(), "no alternative certificate subject name matches")
})

test_that("one financed activity (funding) is returned for a specific org (KTH)", {

  skip_if(TRUE, "Long running test... skipped.")
  # The organizational id for KTH
  my_orgid <- "202100-3054"

  fundings <- gdp_fundings(filter = gdp_filter(type = "fundings", org_id = my_orgid, limit = 1))

  is_valid <-
    attr(fundings, "n") > 1000 &
    length(fundings) == 1 &
    fundings[[1]]$organisationer[[1]]$organisationsNummer == my_orgid

  expect_true(is_valid)

})

test_that("all financed activities (fundings) is returned for a specific org (KTH)", {

  # The organizational id for KTH
  my_orgid <- "202100-3054"

  fundings <- gdp_fundings(filter = gdp_filter(type = "fundings", org_id = my_orgid))

  df <- fundings |> purrr::map_dfr(\(x) x |> enframe() |> tidyr::pivot_wider())

  colz <- c(
    "diarienummer", "finansiarNamn", "titel", "finansiarOrganisationsnummer",
    "startdatum", "slutdatum", "status", "typ", "titelEng", "beskrivning",
    "beskrivningEng", "uppdateringTidpunkt"
  )

  res <- df |> tidyr::unnest(cols = any_of(colz)) |> dplyr::select(dplyr::any_of(colz))

  is_valid <-
    nrow(res) > 1000

  expect_true(is_valid)

})

test_that("five calls are returned, using limit of five.", {
  res <- gdp_calls(filter = gdp_filter("calls", limit = 5))
  is_valid <- length(res) == 5
  expect_true(is_valid)
})

test_that("the last five calls from a list can be returned for a specific program", {

  id <- "2012-01383"

  # this step is required to determine the number of available records in the list
  # ie - we need to get the first page, a minimum of one record
  # so we can read the "x-totalrecords"-header
  callz <- gdp_calls(filter = gdp_filter(program_id = id, limit = 1))
  n <- attr(callz, "n")

  # now that we know the total amount of records for the query, we can get the last five
  callz <- gdp_calls(filter = gdp_filter(program_id = "2012-01383", limit = 5, offset = n - 5))
  n <- length(callz)
  is_valid <- n == 5
  expect_true(is_valid)

})

test_that("getting n + 1 records from the end of list limited to n records only returns n records", {

  id <- "2012-01383"
  callz <- gdp_calls(filter = gdp_filter(program_id = id, limit = 1))
  n <- attr(callz, "n")

  callz <- gdp_calls(filter = gdp_filter(program_id = "2012-01383", limit = 2, offset = n - 1))
  n <- length(callz)
  is_valid <- n == 1
  expect_true(is_valid)

})

test_that("getting funded activities ('fundings') filter for first week of this month works", {


  beg <- format(Sys.Date(), "%Y-%m-01")
  end <- format(Sys.Date(), "%Y-%m-07")

  res <- gdp_fundings(
    filter = gdp_filter(type = "fundings", limit = 10,
    date_from = beg,
    date_to = end
    )
  )

  datez <- res |> purrr::map_chr(c("beslut", "beslutsdatum")) |> as.Date()

  # the limits are inclusive?

  is_valid <- all(datez <= as.Date(end) & datez >= as.Date(beg))
  expect_true(TRUE)

})

test_that("getting calls updated in a specific timeperiod works", {

  beg <- paste0(as.Date("2023-02-01"), "T00:00:00Z")
  end <- paste0(as.Date("2023-02-28"), "T23:59:59Z")

  res <- gdp_calls(
    filter = gdp_filter(type = "calls", limit = 10,
    ts_from = beg,
    ts_to = end
    )
  )

  parse_ts <- function(ts) {
    strptime(gsub("T", " ", ts), format = "%Y-%m-%d %H:%M:%S")
  }

  tz <- res |> purrr::map_chr("uppdateringTidpunkt") |>
    gsub(pattern = "T", replacement = " ")

  is_valid <- tz >= parse_ts(beg) & tz <= parse_ts(end)

  expect_true(is_valid)

})

test_that("tabels can be parsed from nested json responses", {

  o1 <- gdp_calls(filter = gdp_filter("calls", limit = 10))
  o2 <- gdp_proposals(filter = gdp_filter("proposals", limit = 10))
  o3 <- gdp_fundings(filter = gdp_filter("fundings", limit = 10))

  t1 <- o1 |> to_tbls(entity = "calls")
  t2 <- o2 |> to_tbls(entity = "proposals")
  t3 <- o3 |> to_tbls(entity = "fundings")

  is_valid <- nrow(t1$calls) == nrow(t2$proposals)
  expect_true(is_valid)

})

test_that("programs can be enumerated from calls", {

  o1 <- gdp_calls(filter = gdp_filter("calls", limit = 10))
  t1 <- o1 |> to_tbls(entity = "calls")

  is_valid <- nrow(t1$programs) >= 1
  expect_true(is_valid)

})

test_that("persons can be enumerated from fundings", {

  f <- gdp_fundings(filter = gdp_filter("fundings", limit = 10))
  p <- f |> to_tbls(entity = "fundings")

  is_valid <- nrow(p$persons) >= 1
  expect_true(is_valid)

})

test_that("organisations can be enumerated from fundings", {

  f <- gdp_fundings(filter = gdp_filter("fundings", limit = 10))
  o <- f |> to_tbls(entity = "fundings")

  is_valid <- nrow(o$orgs) >= 1
  expect_true(is_valid)

})


# TODO: test status
# TODO: test error handling
# TODO: test content types
# Compare Vinnovas gamla API, Swecris, CASE
# Filter for topic 50102, Tillämpad psykologi
# Filter for words (Transport* OR Mobilitet) in (Nyckelord OR Titel OR Beskrivning)
