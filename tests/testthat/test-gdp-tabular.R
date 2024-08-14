test_that("getting one funding data record as a table works", {

  # curl -X 'GET' \
  # 'https://test.api.vinnova.se/gdp/v1/finansieradeaktiviteter/2014-02560' \
  # -H 'accept: application/json' \
  # -H 'Authorization: 113d769b1f0b4f708b69855520588fea'

  record <- gdp_fundings(id = "2014-02560")

  # NB: If only one record is returned, it is not wrapped in a list...

  tbl <- list(record) |> to_tbls_fundings()

  is_valid <- nrow(tbl$fundings) == 1

  expect_true(is_valid)

})

test_that("getting funding data records for one organisation id as a table works", {

  records <- gdp_fundings(filter = gdp_filter(
    type = "funding",
    org_id = "202100-3054", # KTH
    limit = 1
  ))

  tbls <- records |> to_tbls_fundings()

  is_valid <- nrow(tbls$fundings) >= 1

  expect_true(is_valid)

})

test_that("getting one proposal record as a table works", {

  # curl -X 'GET' \
  # 'https://test.api.vinnova.se/gdp/v1/ansokningar/2015-00438' \
  # -H 'accept: application/json' \
  # -H 'Authorization: 113d769b1f0b4f708b69855520588fea'

  p <- gdp_proposals(id = "2015-00438")

  tbl <- list(p) |> to_tbls_proposals()

  is_valid <- nrow(tbl$proposals) == 1

  expect_true(is_valid)

})

test_that("getting some proposals for a specific organisation as tables works", {

  ps <- gdp_proposals(filter = gdp_filter(
    type = "proposals",
    org_id = "202100-3054",
    limit = 100)
  )

  #ps[[1]]$personer |> toJSON(pretty = TRUE, auto_unbox = TRUE)
  #ps[[1]]$organisationer |> toJSON(pretty = TRUE, auto_unbox = TRUE)

  tbls <- ps |> to_tbls_proposals()

  is_valid <-
    purrr::map_lgl(tbls, \(x) nrow(x) >= 10) |>
    as.vector() |> all()

  expect_true(is_valid)
})


test_that("getting information about one call works", {

  c1 <- gdp_calls(id = "2018-05327")

  tbl <- list(c1) |> to_tbls_calls()

  is_valid <- nrow(tbl$calls) == 1 & nrow(tbl$links) == 1

  expect_true(is_valid)
})

test_that("getting information about several calls works", {

  n <- 10
  c1 <- gdp_calls(filter = gdp_filter(type = "calls", limit = n))
  tbl <- c1 |> to_tbls_calls()

  is_valid <- nrow(tbl$calls) == n & nrow(tbl$links) == n

  expect_true(is_valid)
})

