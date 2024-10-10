test_that("reading gdp lite from file works", {

  skip()

  t1 <- 
    "data-raw/EM/Ansokan_Typ_2.json" |> 
    read_gdp_lite(entity = "ansokningar")

  t2 <- 
    "data-raw/EM/FinansieradAktivitet_Typ_2.json" |> 
    read_gdp_lite(entity = "finansieradeaktiviteter")

  t3 <- 
    "data-raw/EM/Utlysning_Typ_2.json" |> 
    read_gdp_lite(entity = "utlysningar")

  is_ok <- 
    (t1$ansokningar |> nrow() == 3623) &
    (t2$finansieradeaktiviteter |> nrow() == 1672) &
    (t3$utlysningar |> nrow() == 306)

  expect_true(is_ok)

})

test_that("reading gdp lite from url works", {

  skip_on_ci()

  t1 <- 
    "https://data.bibliometrics.lib.kth.se/projects/em/proposals.json" |> 
    read_gdp_lite(entity = "ansokningar")

  t2 <- 
    "https://data.bibliometrics.lib.kth.se/projects/em/fundings.json" |> 
    read_gdp_lite(entity = "finansieradeaktiviteter")

  t3 <- 
    "https://data.bibliometrics.lib.kth.se/projects/em/calls.json" |> 
    read_gdp_lite(entity = "utlysningar")

  is_ok <- 
    (t1$ansokningar |> nrow() == 3623) &
    (t2$finansieradeaktiviteter |> nrow() == 1672) &
    (t3$utlysningar |> nrow() == 306)

  expect_true(is_ok)
    
})

test_that("exporting resources from GDP API lite can be made in duckdb single file database format", {

  skip_on_ci()

  fp <- 
    write_gdp_lite(
      url_proposals = "https://data.bibliometrics.lib.kth.se/projects/em/proposals.json",
      url_fundings = "https://data.bibliometrics.lib.kth.se/projects/em/fundings.json",
      url_calls = "https://data.bibliometrics.lib.kth.se/projects/em/calls.json"
    )
  
  is_ok <- file.exists(fp) & file.size(fp) > 0
  expect_true(is_ok)
})

# con <- duckdb::dbConnect(duckdb())
# fp <- normalizePath("data-raw/EM/proposals.json")

# DBI::dbExecute(con, "set autoinstall_known_extensions=1; set autoload_known_extensions=1;")
# res <- 
#   DBI::dbGetQuery(con, sprintf(
#     "from (from read_json_auto('%s') 
#     select unnest(ansokningar, recursive := true)) select unnest(map_keys(status)), unnest(map_values(status))", fp)) |> 
#   as_tibble()
# res$status[1]
# em_proposals |> View()