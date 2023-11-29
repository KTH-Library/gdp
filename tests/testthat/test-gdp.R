library(swecris)
library(tidyr)
library(lubridate)
library(dplyr)
library(waldo)

test_that("fundings data match (swecris vs gdp)", {
  # fundings for KTH

  swecris_kth_funding <- swecris_funding("KTH")

  # TODO: get GDP data here
  # gdp_kth_funding <- gdp_funding("KTH")
  gdp_kth_funding <- swecris_kth_funding

  is_equal <- length(compare(swecris_kth_funding, gdp_kth_funding)) == 0
  expect_true(is_equal)
})

test_that("fundings data for a specific research area matches (swecris vs gdp)", {

  orgs <- swecris_organisations()
  id <- orgs[which(grepl("KTH", orgs$organisationNameSv)), ][1,]
  swecris_kth_projects <- swecris_projects(id$organisationId)

  research_area <- "Teknik"

  a <-
    swecris_kth_projects |>
    unnest_wider("scbs", simplify = TRUE, names_sep = "_") |>
    select(-paste0("scbs_", 2:5)) |>
    unnest_longer("scbs_1", values_to = "scb_vals", indices_to = "scb_ind") |>
    filter(scb_vals == research_area)

  b <- # TODO: get data from GDP api
    a #|> bind_rows(a[1,])

  is_equal <- length(compare(a, b)) == 0

  expect_true(is_equal)

})

test_that("fundings data for a given coordinating entity matches (swecris vs gdp)", {

  orgs <- swecris_organisations()
  id <- orgs[which(grepl("KTH", orgs$organisationNameSv)), ][1,]
  swecris_kth_projects <- swecris_projects(id$organisationId)

  a <-
    swecris_kth_projects |>
    filter(coordinatingOrganisationId %in% id$organisationId)

  b <- # TODO: get data from GDP api
    a #|> bind_rows(a[1,])

  is_equal <- length(compare(a, b)) == 0

  expect_true(is_equal)

})

test_that("fundings data related to a given coordinator matches (swecris vs gdp)", {

  orgs <- swecris_organisations()
  id <- orgs[which(grepl("KTH", orgs$organisationNameSv)), ][1,]
  swecris_kth_projects <- swecris_projects(id$organisationId)

  # projects for both a specific PI and one co-investigator
  a <-
    swecris_kth_projects |>
    unnest_longer(col = peopleList, values_to = "p", indices_to = "p_ind") |>
    unnest_wider(col = "p", names_sep = "_") |>
    filter(p_roleEn %in% c("Principal Investigator", "Co-Investigator")) |>
    filter(p_fullName %in% c("Anders Ansell", "Mats Nilsson"))

  # specifically per role
  # pi <- a |> filter(p_roleEn == "Principal Investigator")
  # co <- a |> filter(p_roleEn == "Co-Investigator")

  b <- # TODO: get data from GDP api
    a #|> bind_rows(a[1,])

  is_equal <- length(compare(a, b)) == 0

  expect_true(is_equal)

})


test_that("data related to ongoing financed activities (swecris vs gdp)", {


  orgs <- swecris_organisations()
  id <- orgs[which(grepl("KTH", orgs$organisationNameSv)), ][1,]
  swecris_kth_projects <- swecris_projects(id$organisationId)

  # cannot find a VTI-specific API, uses Swecris API for now
  # cannot find a field with status flags like "ongoing", "finished" etc

  # assumptions:
  # ongoing: project has begun and has not yet ended
  a <-
    swecris_kth_projects |>
    mutate(
      beg = lubridate::as_date(strptime(projectStartDate, format = "%Y-%m-%d %H:%M:%S")),
      end = lubridate::as_date(strptime(projectEndDate, format = "%Y-%m-%d %H:%M:%S"))) |>
    filter(beg <= Sys.Date() & end <= Sys.Date())

  b <- # TODO: get data from GDP api
    a #|> bind_rows(a[1,])

  is_equal <- length(compare(a, b)) == 0

  expect_true(is_equal)

})


#Filtrera uttaget med Forskningsämne 50102, Tillämpad psykologi
#Filtrera uttaget med orden (Transport* OR Mobilitet) i (Nyckelord OR Titel OR Beskrivning)

test_that("Test endpoint returns JSON", {
  res <- gdp::gdp_test()
  df <- jsonlite::fromJSON(res, simplifyDataFrame = TRUE) |> tibble::as_tibble()
  is_valid <- nrow(df) == 1 & ncol(df) == 23
  expect_true(is_valid)
})
