
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdp

<!-- badges: start -->

[![R-CMD-check](https://github.com/KTH-Library/gdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KTH-Library/gdp/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of gdp is to provide some tools and resources that are usefull
when working with data from the GDP project.

## Installation

You can install the development version of gdp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KTH-Library/gdp")
```

## Examples

This is an example showing how to convert data from the GDP API to
rectangular data which can be persisted locally in a relational
database:

``` r
library(gdp)

# use filters to extract data

my_orgid <- "202100-3054"  # KTHs organisation id used in GDP

fundings <- gdp_fundings(filter = gdp_filter(type = "fundings", org_id = my_orgid))
proposals <- gdp_proposals(filter = gdp_filter(type = "proposals", org_id = my_orgid))
calls <- gdp_calls()

# convert results to tabular (relational) data format
my_fundings <- fundings |> to_tbls_fundings()
my_proposals <- proposals |> to_tbls_proposals() 
my_calls <- calls |> to_tbls_calls()

# persist data in a local database

library(duckdb)
library(dbplyr)

con <- DBI::dbConnect(duckdb::duckdb())

purrr::map2(paste0("funding_", names(my_fundings)), my_fundings, 
  \(x, y) duckdb::duckdb_register(con, name = x, df = y))

purrr::map2(paste0("proposals_", names(my_proposals)), my_proposals, 
  \(x, y) duckdb::duckdb_register(con, name = x, df = y))

purrr::map2(paste0("calls_", names(my_calls)), my_calls, 
  \(x, y) duckdb::duckdb_register(con, name = x, df = y))

toc <- dbListTables(con)
dest <- sprintf("~/repos/gdp/inst/markdown/db/%s.parquet", toc)

sql <- 
  sprintf("COPY (SELECT * FROM %s) TO '%s' (FORMAT PARQUET);", toc, dest) |> 
  paste0(collapse = "\n")

dbExecute(con, sql)

dbDisconnect(con)

# sync database to remote

system("mc mirror ~/repos/gdp/inst/markdown/db kthb/projects/gdp")
```

This example shows how to search for proposals/applications across a few
different topic areas:

``` r
library(gdp)
library(tidyverse)

# use filters to extract data

my_orgid <- "202100-3054"  # KTHs organisation id used in GDP

my_proposals <- 
  gdp_proposals(filter = gdp_filter(type = "proposals", org_id = my_orgid)) |> 
  to_tbls_proposals()


# proposals related to transport systems and logistics or has code which begins w "2.1.04"
my_proposals$topics |> 
  filter(topic_name_eng == "Transport Systems and Logistics" | 
    grepl("^2[.]1[.]0[4]", topic_code)) |> 
  distinct(id_proposal, topic_code, topic_name_eng) |> 
  left_join(my_proposals$proposals) |> 
  select(id_proposal, title, req_amount, topic_code) |> 
  arrange(desc(req_amount))
```
