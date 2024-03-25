#' Harvest data from GDP API given an organisation
#' @param organisation the identifier for the organisation
#' @return a list of tbls
#' @export
gdp_harvest <- function(organisation = "202100-3054") {

  message("Requesting all calls from GDP API")
  calls <- gdp_calls()

  if (is.null(organisation)) {

    message("Requesting fundings data from GDP API")
    fundings <- gdp_fundings()

    message("Requesting proposals/applications data from GDP API")
    proposals <- gdp_proposals()

  } else {

    message("Requesting fundings data from GDP API, using organisation filter ",
      organisation)

    fundings <-
      gdp_fundings(filter = gdp_filter(
        type = "fundings",
        org_id = organisation)
      )

    message("Requesting proposals/applications data from GDP API, using organisation filter ",
      organisation)

    proposals <-
      gdp_proposals(filter = gdp_filter(
        type = "proposals",
        org_id = organisation)
      )

  }

  message("Converting objects to rectangular/tabular data format")
  my_fundings <- fundings |> to_tbls_fundings()
  my_proposals <- proposals |> to_tbls_proposals()
  my_calls <- calls |> to_tbls_calls()

  list(
    calls = my_calls,
    fundings = my_fundings,
    proposals = my_proposals
  )

}


#' Persist data in a temporary local database and export files in CSV and parquet format
#' @param harvest the result from running the gdp_harvest function
#' @param destdir the directory where the CSV and parquet files are to be created
#' @import duckdb DBI
#' @importFrom purrr map2 walk2
#' @return path to export directory
#' @export
gdp_export_tables <- function(harvest, destdir = NULL) {

  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop(
      "Package \"duckdb\" must be installed to use this function.",
      call. = FALSE
    )
  }

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  purrr::walk2(paste0("funding_", names(harvest$fundings)), harvest$fundings,
    \(x, y) duckdb::duckdb_register(con, name = x, df = y))

  purrr::walk2(paste0("proposals_", names(harvest$proposals)), harvest$proposals,
    \(x, y) duckdb::duckdb_register(con, name = x, df = y))

  purrr::walk2(paste0("calls_", names(harvest$calls)), harvest$calls,
    \(x, y) duckdb::duckdb_register(con, name = x, df = y))

  if (is.null(destdir)) {
    destdir <- file.path(tempdir(check = TRUE), "gdp")
  }

  dest <- function(destdir, fn, ext) {
    if (!dir.exists(file.path(destdir, ext)))
      dir.create(file.path(destdir, ext), recursive = TRUE)
    file.path(destdir, ext, paste0(fn, ".", ext))
  }

  message("Preparing export to ", destdir)

  toc <- DBI::dbListTables(con)
  dests_parquet <- dest(destdir, toc, "parquet")
  dests_csv <- dest(destdir, toc, "csv")

  sql_export_parquet <-
    sprintf("COPY (SELECT * FROM %s) TO '%s' (FORMAT PARQUET);", toc, dests_parquet) |>
    paste0(collapse = "\n")

  sql_export_csv <-
    sprintf("COPY (SELECT * FROM %s) TO '%s' (FORMAT CSV);", toc, dests_csv) |>
    paste0(collapse = "\n")

  message("Exporting parquet files")
  DBI::dbExecute(con, sql_export_parquet)

  message("Exporting CSV files")
  DBI::dbExecute(con, sql_export_csv)

  message("Done, please find files in ", destdir)
  return(destdir)
}

#' Mirror files from a local directory to a S3 target path
#' @param sourcedir the location of the local directory
#' @param s3_targetdir the target spec for the S3 path, by default "kthb/projects/gdp"
#' @return exit status code from system command
#' @export
gdp_upload_files <- function(sourcedir, s3_targetdir = "kthb/projects/gdp") {

  message("Uploading files from ", sourcedir, " to ", s3_targetdir)
  cmd <- sprintf("mc mirror %s %s", sourcedir, s3_targetdir)
  system(cmd)

}

#' Export the results from a harvest as a duckdb database file
#' @param harvest the results from running the gdp_harvest function
#' @param destdir the location to save the database file
#' @return file path to the database file
#' @importFrom purrr walk2
#' @import duckdb DBI
gdp_export_database <- function(harvest, destdir = NULL) {

  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop(
      "Package \"duckdb\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (is.null(destdir)) {
    destdir <- file.path(tempdir(check = TRUE), "gdp", "gdp.db")
  }

  con <- DBI::dbConnect(duckdb::duckdb(dbdir = destdir))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  purrr::walk2(paste0("view_funding_", names(harvest$fundings)), harvest$fundings,
    \(x, y) duckdb::duckdb_register(con, name = x, df = y))

  purrr::walk2(paste0("view_proposals_", names(harvest$proposals)), harvest$proposals,
    \(x, y) duckdb::duckdb_register(con, name = x, df = y))

  purrr::walk2(paste0("view_calls_", names(harvest$calls)), harvest$calls,
    \(x, y) duckdb::duckdb_register(con, name = x, df = y))

  toc <- DBI::dbListTables(con)
  new_tbl <- gsub("^view_", "", toc)

  sql_create_db <- sprintf("create table %s as from %s;", new_tbl, toc) |>
    paste(collapse = "\n")

  message("Creating duckdb file at ", destdir)
  DBI::dbExecute(con, sql_create_db)

  return(destdir)

}

