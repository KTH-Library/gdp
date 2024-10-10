gdp_lite_to_utf8 <- function(fp, encoding) {
  if (file.exists(fp))
    fp <- normalizePath(fp)

  if (missing(encoding)) {
    enc <- readr::guess_encoding(fp)$encoding[1]
    message(sprintf("Using best guess for %s regarding encoding: %s", fp, enc))
  }
  readr::read_file(fp, locale = readr::locale(encoding = enc)) |> 
    jsonlite::fromJSON(simplifyDataFrame = FALSE) |> 
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE, null = "null")
}

cols_one_to_many <- function(df) {
  name <- value <- NULL
  df |> purrr::map(\(x) class(x) == "list") |> as.list() |> 
    dplyr::as_tibble() |> 
    tidyr::pivot_longer(cols = dplyr::everything()) |> 
    dplyr::filter(value == TRUE) |> dplyr::pull(name)  
}

unn <- function(df, id = "diarienummer", mtm) {
  df |> dplyr::select(dplyr::all_of(c(id, mtm))) |> 
    tidyr::unnest(cols = c(dplyr::any_of(mtm)), names_sep = "_")
}

tbls_from_json_lite <- function(df, entity) {

  cols_mtm <- cols_one_to_many(df)
  cols_otm <- setdiff(colnames(df), cols_mtm)

  mtm <- 
    cols_mtm |> map(\(x) unn(df, mtm = x)) |> setNames(nm = sprintf("%s_%s", entity, cols_mtm))

  otm <- 
    list(df |> select(all_of(cols_otm))) |> setNames(nm = entity)

  c(otm, mtm)
  
}

write_duckdb <- function(destfile, tbls) {

  if (file.exists(destfile)) {
    message("Removing existing destfile at ", destfile)
    unlink(destfile)
  }
  
  con <- duckdb::dbConnect(duckdb(dbdir = destfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  reg <- 
    names(tbls) |> purrr::walk(\(x) duckdb::duckdb_register(
      conn = con, 
      name = paste0("view_", x), 
      df = tbls |> getElement(x))
    )

  sql_create_db <- 
    sprintf("create table %s as from %s;", names(tbls), paste0("view_", names(tbls))) |>
    paste(collapse = "\n")  

  message("Creating duckdb file at ", destfile)
  result <- DBI::dbExecute(con, sql_create_db)
  
  return(destfile)

}


#' Read from a file or url a JSON conforming to a GDP API "lite" response
#' 
#' @param url param a file path or url pointing to a GDP API "lite" resource
#' @param entity the resource type, one of "finansieradeaktiviteter", "utlysningar" or "ansokningar"
#' @returns a list with tabular parsed results
#' @export
read_gdp_lite <- function(url, entity = c("finansieradeaktiviteter", "utlysningar", "ansokningar")) {

  e <- match.arg(entity, several.ok = FALSE)
  data <- gdp_lite_to_utf8(url)

  data |> jsonlite::fromJSON() |> getElement(e) |> 
    dplyr::as_tibble() |> 
    tbls_from_json_lite(entity = e)
  
}

#' Export three GDP API "lite" resources as a duckdb database file
#' 
#' @param destfile file path to location of db, by default a file "gdp-lite.db"
#' in the temp dir
#' @param url_proposals the file or url for the location of the JSON for proposals
#' @param url_calls the file or url for the location of the JSON for calls
#' @param url_fundings the file or url for the location of the JSON for fundings
#' @export
write_gdp_lite <- function(
  destfile = file.path(tempdir(), "gdp-lite.db"), 
  url_proposals, url_calls, url_fundings) {
  
  proposals <- 
    url_proposals |> 
    read_gdp_lite(entity = "ansokningar")
  
  calls <- 
    url_calls |> 
    read_gdp_lite(entity = "utlysningar")
  
  fundings <- 
    url_fundings |> 
    read_gdp_lite(entity = "finansieradeaktiviteter")

  tbls <- c(calls, proposals, fundings) 

  destfile |> write_duckdb(tbls)

}
