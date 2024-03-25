mapping_cols_call <- function(x) {
  list(
    id_call = "diarienummer",
    funding_orgname = "finansiarNamn",
    funding_orgid = "finansiarOrganisationsnummer",
    title = "titel",
    title_eng = "titelEng",
    desc = "beskrivning",
    desc_eng = "beskrivningEng",
    amount = "budgetBelopp",
    currency = "budgetValuta",
    status = "status",
    date_open = "oppningsdatum",
    date_close = "stangningsdatum",
    updated_ts = "uppdateringTidpunkt"
  )
}

mapping_cols_program <- function(x) {
  list(
    id_program = "diarienummer",
    funding_orgname = "finansiarNamn",
    funding_orgid = "finansiarOrganisationsnummer",
    title = "titel",
    desc = "beskrivning",
    amount = "budgetBelopp",
    currency = "budgetValuta",
    date_beg = "startdatum",
    date_end = "slutdatum",
    status = "status",
    updated_ts = "uppdateringTidpunkt"
  )
}

mapping_cols_link <- function(x) {
  list(
    href = "href",
    rel = "rel",
    method = "method",
    title_eng = "titelEng"
  )
}

mapping_cols_proposal <- function(x) {
  list(
    id_proposal = "diarienummer",
    funding_orgname = "finansiarNamn",
    funding_orgid = "finansiarOrganisationsnummer",
    title = "titel",
    title_eng = "titelEng",
    status = "status",
    req_amount = "soktBelopp",
    req_currency = "soktBeloppValuta",
    desc = "beskrivning",
    desc_eng = "beskrivningEng",
    updated_ts = "uppdateringTidpunkt"
  )
}

mapping_cols_org <- function(x) {
  list(
    org_name = "namn",
    id_org = "organisationsNummer",
    country = "land",
    county = "lan",
    role_org = "roll"
  )
}

mapping_cols_person <- function(x) {
  list(
    fullname = "namn",
    gender = "kon",
    role_person = "roll"
  )
}

mapping_cols_decision <- function(x) {
  list(
    decision_date = "beslutsdatum",
    decision_type = "beslutstyp",
    decision_outcome = "utfall"
  )
}

mapping_cols_topic <- function(x) {
  list(
    topic_code = "kod",
    topic_name = "namn",
    topic_name_eng = "namnEng"
  )
}

mapping_cols_category <- function(x) {
  list(
    category_name = "namn"
  )
}

mapping_cols_funding <- function(x) {
  list(
    funding_orgname = "finansiarNamn",
    funding_orgid = "finansiarOrganisationsnummer",
    funding_type = "typ",
    title = "titel",
    desc = "beskrivning",
    desc_eng = "beskrivningEng",
    date_beg = "startdatum",
    date_end = "slutdatum",
    status = "status",
    updated_ts = "uppdateringTidpunkt",
    sdg = "hallbarhetsmal"
  )
}

mapping_cols_payout <- function(x) {
  list(
    receiver_name = "namn",
    receiver_orgid = "organisationsNummer",
    receiver_role = "roll",
    receiver_country = "land",
    receiver_county = "lan",
    payout_year = "ar",
    payout_amount = "belopp",
    payout_currency = "valuta",
    payout_basis = "stodgrund"
  )
}

mapping_cols_sdg <- function(x) {
  list(
    sdg_name = "namn",
    sdg_name_eng = "namnEng"
  )
}


#' @importFrom dplyr mutate left_join select pull
#' @importFrom tibble tibble
cols_rename <- function(entity = c(
    "call", "program", "link", "proposal",
    "org", "person", "decision",
    "topic", "category",
    "funding", "payout",
    "sdg"
  ), fields) {

  old <- new <- NULL

  lookup <- switch(match.arg(entity),
    call = mapping_cols_call(fields),
    program = mapping_cols_program(fields),
    link = mapping_cols_link(fields),
    proposal = mapping_cols_proposal(fields),
    org = mapping_cols_org(fields),
    person = mapping_cols_person(fields),
    decision = mapping_cols_decision(fields),
    topic = mapping_cols_topic(fields),
    category = mapping_cols_category(fields),
    funding = mapping_cols_funding(fields),
    payout = mapping_cols_payout(fields),
    sdg = mapping_cols_sdg(fields)
  )

  tibble::tibble(old = fields) |>
    dplyr::left_join(tibble(new = names(lookup), old = unlist(lookup)), by = "old") |>
    dplyr::mutate(new = ifelse(is.na(new), old, new)) |>
    dplyr::pull("new")

}

#' @importFrom purrr transpose
#' @importFrom tibble as_tibble
#' @importFrom dplyr all_of rename_with
#' @importFrom tidyr unnest
#' @importFrom readr type_convert
to_tbls_calls <- function(x) {

  diarienummer <- lank <- program <- NULL

  one_to_many <- c(
    "lank", "program"
  )

  one_to_one <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    tidyr::unnest(-dplyr::all_of(one_to_many)) |>
    dplyr::select(-dplyr::all_of(one_to_many)) |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename(id_call = "diarienummer") |>
    dplyr::rename_with(cols_rename, entity = "call")

  links <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
      dplyr::mutate(lank = purrr::pmap(list(diarienummer, lank), \(a, b) c(id_call = a, b))) |>
      dplyr::pull("lank") |> dplyr::bind_rows() |>
      readr::type_convert() |> suppressMessages() |>
      dplyr::rename_with(cols_rename, entity = "link")

  programs <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(program = purrr::pmap(list(diarienummer, program), \(a, b) c(id_call = a, b))) |>
    dplyr::pull("program") |> dplyr::bind_rows() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "program")

  list(calls = one_to_one, links = links, programs = programs)

}

to_tbls_proposals <- function(x) {

  diarienummer <- lank <- program <- organisationer <- personer <-
    beslut <- forskningsamne <- kategoriseringfinansiar <-
    utlysning <- NULL

  one_to_many <- c(
    "lank", "program",
    "organisationer", "personer",
    "beslut",
    "forskningsamne",
    "kategoriseringfinansiar",
    "utlysning"
  )

  one_to_one <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    tidyr::unnest(-dplyr::all_of(one_to_many)) |>
    dplyr::select(-dplyr::all_of(one_to_many)) |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename(id_proposal = "diarienummer") |>
    dplyr::rename_with(cols_rename, entity = "proposal")

  links <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
      dplyr::mutate(lank = purrr::pmap(list(diarienummer, lank), \(a, b) c(id_proposal = a, b))) |>
      dplyr::pull("lank") |> dplyr::bind_rows() |>
      readr::type_convert() |> suppressMessages() |>
      dplyr::rename_with(cols_rename, entity = "link")

  programs <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
      dplyr::mutate(program = purrr::pmap(list(diarienummer, program), \(a, b) c(id_proposal = a, b))) |>
      dplyr::pull("program") |> dplyr::bind_rows() |>
      readr::type_convert() |> suppressMessages() |>
      dplyr::rename_with(cols_rename, entity = "program")

  orgs <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(org = purrr::pmap(list(diarienummer, organisationer), \(a, b) c(id_proposal = a, bind_rows(b)))) |>
    dplyr::pull("org") |> dplyr::bind_rows() |>
    tidyr::unnest("roll") |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "org")

  persons <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(person = purrr::pmap(list(diarienummer, personer), \(a, b) c(id_proposal = a, bind_rows(b)))) |>
    dplyr::pull("person") |> dplyr::bind_rows() |>
    tidyr::unnest("roll") |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "person")

  decisions <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(decision = purrr::pmap(list(diarienummer, beslut), \(a, b) c(id_proposal = a, bind_rows(b)))) |>
    dplyr::pull("decision") |> dplyr::bind_rows() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "decision")

  topics <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(topic = purrr::pmap(list(diarienummer, forskningsamne), \(a, b) c(id_proposal = a, bind_rows(b)))) |>
    dplyr::pull("topic") |> dplyr::bind_rows() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "topic")

  categories <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(categories = purrr::pmap(list(diarienummer, kategoriseringfinansiar), \(a, b) c(id_proposal = a, bind_rows(b)))) |>
    dplyr::pull("categories") |> dplyr::bind_rows() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "category")

  calls <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(calls = purrr::pmap(list(diarienummer, utlysning), \(a, b) c(id_proposal = a, b))) |>
    dplyr::pull("calls") |> dplyr::bind_rows() |>
    dplyr::select(-c("lank")) |> dplyr::distinct() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "call")

    # "utlysning"


  list(
    proposals = one_to_one, links = links, programs = programs,
    organisations = orgs, persons = persons, decisions = decisions,
    topics = topics, categories = categories, calls = calls
  )

}

to_tbls_fundings <- function(x) {

  diarienummer <- lank <- program <- organisationer <-
    forskningsamne <- kategoriseringfinansiar <-
    utlysning <- beslutadFinansiering <-
    belopp <- hallbarhetsmal <- beslut <- personer <- NULL

  one_to_many <- c(
    "lank", "program",
    "organisationer", "personer",
    "beslut",
    "beslutadFinansiering",
    "forskningsamne",
    "kategoriseringfinansiar",
    "utlysning",
    "hallbarhetsmal"
  )

  one_to_one <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    tidyr::unnest(-dplyr::all_of(one_to_many)) |>
    dplyr::select(-dplyr::all_of(one_to_many)) |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename(id_funding = "diarienummer") |>
    dplyr::rename_with(cols_rename, entity = "funding")

  links <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
      dplyr::mutate(lank = purrr::pmap(list(diarienummer, lank), \(a, b) c(id_funding = a, b))) |>
      dplyr::pull("lank") |> dplyr::bind_rows() |>
      readr::type_convert() |> suppressMessages() |>
      dplyr::rename_with(cols_rename, entity = "link")

  programs <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
      dplyr::mutate(program = purrr::pmap(list(diarienummer, program), \(a, b) c(id_funding = a, b))) |>
      dplyr::pull("program") |> dplyr::bind_rows() |>
      readr::type_convert() |> suppressMessages() |>
      dplyr::rename_with(cols_rename, entity = "program")

  orgs <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(org = purrr::pmap(list(diarienummer, organisationer), \(a, b) c(id_funding = a, bind_rows(b)))) |>
    dplyr::pull("org") |> dplyr::bind_rows() |>
    tidyr::unnest("roll") |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "org")

  persons <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(person = purrr::pmap(list(diarienummer, personer), \(a, b) c(id_funding = a, bind_rows(b)))) |>
    dplyr::pull("person") |> dplyr::bind_rows() |>
    tidyr::unnest("roll") |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "person")

  decisions <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(decision = purrr::pmap(list(diarienummer, beslut), \(a, b) c(id_funding = a, bind_rows(b)))) |>
    dplyr::pull("decision") |> dplyr::bind_rows() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "decision")

  topics <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(topic = purrr::pmap(list(diarienummer, forskningsamne), \(a, b) c(id_funding = a, bind_rows(b)))) |>
    dplyr::pull("topic") |> dplyr::bind_rows() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "topic")

  categories <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(categories = purrr::pmap(list(diarienummer, kategoriseringfinansiar), \(a, b) c(id_funding = a, bind_rows(b)))) |>
    dplyr::pull("categories") |> dplyr::bind_rows() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "category")

  calls <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(calls = purrr::pmap(list(diarienummer, utlysning), \(a, b) c(id_funding = a, b))) |>
    dplyr::pull("calls") |> dplyr::bind_rows() |>
    dplyr::select(-c("lank")) |> dplyr::distinct() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "call")

  payouts <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(payouts = purrr::pmap(list(diarienummer, beslutadFinansiering), \(a, b) c(id_funding = a, bind_rows(b)))) |>
    dplyr::pull("payouts") |> dplyr::bind_rows() |>
    tidyr::unnest_wider("finansieradOrganisation") |>
    tidyr::unnest("roll") |> tidyr::unnest("roll") |>
    dplyr::filter(belopp > 0) |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "payout")

  sdgs <-
    x |> purrr::transpose() |> tibble::as_tibble() |>
    dplyr::mutate(sdgs = purrr::pmap(list(diarienummer, hallbarhetsmal), \(a, b) c(id_funding = a, bind_rows(b)))) |>
    dplyr::pull("sdgs") |> dplyr::bind_rows() |>
    readr::type_convert() |> suppressMessages() |>
    dplyr::rename_with(cols_rename, entity = "sdg")

  list(
    proposals = one_to_one, links = links, programs = programs,
    organisations = orgs, persons = persons, decisions = decisions,
    topics = topics, categories = categories, calls = calls,
    payouts = payouts, sdgs = sdgs
  )

}
