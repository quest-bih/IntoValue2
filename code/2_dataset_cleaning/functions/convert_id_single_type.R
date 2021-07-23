#' Convert publiction ids using the [Fatcat](https://fatcat.wiki/) [API](https://api.fatcat.wiki/)
#'
#' Non-vectorized function. Wrap in `purrr::map_dfr()` to call on multiple ids. May further develop vectorized option with `purrr::map()` or `plyr::llply`.
#'
#' @param id PMID (character) or DOI (character or numeric)
#' @param from Character. One of "doi", "pmid", "pmcid"
#' @param to Character. One of "doi", "pmid", "pmcid". Must be different than \code{from}.
#' @param quiet logical. Should success/error info be displayed? Default is TRUE.
#'
#' @return Character. id of type \code{to}. Returns NA if input is NA, API error, or API does not have \code{to} id for input \code{id}.
#'
#' @export
#'
#' @examples
#' convert_id("10.1016/S0168-8278(08)61002-8", "doi", "pmid", quiet = FALSE)
#' purrr::map(
#'   rlang::set_names(c("10.1056/NEJMoa1916623", "10.1038/ncomms11393", "10.5348/D01-2017-21-OA-1")),
#'   convert_id,
#'   from = "doi",
#'   to = "pmid"
#' )

convert_id_single_type <- function(id, from = NULL, to = NULL, quiet = TRUE){

  ext_ids <- c("doi", "pmid", "pmcid")

  error_txt <-
    glue::glue_collapse(glue::glue("'{ext_ids}'"), sep = ", ", last = ", or ")

  # Check input
  if (!from %in% ext_ids){
    rlang::abort(glue::glue("`from` must be {error_txt}"))
  }

  if (!to %in% ext_ids){
    rlang::abort(glue::glue("`to` must be {error_txt}"))
  }

  if (from == to){
    rlang::abort('`from` and `to` must not be the same')
  }

  # If input is NA, output is NA
  if (is.na(id)) {
    ifelse(to == "pmid", return(NA_real_), return(NA_character_))
  }


  url <-
    paste0("https://api.fatcat.wiki/v0/release/lookup?", from, "=", id)

  res <- httr::GET(url)

  # If API error, return NA
  if (res$status_code != 200) {
    status <- "API error"
    out <- NA_character_
  }

  if (res$status_code == 200) {
    parsed <- suppressMessages(jsonlite::parse_json(res))
    out <- purrr::pluck(parsed$ext_ids, to)

    # If from id available in fatcat but not to id, returns NULL, so convert to NA
    if (purrr::is_null(out)) {
      status <- glue::glue("{to} not available")
      out <- NA_character_
    } else {status <- "converted"}
  }

  # Convert pmid to numeric
  if (to == "pmid") out <- as.numeric(out)

  if (!quiet) rlang::inform(glue::glue("{id}: {status}"))

  out
}
