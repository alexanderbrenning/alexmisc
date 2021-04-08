#' Geocode town name using (German) Wikipedia
#'
#' A quick and dirty attempt at geocoding places using Wikipedia.
#'
#' @param name,names Name of town as it appears in its Wikipedia URL, e.g. `"Halle_(Saale)"` or .
#' @param wiki Identifies Wikipedia language edition, e.g. `"de"` for German (default) or `"en"` for English.
#' @param digits Round lat/lon to this many decimal places.
#' @param delay Delay (in seconds) after Wikipedia call.
#'
#' @return A list containing the `name`, `latitude` and `longitude` of the requested town.
#' Latitude and longitude are `NA` if the attempt failed.
#' @export
#'
#' @examples
#' geocode_wiki_town("Jena")
#' geocode_wiki_town("Halle_(Saale)") # with underscore
#' geocode_wiki_town("Nürnberg") # with umlaut
geocode_wiki_town <- function(name, wiki = "de", digits = 5, delay = 0.1) {
  require("magrittr")
  lat <- lon <- NA
  if (!failed(try(
    res <- xml2::read_html(paste0("http://",
                                  tolower(wiki),
                                  ".wikipedia.org/wiki/",
                                  name))
  ))) {
    try(lat <- res %>% rvest::html_node("span .latitude") %>%
          rvest::html_text() %>% as.numeric() %>% round(digits = digits)
    )
    try(lon <- res %>% rvest::html_node("span .longitude") %>%
          rvest::html_text() %>% as.numeric() %>% round(digits = digits)
    )
  }
  if (delay > 0) Sys.sleep(delay)

  list(
    name = name,
    latitude = lat,
    longitude = lon
  )
}



#' @describeIn geocode_wiki_town Iterate `geocode_wiki_town` over multiple towns
#' @export
geocode_wiki_towns <- function(names, wiki = "de", digits = 5, delay = 1) {
  names %>% purrr::map(geocode_wiki_town,
                       wiki = wiki,
                       digits = digits,
                       delay = delay) %>%
    dplyr::bind_rows() %>%
    as.data.frame()
}

