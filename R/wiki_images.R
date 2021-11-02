#' Download image from Wikipedia
#'
#' @param name Name of Wikipedia entry.
#' @param destfolder,destfile Destination folder and file name for saving image. If `NULL` (default), use name of Wikipedia page.
#' @param wiki Which Wikipedia to use: English (`"en"`, default), or German (`"de"`).
#' @param base_url Wikipedia base URL, which depends on `wiki`.
#' @param filter Defaults to `NULL`, which will pick the appropriate filter. E.g., `"/wiki/File:"`, depending on language.
#' @param which Determines which picture to choose: Either `"first"` (default), `"last"`, `"random"`, or a numeric value indicating the picture number.
#' @param save_metadata If `TRUE` (default and recommended), save extracted metadata (e.g., source URL, author, licence, if possible) to a `.rds` file with same file name as image file. The metadata is stored in a list format.
#' @param verbose Logical; controls verbosity.
#' @param quiet_download Passed to [download.file()] as its `quiet` argument. Defaults to `!verbose`.
#'
#' @return Invisible returns a list with metadata such as source URL, author, licence, if possible.
#' @export
#' @import stringr
#' @import rvest
#' @import dplyr
#' @importFrom xml2 read_html
#' @importFrom utils download.file
#'
#' @examples
#' # res <- download_wiki_image("ArcGIS",
#' #                            destfile = "test", destfolder = "figures")
#' # res <- download_wiki_image("Stoppschild", wiki = "de",
#' #                            destfile = "test", destfolder = "figures")
#' # res <- download_wiki_image("Lake Winnipesaukee",
#' #                            destfile = "test", destfolder = "figures")
#' # res <- download_wiki_image("E10 (Kraftstoff)", wiki = "de", which = 2,
#' #                            destfolder = "figures")
download_wiki_image <- function(name,
                                destfolder = ".",
                                destfile = NULL,
                                wiki = c("en","de")[1],
                                base_url = NULL,
                                filter = NULL,
                                which = c("first", "last", "random")[1],
                                save_metadata = TRUE,
                                verbose = FALSE,
                                quiet_download = !verbose) {
  wiki_name <- name %>% stringr::str_replace_all(" ", "_")

  if (is.null(destfile))
    destfile <- wiki_name %>% str_remove_all("\\(") %>% str_remove_all("\\)")

  # Identify base URL and image URL tag for the chosen Wikipedia edition:
  if (is.null(base_url)) {
    base_url <- switch(wiki,
                       "https://en.wikipedia.org",
                       en = "https://en.wikipedia.org",
                       de = "https://de.wikipedia.org")
  }
  if (is.null(filter)) {
    filter <- switch(wiki,
                     "/wiki/File:",
                     en = "/wiki/File:",
                     de = "/wiki/Datei:")
  }

  if (verbose)
    cat("Download ", which, " image from ",
        switch(wiki, de = "German", en = "English", wiki),
        " Wikipedia article for '", name, "'...\n", sep = "")

  # Set up results object:
  RESULT <- list(
    name = name,
    base_url = base_url,
    wiki = wiki,
    filter = filter
  )

  url <- paste0(base_url, "/wiki/", wiki_name)
  RESULT$url <- url

  # Read Wikipedia article:
  if (verbose)
    cat("Read Wikipedia article from <", url, ">...\n", sep = "")
  res <- xml2::read_html(url)

  urls <- res %>% rvest::html_nodes("div div .thumbimage a") %>% rvest::html_attr("href")
  if (length(urls) == 0) {
    urls2 <- res %>% rvest::html_nodes("div div a") %>% rvest::html_attr("href")
    urls2 <- urls2 %>% stringr::str_subset(filter) %>% unique()
    urls <- c(urls, urls2)
  }
  RESULT$image_urls <- urls

  if (verbose) {
    cat("Found ", length(urls), " images in Wikipedia article:\n", sep = "")
    cat(paste("-", urls), sep = "\n")
  }

  if (length(urls) == 0) {
    warning("Couldn't find an image URL in the Wikipedia article.")
    return(invisible(RESULT))
  }

  if (length(urls) > 1) {
    if (which == "last") {
      url <- dplyr::last(urls)
    } else if (which == "random") {
      url <- sample(urls, size = 1)
    } else if (is.numeric(which)) {
      url <- urls[ min(max(1, which), length(urls)) ]
    } else url <- dplyr::first(urls)
  } else url <- urls
  # Add base_url only if relative path is provided:
  if (!stringr::str_detect(url, "^http[s]?://"))
    url <- paste0(base_url, url)
  RESULT$chosen_image_url <- url

  # Read Wikimedia page for chosen image:
  if (verbose)
    cat("Read Wikimedia image description from <", url, ">...\n", sep = "")
  res <- xml2::read_html(url)

  # Try to extract licence information:
  lic_text <- NULL
  lic_tbl <- res %>% rvest::html_nodes("table.licensetpl") %>% rvest::html_nodes("td span")
  lic_attr <- lic_tbl %>% rvest::html_attr("class")
  if (length(lic_attr) > 0) {
    lic_text <- lic_tbl %>% rvest::html_text("class")
    if (any(lic_attr == "licensetpl_long")) {
      lic_text <- lic_text[lic_attr == "licensetpl_long"]
    } else if (any(lic_attr == "licensetpl_short")) {
      lic_text <- lic_text[lic_attr == "licensetpl_short"]
    }
    lic_text <- stringr::str_trim(lic_text)
    if (length(lic_text) > 1) {
      lic_text <- lic_text %>%
        stringr::str_subset(".*GNU Free Documentation License.*", negate = TRUE) %>%
        stringr::str_subset(".*GFDL.*", negate = TRUE)
    }
    RESULT$license <- lic_text
  }
  if (verbose) {
    if (is.null(lic_text)) {
      cat("Couldn't extract license information.\n")
    } else {
      cat("License: '", lic_text, "'\n", sep = "")
    }
  }

  # Try to extract additional metadata:
  info_tbl <- res %>% rvest::html_node("table.fileinfotpl-type-information")
  if (class(info_tbl) == "xml_missing") {
    RESULT$info <- list(
      description = NA,
      date = NA,
      source = NA,
      author = NA
    )
    if (verbose)
      cat("Couldn't find table with image description and source.\n")
  } else {
    # Convert to data.frame - but first column holds names:
    info_tbl <- info_tbl %>% rvest::html_table(fill = TRUE)
    # Turn it into a named list:
    info <- as.list(info_tbl$X2)
    names(info) <- info_tbl$X1
    if (wiki == "de") {
      wh <- grep("^Beschreibung", info_tbl$X1)
      if (length(wh) == 1) names(info)[wh] <- "Description"
      names(info)[names(info) == "Quelle"] <- "Source"
      names(info)[names(info) == "Datum"] <- "Date"
      names(info)[names(info) == "Autor"] <- "Author"
    } else { # assuming wiki == "en":
      wh <- grep("^Description", info_tbl$X1)
      if (length(wh) == 1) names(info)[wh] <- "Description"
    }
    if (!is.null(info$Description)) if (!is.na(info$Description)) {
      info$Description <- strsplit(info$Description, "\n")[[1]][1] %>% stringr::str_trim() %>%
        stringr::str_replace("^Deutsch: ", "") %>% stringr::str_replace("^English: ", "")
    }
    names(info) <- tolower(names(info))
    if (is.null(info$description)) info$description <- NA
    if (is.null(info$date)) info$date <- NA
    if (is.null(info$source)) info$source <- NA
    if (is.null(info$author)) info$author <- NA
    info <- info[c("description", "date", "source", "author")]
    RESULT$info <- info
    if (verbose)
      cat("Image information: Author: ", info$author, "; source: ", info$source,
          "; date: ", info$date, "; description: '",
          substr(info$description, 1, 20),
          ifelse(nchar(info$description) > 20, "[...]", ""),
          "'\n", sep = "")
  }

  urls <- res %>% rvest::html_nodes("meta") %>% rvest::html_attr("content")
  props <- res %>% rvest::html_nodes("meta") %>% rvest::html_attr("property")
  url <- urls[ (props == "og:image") & !is.na(props) ] %>% dplyr::first()
  RESULT$download_image_url <- url

  if (is.null(url)) {
    warning("Couldn't find a download link for the image.")
    return(invisible(RESULT))
  }

  # Retrieve ouput file name:
  if (is.null(destfile)) {
    # Use original file name as used in Wikipedia:
    destfile <- strsplit(url, "/")[[1]] %>% dplyr::last()
  } else {
    # Use (base) file name provided by user,
    # but add file extension from Wiki:
    file_ext <- strsplit(url, "/")[[1]] %>% dplyr::last() %>%
      stringr::str_extract("\\.[:alnum:]{0,4}$")
    destfile <- paste0(destfile, file_ext)
  }
  metadatafile <- paste0(destfile, ".rds")

  if (verbose)
    cat("Download image <", url, "> to file '", destfile,
        "' in folder '", destfolder, "'...\n", sep = "")

  # Download the image:
  utils::download.file(url, mode = "wb",
                destfile = file.path(destfolder, destfile),
                quiet = quiet_download)
  RESULT$destfolder <- destfolder
  RESULT$destfile <- destfile

  if (save_metadata) {
    if (verbose)
      cat("Saving metadata to file '", metadatafile,
          "' in folder '", destfolder, "'...\n", sep = "")
    saveRDS(RESULT, file = file.path(destfolder, metadatafile))
  }

  if (verbose)
    cat("Done.\n")

  invisible(RESULT)
}




#' Download Wikipedia image if it doesn't already exist
#'
#' @inheritParams download_wiki_image
#' @param ... additional arguments passed to [download_wiki_image()].
#' @return A list containing image metadata, as returned by [download_wiki_image()].
#' @export
download_new_wiki_image <- function(name,
                                    destfile = gsub(" ", "_", name),
                                    destfolder = ".",
                                    ...) {
  check_files <- paste0(destfolder, "/", destfile, ".",
                        c("jpg", "jpeg", "tif", "tiff", "png", "svg"))
  #cat(check_files, sep = "\n")
  found <- any(file.exists(check_files))
  if (found) {
    cat("Skipping '", name, "', file exists...\n", sep = "")
    res <- NULL
  } else {
    res <- download_wiki_image(name = name,
                               destfile = destfile, destfolder = destfolder,
                               ...)
  }
  invisible(res)
}




#' Include an image file in an RMarkdown document
#'
#' @param file,path File name and folder of image to be included in the Markdown document. It is recommended to use [here::here()] to specify the path.
#' @param caption Figure caption. If `"NULL"` and metadata exists, use [image_caption()] to construct an image caption.
#' @param default_caption Default caption, if `caption` argument is `NULL`.
#' @param width Image width, e.g. `"5cm"`. If `NULL` (default), don't specify width in Markdown.
#' @param include_suppl Include image file? Defaults to `TRUE`.
#' @param fontsize HTML font size, relative to current standard setting. Default: `-2`.
#'
#' @return Markdown code to be included in the document. The function is also called for its side effect of calling [exams::include_supplement()].
#' @export
#' @import stringr
#' @importFrom  exams include_supplement
include_md_image <- function(file, path = NULL, caption = NULL, default_caption = "",
                             width = NULL, include_suppl = TRUE,
                             fontsize = -2) {
  if (is.null(file))
    return("")

  if (is.null(caption)) {
    metafile <- paste0(file, ".rds")
    if (!is.null(path)) metafile <- file.path(path, metafile)
    if (file.exists(metafile)) {
      meta <- readRDS(metafile)
      caption <- image_caption(meta)
    } else {
      caption <- default_caption
    }
  }
  if (include_suppl)
    exams::include_supplement(file, dir = path)
  caption <- caption %>% stringr::str_remove_all("\\[") %>% stringr::str_remove_all("\\]")
  res <- paste0("![",
                ifelse(is.null(fontsize), "", paste0('<font size="', fontsize, '">')),
                caption,
                ifelse(is.null(fontsize), "", "</font>"),
                "](", file, ")")
  if (!is.null(width))
    res <- paste0(res, "{width=", width, "}")
  res
}






#' Abbreviation by ellipsis
#'
#' @param x Character string to be abbreviated.
#' @param nc Maximum number of characters of the result.
#' @param replacement Replacement, defaults to `"..."`.
#'
#' @return An abbreviated character string of maximum `nc` characters length.
#' @export
#'
#' @examples
#' my_abbreviate("Hello, World!", nc = 8)
my_abbreviate <- function(x, nc, replacement = "...") {
  if (is.null(x)) return(x)
  if (is.na(x)) return(NA)
  ncr <- nchar(replacement)
  if (nc > ncr) {
    if (nchar(x) > nc - ncr)
      x <- paste0(substr(x, 1, nc - ncr), replacement)
  }
  x
}




#' Simple image caption from metadata
#'
#' @param x Image metadata as returned by [download_wiki_image()]. The `description` field will be used to create a figure caption.
#' @param use_name Include the author's name in figure caption? (Default: `TRUE`).
#' @param use_date Include the image date in figure caption? (Default: `FALSE`).
#' @param nchar_description Maximum number of characters for the description.
#' @param language Either `"de"` (German caption; the current default) or `"en"` (English version).
#'
#' @return A single character string with an image description and basic provenance information.
#' @export
image_caption <- function(x, use_name = TRUE, use_date = FALSE,
                          nchar_description = 30, language = c("de", "en")[1]) {
  if (language == "de") {
    tags <- list(
      author = "Autor",
      source = "Quelle",
      date = "Datum",
      license = "Lizenz"
    )
  } else {
    tags <- list(
      author = "Author",
      source = "Source",
      date = "Date",
      license = "License"
    )
  }
  descr <- x$info$description
  if (is.null(descr) | use_name)
    descr <- x$name
  cap <- paste0(
    my_abbreviate(descr, nc = nchar_description, replacement = ".."), ". ",
    tags$license, ": ",
    x$license, ". ",
    tags$author, ": ",
    my_abbreviate(x$info$author, nc = 20, replacement = ".."), ". ",
    tags$source, ": ",
    my_abbreviate(x$info$source, nc = 20, replacement = ".."), ".")
  if (use_date) {
    cap <- paste0(cap, " ", tags$date, ": ",
                  my_abbreviate(x$info$date, nc = 20, replacement = ".."), ". ")
  }
  cap
}

