# library(pacman)
# p_load("rvest")
# p_load("magrittr")
# p_load("stringr")
# p_load("dplyr")
# p_load("stringi")
# p_load("purrr")
# p_load("retry")

#' Replace German umlaute with `"ae"` and the like
#'
#' @param x Character string containing umlaute
#'
#' @return Character string with umlaute replaced
#' @export
#'
#' @examples
#' replace_umlaute("Jenalöbnitz")
#' replace_umlaute("Jenaprießnitz")
replace_umlaute <- function(x) {
  stringi::stri_replace_all_fixed(x,
                                  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"),
                                  c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"),
                                  vectorize_all = FALSE)
}


#' Determine if `try()` call failed
#'
#' @param x Result of a [try()] call.
#'
#' @return `TRUE` if `x` is of class `"try-error"`, indicating
#' an error.
#' @seealso [try()]
#' @export
#'
#' @examples
#' failed(try(sqrt("a")))
#' failed(try(sqrt(2)))
failed <- function(x) {
  inherits(x, "try-error")
}


#' Convenience function that tr
#'
#' @param expr An expression to be evaluated
#' @param when Condition under which to retry, see [retry::retry()]
#' @param ... Additional arguments for [retry::retry()]
#'
#' @return The result of the [try()] call, i.e. the value of
#' the expression being evaluated, or an object of class
#' `"try-error"`.
#' @seealso [try_read_html()]
#' @export
#'
#' @examples
#' failed(try_retry(sqrt("a")))
#' # This one won't get any better just by repeating its
#' # evaluation but maybe if a webpage can't be read,
#' # it's worth a second try...
try_retry <- function(expr, when = "error", ...) {
  try(retry::retry(expr, when = when, ...))
}


#' Replace `NULL` values with `NA`
#'
#' @param x An object, or `NULL`
#'
#' @return `NA` if `x` is `NULL`, `x` otherwise.
#' @export
replace_null <- function(x) {
  if (is.null(x)) return(NA) else return(x)
}


#' Recursive version of `file.exists()`
#'
#' @param x File name
#' @param path Folder in which to start the recursive search.
#'
#' @return `TRUE` if at least one file matching `x` was
#' found, and `FALSE` otherwise.
#' @details Currently does not support regular expressions or
#' wildcards; dot (for file extensions etc.) are dealt with.
#' @export
#'
#' @examples
#' # recursive_file_exists("I_dont_exist.R")
recursive_file_exists <- function(x, path = ".") {
  (length(dir(path = path,
              pattern = gsub(".", "\\.", x, fixed = TRUE),
              recursive = TRUE)) > 0)
}



#' Try and retry reading a webpage
#'
#' Try reading a webpage possibly multiple times in case of error, with delays before, between, and after the attempts.
#'
#' @param expose_url URL
#' @param delay_before,delay_after Delay (in seconds) before and/or after trying to read the webpage
#' @param max_tries Maximum number of attempts
#' @param interval Interval between the attempts; defaults to the maximum of the two delay argument.
#' @param ... Additional arguments for [xml2::read_html()]
#'
#' @return The result of [xml2::read_html()], or an object of class `"try-error"` in case of error.
#' @export
try_read_html <- function(expose_url,
                          delay_before = 0, delay_after = 1,
                          max_tries = 2, interval = max(delay_before, delay_after),
                          ...) {
  if (delay_before > 0)
    Sys.sleep(delay_before)
  expose <- try_retry(xml2::read_html(expose_url, ...),
                      max_tries = max_tries,
                      interval = interval)
  if (delay_after > 0)
    Sys.sleep(delay_after)
  expose
}

