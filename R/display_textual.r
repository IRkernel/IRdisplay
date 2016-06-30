#' Display JSON
#'
#' Either data or file must be passed.
#' @param data  The JSON code as a character vector
#' @param file  The path to a JSON file or a connection
#' @export
display_json <- function(data = NULL, file = NULL) display_raw('application/json', FALSE, data, file)

#' Display (execute) Javascript
#'
#' Either data or file must be passed.
#' @param data  The Javascript code as a character vector
#' @param file  The path to a Javascript file or a connection
#' @export
display_javascript <- function(data = NULL, file = NULL) display_raw('application/javascript', FALSE, data, file)

#' Display HTML output
#'
#' Either data or file must be passed.
#' @param data  The HTML code as a character vector
#' @param file  The path to a HTML file or a connection
#' @export
display_html <- function(data = NULL, file = NULL) display_raw('text/html', FALSE, data, file)

#' Display Markdown output
#'
#' Either data or file must be passed.
#' @param data  The Markdown code as a character vector
#' @param file  The path to a Markdown file or a connection
#' @export
display_markdown <- function(data = NULL, file = NULL) display_raw('text/markdown', FALSE, data, file)

#' Display LaTeX output
#'
#' Either data or file must be passed.
#' @param data  The LaTeX code as a character vector
#' @param file  The path to a LaTeX file or a connection
#' @export
display_latex <- function(data = NULL, file = NULL) display_raw('text/latex', FALSE, data, file)
