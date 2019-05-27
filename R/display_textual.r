#' Display a specific textual output
#'
#' Either \code{data} or \code{file} must be passed.
#' 
#' @param data  The code or markup content as a \code{\link[base]{character}} vector
#' @param file  The path to a file or a \code{\link[base]{connection}} containing the content
#' 
#' @seealso \code{\link{display_<image>}}
#' 
#' @examples \dontrun{## (Run inside of an IRkernel)
#' display_text('Just text')
#' display_markdown('[MD](http://commonmark.org) *formatted*')
#' display_javascript('execute(this)')}
#' 
#' @name display_<text>
#' @rdname display-textual
NULL

#' @rdname display-textual
#' @export
display_json <- function(data = NULL, file = NULL) display_raw('application/json', FALSE, data, file)

#' @rdname display-textual
#' @export
display_javascript <- function(data = NULL, file = NULL) display_raw('application/javascript', FALSE, data, file)

#' @rdname display-textual
#' @export
display_html <- function(data = NULL, file = NULL) display_raw('text/html', FALSE, data, file, isolate_full_html(list('text/html' = data)))

#' @rdname display-textual
#' @export
display_markdown <- function(data = NULL, file = NULL) display_raw('text/markdown', FALSE, data, file)

#' @rdname display-textual
#' @export
display_latex <- function(data = NULL, file = NULL) display_raw('text/latex', FALSE, data, file)
