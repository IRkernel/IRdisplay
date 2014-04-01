base_display <- function(mimetype, content, metadata) {
    warning("IR_display can only be used from the IPython R kernel and R magic.")
}

#'Display data by mimetype
#'
#' @param mimetype The mimetype of the data
#' @param  content The data as a length 1 string vector
#' @param  metadata A named list of metadata
#' @export
display <- function(mimetype, content, metadata) {
    base_display(mimetype, content, metadata)
}

#'Display HTML output
#'
#' @param  content The HTML as a length 1 string vector
#' @export
display_html = function(content) {
    base_display('text/html', content)
}
