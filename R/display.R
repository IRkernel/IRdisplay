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

display_png = function(data=NULL, filename=NULL, width=NULL, height=NULL) {
    if (!is.null(data)) {
        filename = tempfile()
        writeBin(data, filename)
    }
    tf = tempfile()
    encode(filename, tf) # base64
    b64data = paste(readLines(tf), collapse = "")
    metadata = setNames(list(), character(0))
    if (!is.null(width)) metadata$width=width
    if (!is.null(height)) metadata$height=height
    base_display('image/png', b64data, metadata)
}
