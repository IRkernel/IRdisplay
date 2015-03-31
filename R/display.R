base_display <- function(data, metadata) {
    warning("IR_display can only be used from the IPython R kernel and R magic.")
}

#'Display data by mimetype, with optional alternative representations.
#'
#' @param data A named list mapping mimetypes to content (base64 encoded for binary data)
#' @param metadata A named list mapping mimetypes to named lists of specific metadata
#' @export
display <- function(data, metadata=NULL) {
    base_display(data, metadata)
}

#'Display a single type of data
#'
#' @param mimetype The mimetype of the data
#' @param  content The data as a length 1 string vector
#' @param  metadata A named list of metadata
#' @export
display1 <- function(mimetype, content, metadata=NULL) {
    data = list()
    data[mimetype] = content
    base_display(data, metadata)
}

#'Display HTML output
#'
#' @param  content The HTML as a length 1 string vector
#' @export
display_html = function(content) {
    display1('text/html', content)
}

#'Display PNG output
#'
#' Either data or filename must be passed.
#' @param data The PNG data as a raw vector
#' @param file The path to a PNG file or a connection
#' @param width The width to display the image
#' @param height The height to display the image
#' @importFrom base64enc base64encode
#' @export
display_png = function(data = NULL, file = NULL, width = NULL, height = NULL) {
    if (!is.null(file)) {
        b64data <- base64encode(filename)
    } else if (!is.null(data) && is.raw(data)) {
        b64data <- base64encode(data)
    } else stop('Either need to specify raw data as vector or filename/connection')
    
    metadata = setNames(list(), character(0))
    if (!is.null(width))  metadata$width  <- width
    if (!is.null(height)) metadata$height <- height
    display1('image/png', b64data, metadata)
}

#'Display SVG output
#'
#' Either data or filename must be passed.
#' @param data The SVG data as character vector
#' @param filename The path to a SVG file
#' @param width The width to display the image
#' @param height The height to display the image
#' @export
display_svg = function(data = NULL, file = NULL, width = NULL, height = NULL) {
    if (!is.null(file)) {
        stopifnot(is.null(data))
        data <- readChar(file, file.info(file)$size)
    }
    
    metadata = setNames(list(), character(0))
    if (!is.null(width))  metadata$width  <- width
    if (!is.null(height)) metadata$height <- height
    display1('image/svg+xml', data, metadata)
}
