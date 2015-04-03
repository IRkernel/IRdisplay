namedlist <- function() setNames(list(), character(0))

base_display <- function(data, metadata) {
    warning("IR_display can only be used from the IPython R kernel and R magic.")
}

#'Display data by mimetype, with optional alternative representations.
#'
#' @param data  A named list mapping mimetypes to content (base64 encoded for binary data)
#' @param metadata  A named list mapping mimetypes to named lists of specific metadata
#' @export
display <- function(data, metadata = NULL) {
    base_display(data, metadata)
}

#'Display a single type of data
#'
#' @param mimetype  The mimetype of the data
#' @param content  The data as a length 1 string vector
#' @param metadata  A named list of metadata
#' @export
display1 <- function(mimetype, content, metadata = NULL) {
    data <- list()
    data[[mimetype]] <- content
    base_display(data, metadata)
}

#'Display a set of formatted objects that the Jupyter frontend/output can select from
#'
#' @param metadata  Initial metadata. Mimetype-wise data will be added
#' @param ...  Mimetypes and content.
#' 
#' You can either give raw vectors or character vectors as content.
#' 
#' Also you can give a list of which the first element will be content, the rest metadata.
#' 
#' @examples
#' \dontrun{
#' display_multi(
#'   'text/html'       = '<em>Wop</em>'
#'   'application/pdf' = base64encode('test.pdf'),
#'   'image/png'       = list(some.png.data, width = 800, height = 600),
#'   'image/svg+xml'   = '<svg></svg>')
#' }
#' 
#' @importFrom base64enc base64encode
#' @export
display_multi <- function(..., metadata = namedlist()) {
    contents <- list(...)
    
    data <- list()
    for (mime in names(contents)) {
        content <- contents[[mime]]
        if (is.list(content)) {
            metadata[[mime]] <- content[-1]
            content <- content[[1]]
        }
        
        if (is.raw(content))
            content <- base64encode(content)
        
        data[[mime]] <- c(data[[mime]], content)  #append if existing
    }
    base_display(data, metadata)
}

prepare_content <- function(data = NULL, file = NULL, isbinary = TRUE) {
    if (is.null(file) == is.null(data))
        stop('Either need to specify data or file, but not both')
    
    if (isbinary) {
        if (!is.null(file)) {
            content <- base64encode(file)
        } else if (is.raw(data)) {
            content <- base64encode(data)
        } else stop('Data needs to be a raw vector')
    } else {
        if (!is.null(file)) {
            content <- readChar(file, file.info(file)$size)
        } else if (is.character(data)) {
            content <- data
        } else stop('Data needs to be a character vector')
    }
    
    content
}

display_raw <- function(mimetype, isbinary, data, file, metadata = NULL) {
    content <- prepare_content(data, file, isbinary)
    display1(content, metadata)
}

#'Display HTML output
#'
#' Either data or file must be passed.
#' @param data  The HTML code as a character vector
#' @param file  The path to a HTML file or a connection
#' @export
display_html <- function(data = NULL, file = NULL) {
    display_raw('text/html', FALSE, data, file)
}

#'Display PNG output
#'
#' Either data or file must be passed.
#' @param data  The PNG data as a raw vector
#' @param file  The path to a PNG file or a connection
#' @param width  The width to display the image
#' @param height  The height to display the image
#' 
#' @export
display_png <- function(data = NULL, file = NULL, width = NULL, height = NULL) {
    metadata = namedlist()
    if (!is.null(width))  metadata$width  <- width
    if (!is.null(height)) metadata$height <- height
    
    display_raw('image/png', TRUE, data, file, metadata)
}

#'Display PDF output
#'
#' Either data or file must be passed.
#' @param data  The PDF data as a raw vector
#' @param file  The path to a PDF file or a connection
#' 
#' @export
display_pdf <- function(data = NULL, file = NULL) display_raw('application/pdf', TRUE, data, file)

#'Display SVG output
#'
#' Either data or file must be passed.
#' @param data  The SVG data as character vector
#' @param filename  The path to a SVG file
#' 
#' @export
display_svg <- function(data = NULL, file = NULL) display_raw('image/svg+xml', FALSE, data, file)
