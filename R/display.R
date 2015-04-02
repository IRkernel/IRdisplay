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
#' @param ...  Formats and their parameters.
#' Parameters have to be lists if more/other than the first one (data) is given.
#' 
#' Allowed formats: html, png, pdf, svg
#' 
#' @examples
#' \dontrun{
#' display_multi(
#'   html = '<em>Wop</em>'
#'   pdf = list(file = 'test.pdf'),
#'   png = list(some.png.data, metadata = list(width = 800, height = 600)),
#'   svg = '<svg></svg>')
#' }
#' 
#' @export
display_multi <- function(...) {
    paramsets <- list(...)
    
    data <- list()
    metadata <- namedlist()
    for (format in names(paramsets)) {
        params <- paramsets[[format]]
        if (!is.list(params))  #`params` is just data
            params <- list(params)
        
        def <- do.call(prepare, c(list(format), params))
        
        data[[def$mimetype]] <- def$content
        if (length(def$metadata) != 0)
            metadata[[def$mimetype]] <- def$metadata
    }
    base_display(data, metadata)
}

prepare <- function(format, data = NULL, file = NULL) {
    if (!(format %in% names(mimes)))
        stop(sprintf('invalid format %s. Choose from %s', format, paste(names(mimes), collapse = ', ')))
    if (is.null(file) == is.null(data))
        stop('Either need to specify data or file, but not both')
    
    if (isbinary[[format]]) {
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
    
    list(mimetype = mimes[[format]], content = content)
}

display_raw <- function(format, data, file, metadata = NULL) {
    args <- prepare(format, data, file)
    display1(args$mimetype, args$content, metadata)
}

mimes <- list(
    html = 'text/html',
    png = 'image/png',
    pdf = 'application/pdf',
    svg = 'image/svg+xml')

isbinary <- list(
    html = FALSE,
    png = TRUE,
    pdf = TRUE,
    svg = FALSE)

#'Display HTML output
#'
#' Either data or file must be passed.
#' @param data  The HTML code as a character vector
#' @param file  The path to a HTML file or a connection
#' @export
display_html <- function(data = NULL, file = NULL) {
    display_raw('html', data, file)
}

#'Display PNG output
#'
#' Either data or file must be passed.
#' @param data  The PNG data as a raw vector
#' @param file  The path to a PNG file or a connection
#' @param width  The width to display the image
#' @param height  The height to display the image
#' 
#' @importFrom base64enc base64encode
#' @export
display_png <- function(data = NULL, file = NULL, width = NULL, height = NULL) {
    metadata = namedlist()
    if (!is.null(width))  metadata$width  <- width
    if (!is.null(height)) metadata$height <- height
    
    display_raw('png', data, file, metadata)
}

#'Display PDF output
#'
#' Either data or file must be passed.
#' @param data  The PDF data as a raw vector
#' @param file  The path to a PDF file or a connection
#' 
#' @importFrom base64enc base64encode
#' @export
display_pdf <- function(data = NULL, file = NULL) display_raw('pdf', data, file)

#'Display SVG output
#'
#' Either data or file must be passed.
#' @param data  The SVG data as character vector
#' @param filename  The path to a SVG file
#' 
#' @export
display_svg <- function(data = NULL, file = NULL) display_raw('svg', data, file)
