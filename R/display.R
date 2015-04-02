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

get_display_fun <- function(format) switch(
    format,
    png = display_png,
    pdf = display_pdf,
    svg = display_svg,
    stop(sprintf('invalid format “%s” specified', format)))

#'Display a set of image formats that the output format can select from
#'
#' @param ...  Formats and their parameters.
#' Parameters have to be lists if more/other than the first one (data) is given.
#' 
#' Allowed formats: png, pdf, svg
#' 
#' @examples
#' \dontrun{
#' display_images(
#'   pdf = list(file = 'test.pdf'),
#'   png = list(some.pngdata, width = 800, height = 600),
#'   svg = '<svg></svg>')
#' }
#' 
#' @export
display_images <- function(...) {
    paramsets <- list(...)
    
    data <- list()
    metadata <- namedlist()
    for (format in names(paramsets)) {
        params <- paramsets[[format]]
        if (!is.list(params))  #`params` is just data
            params <- list(params)
        
        args <- c(params, list(defer = TRUE))
        def <- do.call(get_display_fun(format), args)
        
        data[[def$mimetype]] <- def$content
        if (length(def$metadata) != 0)
            metadata[[def$mimetype]] <- def$metadata
    }
    base_display(data, metadata)
}

#'Display HTML output
#'
#' @param content  The HTML as a length 1 string vector
#' @export
display_html <- function(content) {
    display1('text/html', content)
}

display_binary_image <- function(mimetype, data = NULL, file = NULL, width = NULL, height = NULL, defer = FALSE) {
    if (!is.null(file)) {
        b64data <- base64encode(file)
    } else if (!is.null(data) && is.raw(data)) {
        b64data <- base64encode(data)
    } else stop('Either need to specify raw data as vector or filename/connection')
    
    metadata = namedlist()
    if (!is.null(width))  metadata$width  <- width
    if (!is.null(height)) metadata$height <- height
    
    if (!defer)
        display1(mimetype, b64data, metadata)
    else
        list(mimetype = mimetype, content = b64data, metadata = metadata)
}

#'Display PNG output
#'
#' Either data or file must be passed.
#' @param data  The PNG data as a raw vector
#' @param file  The path to a PNG file or a connection
#' @param width  The width to display the image
#' @param height  The height to display the image
#' @param defer  Do not call \link{display1}, but return prepared data instead
#' 
#' @return If \code{defer} is TRUE, a list with the components:
#' \itemize{
#'  \item{mimetype}{\code{'image/png'}}
#'  \item{content}{Base 64 encoded PNG data}
#'  \item{metadata}{A list with \code{width} and \code{height} if given as parameters}
#' }
#' 
#' @importFrom base64enc base64encode
#' @export
display_png <- function(data = NULL, file = NULL, width = NULL, height = NULL, defer = FALSE) {
    display_binary_image('image/png', data, file, width, height, defer)
}

#'Display PDF output
#'
#' Either data or file must be passed.
#' @param data  The PDF data as a raw vector
#' @param file  The path to a PDF file or a connection
#' @param width  The width to display the image
#' @param height  The height to display the image
#' @param defer  Do not call \link{display1}, but return prepared data instead
#' 
#' @return If \code{defer} is TRUE, a list with the components:
#' \itemize{
#'  \item{mimetype}{\code{'application/pdf'}}
#'  \item{content}{Base 64 encoded PDF data}
#'  \item{metadata}{A list with \code{width} and \code{height} if given as parameters}
#' }
#' 
#' @importFrom base64enc base64encode
#' @export
display_pdf <- function(data = NULL, file = NULL, width = NULL, height = NULL, defer = FALSE) {
    display_binary_image('application/pdf', data, file, width, height, defer)
}

#'Display SVG output
#'
#' Either data or file must be passed.
#' @param data  The SVG data as character vector
#' @param filename  The path to a SVG file
#' @param width  The width to display the image
#' @param height  The height to display the image
#' @param defer  Do not call \link{display1}, but return prepared data instead
#' 
#' @return If \code{defer} is TRUE, a list with the components:
#' \itemize{
#'  \item{mimetype}{\code{'image/svg+xml'}}
#'  \item{content}{SVG code as character string}
#'  \item{metadata}{A list with \code{width} and \code{height} if given as parameters}
#' }
#' 
#' @export
display_svg <- function(data = NULL, file = NULL, width = NULL, height = NULL, defer = FALSE) {
    if (!is.null(file)) {
        stopifnot(is.null(data))
        data <- readChar(file, file.info(file)$size)
    }
    
    metadata = namedlist()
    if (!is.null(width))  metadata$width  <- width
    if (!is.null(height)) metadata$height <- height
    
    if (!defer)
        display1('image/svg+xml', data, metadata)
    else
        list(mimetype = 'image/svg+xml', content = data, metadata = metadata)
}
