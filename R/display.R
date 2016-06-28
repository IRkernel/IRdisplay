# Create an empty named list
#' @importFrom stats setNames
namedlist <- function() setNames(list(), character(0))

base_display <- function(data, metadata = NULL) {
    getOption('jupyter.base_display_func')(data, metadata)
}

#' Display data by mimetype, with optional alternative representations.
#'
#' @param data  A named list mapping mimetypes to content (base64 encoded for binary data)
#' @param metadata  A named list mapping mimetypes to named lists of specific metadata
#' @export
publish_mimebundle <- function(data, metadata=NULL) {
    base_display(data, metadata)
}

#' Display an object using any available reprs
#'
#' @param obj  The object to be displayed
#' @importFrom repr mime2repr repr_text
#' @export
display <- function (obj) {
    data <- namedlist()
    if (getOption('jupyter.rich_display')) {
        for (mime in getOption('jupyter.display_mimetypes')) {
            r <- mime2repr[[mime]](obj)
            if (!is.null(r)) data[[mime]] <- r
        }
    } else {
        data[['text/plain']] <- repr_text(obj)
    }
    publish_mimebundle(data)
}

#' @importFrom base64enc base64encode
prepare_content <- function(isbinary, data, file) {
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
    content <- prepare_content(isbinary, data, file)
    bundle <- list()
    bundle[[mimetype]] <- content
    # Isolating full html pages (putting them in an iframe)
    if (identical(mimetype, 'text/html')) {
        if (grepl('<html.*>', content, ignore.case = TRUE)) {
            if (is.null(metadata)) {
                metadata <- namedlist()
            }
            html_md <- metadata[[mimetype]]
            if (is.null(html_md)) {
                html_md <- list(isolated = TRUE)
            } else {
                html_md[['isolated']] <- TRUE
            }
            metadata[[mimetype]] <- html_md
        }
    }
    publish_mimebundle(bundle, metadata)
}

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

#' Display PNG output
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

#' Display JPEG output
#'
#' Either data or file must be passed.
#' @param data  The JPEG data as a raw vector
#' @param file  The path to a JPEG file or a connection
#' @param width  The width to display the image
#' @param height  The height to display the image
#' 
#' @export
display_jpeg <- function(data = NULL, file = NULL, width = NULL, height = NULL) {
    metadata = namedlist()
    if (!is.null(width))  metadata$width  <- width
    if (!is.null(height)) metadata$height <- height
    
    display_raw('image/jpeg', TRUE, data, file, metadata)
}

#' Display PDF output
#'
#' Either data or file must be passed.
#' @param data  The PDF data as a raw vector
#' @param file  The path to a PDF file or a connection
#' 
#' @export
display_pdf <- function(data = NULL, file = NULL) display_raw('application/pdf', TRUE, data, file)

#' Display SVG output
#'
#' Either data or file must be passed.
#' @param data  The SVG data as character vector
#' @param file  The path to a SVG file
#' 
#' @export
display_svg <- function(data = NULL, file = NULL) display_raw('image/svg+xml', FALSE, data, file)
