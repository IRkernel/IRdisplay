#' Display data by mimetype, with optional alternative representations.
#'
#' @param data      A named list mapping mimetypes to content (base64 encoded for binary data)
#' @param metadata  A named list mapping mimetypes to named lists of specific metadata
#' @export
publish_mimebundle <- function(data, metadata = NULL) {
    getOption('jupyter.base_display_func')(data, metadata)
}

#' Display an object using any available reprs
#'
#' @param obj  The object to be displayed
#' @importFrom repr mime2repr repr_text
#' @importFrom stats setNames
#' @export
display <- function(obj) {
    mimes <- getOption('jupyter.display_mimetypes')
    if (length(mimes) == 0L)
        stop('option jupyter.display_mimetypes may not be NULL or of length 0')
    data <- filter_map(mimes, function(mime) mime2repr[[mime]](obj))
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
    
    # Special case: modify metadata if the content is a complete HTML document
    metadata <- isolate_full_html(metadata, content, mimetype)
    
    publish_mimebundle(bundle, metadata)
}
