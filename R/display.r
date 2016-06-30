#' Display data by mimetype, with optional alternative representations.
#'
#' @param data      A named list mapping mimetypes to content (base64 encoded for binary data)
#' @param metadata  A named list mapping mimetypes to named lists of specific metadata
#' @export
publish_mimebundle <- function(data, metadata = NULL) {
    getOption('jupyter.base_display_func')(data, metadata)
}

#' Display an object using all available reprs
#'
#' @param obj            The object to be displayed
#' @export
display <- function(obj) {
    bundle <- prepare_mimebundle(obj)
    publish_mimebundle(bundle$data, bundle$metadata)
}

#' Create a mimebundle for multiple reprs
#' 
#' @param obj            The object to create representations for displayed
#' @param mimetypes      Mimetypes to create reprs for
#' @param metadata       Metadata to attach to the result (can be expanded by additional metadata)
#' @param error_handler  Function used when errors in individual reprs occur
#' @return A list with the two items \code{data} (a list mapping mimetype to character) and \code{metadata} (mapping mimetype to lists of metadata)
#' 
#' @importFrom repr mime2repr
#' @export
prepare_mimebundle <- function(
    obj,
    mimetypes = getOption('jupyter.display_mimetypes'),
    metadata = NULL,
    error_handler = stop
) {
    if (length(mimetypes) == 0L)
        stop('option jupyter.display_mimetypes may not be NULL or of length 0')
    
    outer_handler <- if (identical(error_handler, stop)) stop else function(e) {}
    
    # Use withCallingHandlers as that shows the inner stacktrace:
    # https://stackoverflow.com/questions/15282471/get-stack-trace-on-trycatched-error-in-r
    # the tryCatch is  still needed to prevent the error from showing
    # up outside further up the stack :-/
    data <- filter_map(mimetypes, function(mime) {
        tryCatch(withCallingHandlers({
            rpr <- mime2repr[[mime]](obj)
            if (is.null(rpr)) return(NULL)
            prepare_content(is.raw(rpr), rpr)
        }, error = error_handler),
        error = outer_handler)
    })
    
    list(data = data, metadata = isolate_full_html(data, metadata))
}

#' @importFrom base64enc base64encode
prepare_content <- function(isbinary, data = NULL, file = NULL) {
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
    data <- list()
    data[[mimetype]] <- content
    publish_mimebundle(data, metadata)
}
