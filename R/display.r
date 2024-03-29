#' Display data by mimetype, with optional alternative representations.
#' 
#' \code{publish_mimebundle} calls the function stored as option value of \code{jupyter.base_display_func},
#' \code{clear_output} calls the value of option \code{jupyter.clear_output_func}. (see: \link{IRdisplay-options})
#' 
#' @param data      A named list mapping mimetypes to content (\code{\link[base]{character}} or \code{\link[base]{raw} vectors})
#' @param metadata  A named list mapping mimetypes to named lists of metadata, e.g. \code{list('image/png' = list(width = 5))}
#' @param wait      Wait to clear the output until new output is available. Default \code{TRUE}.
#'                  If \code{FALSE}, clears the existing output immediately before the new output is displayed.
#' 
#' @seealso \code{\link{prepare_mimebundle}}
#' 
#' @examples \dontrun{## (Run inside of an IRkernel)
#' publish_mimebundle(list('text/html' = '<h1>Hi!</h1>'))
#' publish_mimebundle(
#'   list('image/svg+xml' = '<svg xmlns="http://www.w3.org/2000/svg"><circle r="100"/></svg>'),
#'   list('image/svg+xml' = list(width = 100, height = 100)))
#' 
#' for (i in 1:5) {
#'   Sys.sleep(.2)    # simulate work
#'   clear_output()   # clear previous iteration
#'   cat(i)           # alternative: IRdisplay::display(i)
#'   flush.console()  # make output available
#' }}
#' 
#' @export
publish_mimebundle <- function(data, metadata = NULL) {
    getOption('jupyter.base_display_func')(data, metadata)
}

#' @describeIn publish_mimebundle Clear the output from the current cell.
#' @export
clear_output <- function(wait = TRUE) {
    getOption('jupyter.clear_output_func')(wait)
}

#' Create and use multiple available reprs
#' 
#' Both functions create a mimebundle for multiple reprs.
#' \code{display} proceeds to publish it using \code{\link{publish_mimebundle}}.
#' \code{prepare_mimebundle} returns it (see \emph{Value} for details)
#' 
#' @param obj            The object to create representations for
#' @param mimetypes      Mimetypes to create reprs for. The defaults are defined by the option \code{jupyter.display_mimetypes}. (see: \link{IRdisplay-options})
#' @param metadata,...   Metadata to attach to the result (can be expanded by additional metadata)
#' @param error_handler  Function used when errors in individual reprs occur
#' 
#' @return \code{prepare_mimebundle} returns a list with items corresponding to the parameters of \code{\link{publish_mimebundle}} (\code{data} and \code{metadata})
#' 
#' @seealso \code{\link{publish_mimebundle}}
#' 
#' @examples
#' bundle <- prepare_mimebundle(diag(3))
#' 
#' \dontrun{## (Run inside of an IRkernel)
#' display(help(display))}
#' 
#' @name display
#' @export
display <- function(
    obj,
    ...,
    mimetypes = getOption('jupyter.display_mimetypes'),
    error_handler = stop
) {
    metadata <- list(...)
    if (length(metadata) == 0L) metadata <- NULL
    bundle <- prepare_mimebundle(obj, mimetypes, metadata, error_handler)
    publish_mimebundle(bundle$data, bundle$metadata)
}


#' @importFrom repr mime2repr
#' @rdname display
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
            if (! mime %in% names(repr::mime2repr)) stop('No repr_* for mimetype ', mime, ' in repr::mime2repr')
            rpr <- repr::mime2repr[[mime]](obj)
            if (is.null(rpr)) return(NULL)
            prepare_content(is.raw(rpr), rpr)
        }, error = error_handler),
        error = outer_handler)
    })
    
    list(data = data, metadata = isolate_full_html(data, metadata))
}

prepare_content <- function(isbinary, data = NULL, file = NULL) {
    if (is.null(file) == is.null(data))
        stop('Either need to specify data or file, but not both')
    
    if (is.null(file)) {
        if (isbinary) {
            if (!is.raw(data)) stop('Data needs to be a raw vector')
        }
        data 
    } else {
        read_all(file, isbinary)
    }
}

display_raw <- function(mimetype, isbinary, data, file, metadata = NULL) {
    content <- prepare_content(isbinary, data, file)
    data <- list()
    data[[mimetype]] <- content
    publish_mimebundle(data, metadata)
}
