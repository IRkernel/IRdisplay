#' IRdisplay options
#' 
#' Some \link{options} to control the formats \code{\link{display}} and \code{\link{prepare_mimebundle}} emit,
#' and the function they use to display them.
#' 
#' @section Options:
#' 
#' \describe{
#' 
#' \item{\code{jupyter.display_mimetypes}}{
#'  The default is all MIME types supported by Jupyter.
#' }
#' \item{\code{jupyter.base_display_func}}{
#' 	Function used by \code{\link{display}} and all \code{\link{display_<text>}} / \code{\link{display_<image>}} functions.
#' 	Has the signature \code{function(data, metadata = NULL)}.
#' 	Per default emits a \code{\link{warning}}, and is set when running an \code{IRkernel}.
#' }
#' 
#' }
#' 
#' @rdname IRdisplay-options
#' @aliases IRdisplay-options
#' @export
irdisplay_option_defaults <- list(
    jupyter.display_mimetypes = c(
        'text/plain',
        'text/html',
        'text/markdown',
        'text/latex',
        
        'application/json',
        'application/javascript',
        
        'application/geo+json',
        'application/vdom.v1+json',
        'application/vnd.plotly.v1+json',
        'application/vnd.vegalite.v2+json',
        'application/vnd.vega.v4+json',
        
        'application/pdf',
        'image/png',
        'image/jpeg',
        'image/svg+xml'),
    jupyter.base_display_func = function(data, metadata = NULL) {
        warning('IRdisplay can only be used from the IPython R kernel and R magic.')
    })

.onLoad <- function(libname = NULL, pkgname = NULL) {
    for (option in names(irdisplay_option_defaults)) {
        if (is.null(getOption(option))) {
            do.call(options, irdisplay_option_defaults[option])  # single []: preserve name
        }
    }
}
