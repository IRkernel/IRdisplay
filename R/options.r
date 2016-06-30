opt.defaults <- list(
    jupyter.display_mimetypes = c(
        'text/plain',
        'text/html',
        'text/markdown',
        'text/latex',
        
        'application/json',
        'application/javascript',
        
        'application/pdf',
        'image/png',
        'image/jpeg',
        'image/svg+xml'),
    jupyter.base_display_func = function(data, metadata = NULL) {
        warning('IRdisplay can only be used from the IPython R kernel and R magic.')
    })

.onLoad <- function(libname = NULL, pkgname = NULL) {
    for (option in names(opt.defaults)) {
        if (is.null(getOption(option))) {
            do.call(options, opt.defaults[option])  # single []: preserve name
        }
    }
}
