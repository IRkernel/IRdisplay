opt.defaults <- list(
    jupyter.rich_display = TRUE,
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
)

.onLoad <- function(libname = NULL, pkgname = NULL) {
    for (option in names(opt.defaults)) {
        if (is.null(getOption(option))) {
            do.call(options, opt.defaults[option])  # single []: preserve name
        }
    }
}
