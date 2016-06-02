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
    jupyter.base_display_func = function(data,  metadata = NULL) {
        if ('text/plain' %in% names(data)) {
            cat(data[['text/plain']])
        } else warning(
            'IRdisplay is designed to work with an IRkernel or R magic. ',
            'Non-textual data cannot be displayed in an unknown environment, ',
            'and we only have the following mime types:\n',
            paste(names(data), collapse = ', '))
    }
)

.onLoad <- function(libname = NULL, pkgname = NULL) {
    for (option in names(opt.defaults)) {
        if (is.null(getOption(option))) {
            do.call(options, opt.defaults[option])  # single []: preserve name
        }
    }
}
