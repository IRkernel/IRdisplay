# maps a function and return a named list with the NULL results removed
filter_map <- function(x, f, simplify = FALSE)
    Filter(Negate(is.null), sapply(x, f, simplify = simplify))

# create metadata bundle for images
img_metadata <- function(width, height) {
    if (is.null(width) && is.null(height))
        return(NULL)
    
    metadata <- list()
    if (!is.null(width))  metadata$width  <- width
    if (!is.null(height)) metadata$height <- height
    metadata
}

# add an "isolate" flag to <html>-containing data, or the unchanged metadata
isolate_full_html <- function(data, metadata = NULL) {
    if (!('text/html' %in% names(data)) || !isTRUE(grepl('<html.*>', data[['text/html']], ignore.case = TRUE)))
        return(metadata)
    
    if (is.null(metadata)) metadata <- list()
    if (is.null(metadata[['text/html']])) metadata[['text/html']] <- list()
    
    metadata[['text/html']]$isolated <- TRUE
    metadata
}

#' @importFrom methods is
read_all <- function(file, isbinary) {
    read <- if (isbinary)
        function(s) readBin(file, 'raw', s)
    else
        function(s) readChar(file, s)
    
    size <- tryCatch(file.info(file)$size, error = function(e) NULL)
    
    if (!is.null(size)) {
        read(file, size)
    } else {
        if (is.character(file)) file <- base::file(file, 'rb')
        stopifnot(is(file, 'connection'))
        if (isOpen(file)) open(file)
        rv <- if (isbinary) raw() else character()
        chunk <- read(1024)
        while (length(chunk) > 0) {
            rv <- c(rv, chunk)
            chunk <- read(1024)
        }
        rv
    }
}
