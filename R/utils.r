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

# add an "isolate" flag to <html>-containing data
isolate_full_html <- function(metadata, content, mime) {
    if (!identical(mime, 'text/html') || !grepl('<html.*>', content, ignore.case = TRUE))
        return(metadata)
    
    if (is.null(metadata)) metadata <- list()
    if (is.null(metadata[[mime]])) metadata[[mime]] <- list()
    
    metadata[[mime]]$isolated <- TRUE
    metadata
}

