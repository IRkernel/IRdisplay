context('general display functions')
library(repr)

last_data <- NULL
test_display_func <- function(data, metadata = NULL) {
    last_data <<- list(data, metadata)
}

get_last_data <- function() {
    ret <- last_data
    last_data <<- NULL
    ret
}

withr::with_options(list(jupyter.base_display_func = test_display_func), {

test_that('publish_mimebundle works', {
    publish_mimebundle(NULL)
    expect_equal(get_last_data(), list(NULL, NULL))
    
    publish_mimebundle(1, 1)
    expect_equal(get_last_data(), list(1, 1))
})

test_that('display works', {
    # NULL only displays in text/plain
    exp <- list('text/plain' = repr_text(NULL))
    display(NULL)
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display only creates the jupyter.display_mimetypes', {
    exp <- list(
        'text/plain' = repr_text(1),
        'text/html'  = repr_html(1))
    
    withr::with_options(
        list(jupyter.display_mimetypes = c('text/plain', 'text/html')),
        display(1))
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display attaches metadata', {
    exp <- list(
        'text/plain' = '<html>x</html>',
        'text/html'  = '<html>x</html>')
    exp_md <- list('text/html' = list(isolated = TRUE))
    
    withr::with_options(list(jupyter.display_mimetypes = c('text/plain', 'text/html')), {
        .GlobalEnv$repr_text.html_literal <- .GlobalEnv$repr_html.html_literal <- function(obj, ...) unclass(obj)
        display(structure('<html>x</html>', class = 'html_literal'))
    })
    expect_equal(get_last_data(), list(exp, exp_md))
})

test_that('display handles raw data', {
    exp <- list(
        'text/plain' = 'PNG data: 01 02 03 04 05...',
        'image/png' = 'AQIDBAUGBwgJCg==')
    
    withr::with_options(list(jupyter.display_mimetypes = c('text/plain', 'image/png')), {
        .GlobalEnv$repr_text.raw_png <- function(obj, ...) sprintf('PNG data: %s...', paste(obj[1:5], collapse = ' '))
        .GlobalEnv$repr_png.raw_png <- function(obj, ...) unclass(obj)
        display(structure(as.raw(1:10), class = 'raw_png'))
    })
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display needs > 0 display_mimetypes', {
    withr::with_options(
        list(jupyter.display_mimetypes = character(0L)),
        expect_error(display(1), 'may not be NULL or of length 0'))
    
    withr::with_options(
        list(jupyter.display_mimetypes = NULL),
        expect_error(display(1), 'may not be NULL or of length 0'))
})

test_that('display_raw works', {
    exp <- list('text/plain' = 'data')
    
    display_raw('text/plain', FALSE, 'data', NULL)
    expect_equal(get_last_data(), list(exp, NULL))
})

})
