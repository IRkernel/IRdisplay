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
    res <- get_last_data()
    expect_equal(res, list(exp, NULL))
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
