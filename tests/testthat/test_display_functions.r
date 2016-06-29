context('display functions')
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

old_base_display <- getOption('jupyter.base_display_func')
old_rich <- getOption('jupyter.rich_display')

on.exit({
    options(jupyter.base_display_func = old_base_display)
    options(jupyter.rich_display = old_rich)
})

options(jupyter.base_display_func = test_display_func)
options(jupyter.rich_display = FALSE)


test_that('publish_mimebundle works', {
    publish_mimebundle(NULL)
    expect_equal(get_last_data(), list(NULL, NULL))
    publish_mimebundle(1, 1)
    expect_equal(get_last_data(), list(1, 1))
})

test_that('display works', {
    exp <- namedlist()
    # NULL only displays in plain/text
    display(NULL)
    exp[['text/plain']] <- repr_text(NULL)
    expect_equal(get_last_data(), list(exp, NULL))
    # This displays in everything, but just test it with html and plain text
    withr::with_options(
        list(jupyter.display_mimetypes = c('text/plain', 'text/html')),
        display(1)
    )
    exp[['text/plain']] <- repr_text(1)
    exp[['text/html']] <- repr_html(1)
    res = get_last_data()
    expect_equal(res, list(exp, NULL))
})

test_that('display_raw works', {
    exp <- namedlist()
    display_raw('text/plain', FALSE, 'data', NULL)
    exp[['text/plain']] <- 'data'
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display_json works', {
    exp <- namedlist()
    display_json('data')
    exp[['application/json']] <- 'data'
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display_javascript works', {
    exp <- namedlist()
    display_javascript('data')
    exp[['application/javascript']] <- 'data'
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display_html works', {
    exp <- namedlist()
    display_html('data')
    exp[['text/html']] <- 'data'
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display_markdown works', {
    exp <- namedlist()
    display_markdown('data')
    exp[['text/markdown']] <- 'data'
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display_latex works', {
    exp <- namedlist()
    display_latex('data')
    exp[['text/latex']] <- 'data'
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display_png works', {
    exp <- namedlist()
    exp_metadata = namedlist()
    display_png(charToRaw('data'))
    exp['image/png'] <- base64encode(charToRaw('data'))
    expect_equal(get_last_data(), list(exp, exp_metadata))
    display_png(charToRaw('data'), width = 1, height = 2)
    exp_metadata$width  <- 1
    exp_metadata$height <- 2
    expect_equal(get_last_data(), list(exp, exp_metadata))
})

test_that('display_jpeg works', {
    exp <- namedlist()
    exp_metadata = namedlist()
    display_jpeg(charToRaw('data'))
    exp['image/jpeg'] <- base64encode(charToRaw('data'))
    expect_equal(get_last_data(), list(exp, exp_metadata))
    display_jpeg(charToRaw('data'), width = 1, height = 2)
    exp_metadata$width  <- 1
    exp_metadata$height <- 2
    expect_equal(get_last_data(), list(exp, exp_metadata))
})

test_that('display_pdf works', {
    exp <- namedlist()
    display_pdf(charToRaw('data'))
    exp['application/pdf'] <- base64encode(charToRaw('data'))
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display_svg works', {
    exp <- namedlist()
    display_svg('data')
    exp['image/svg+xml'] <- 'data'
    expect_equal(get_last_data(), list(exp, NULL))
})