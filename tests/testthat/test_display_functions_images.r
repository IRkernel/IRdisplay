context('image display functions')

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

test_that('display_png works', {
    dta <- as.raw(1:3)
    exp    <- list('image/png' = dta)
    exp_md <- list(width = 1, height = 2)
    
    display_png(dta)
    expect_equal(get_last_data(), list(exp, NULL))
    
    display_png(dta, width = 1, height = 2)
    expect_equal(get_last_data(), list(exp, exp_md))
})

test_that('display_jpeg works', {
    dta <- as.raw(1:3)
    exp    <- list('image/jpeg' = dta)
    exp_md <- list(width = 1, height = 2)
    
    display_jpeg(dta)
    expect_equal(get_last_data(), list(exp, NULL))
    
    display_jpeg(dta, width = 1, height = 2)
    expect_equal(get_last_data(), list(exp, exp_md))
})

test_that('display_pdf works', {
    dta <- as.raw(1:3)
    exp <- list('application/pdf' = dta)
    
    display_pdf(dta)
    expect_equal(get_last_data(), list(exp, NULL))
})

test_that('display_svg works', {
    exp <- list('image/svg+xml' = '<svg></svg>')
    
    display_svg('<svg></svg>')
    expect_equal(get_last_data(), list(exp, NULL))
})

})
