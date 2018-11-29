context('default options')

test_that('default options are set', {
    expect_equal(length(getOption('jupyter.display_mimetypes')), 15L)
})

