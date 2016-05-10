context('default options')

test_that('default options are set', {
  expect_true(getOption('jupyter.rich_display'))
  expect_equal(length(getOption('jupyter.display_mimetypes')), 10)
})

