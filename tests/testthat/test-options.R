context("default options")

test_that("If n = 0, lead and lag return x", {
  expect_true(getOption("jupyter.rich_display"))
  expect_equal(length(getOption("jupyter.display_mimetypes")), 10)
})

