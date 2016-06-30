context('jupyter.base_display_func')

test_that('original jupyter.base_display_func raises a warning', {
	expect_warning(getOption('jupyter.base_display_func')(NULL), regexp = 'can only be used from')
})
