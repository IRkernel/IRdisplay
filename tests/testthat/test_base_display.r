context('base_display')

test_that('original base_display raises a warning', {
	expect_warning(base_display(NULL), regexp = 'can only be used from')
})

test_that('base_display uses supplied functions', {
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
	
	on.exit({
		options(jupyter.base_display_func = old_base_display)
	})
	
	options(jupyter.base_display_func = test_display_func)
	base_display(NULL)
	expect_equal(get_last_data(), list(NULL, NULL))
})
