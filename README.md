IRdisplay [![b-Travis]][Travis] [![b-CRAN]][CRAN]
=========

[b-Travis]: https://travis-ci.com/IRkernel/IRdisplay.svg?branch=master "Build status"
[Travis]: https://travis-ci.com/IRkernel/IRdisplay
[b-CRAN]: https://www.r-pkg.org/badges/version/IRdisplay "Comprehensive R Archive Network"
[CRAN]: https://cran.r-project.org/package=IRdisplay

IRdisplay is a front-end package for Jupyter.
It’s automatically integrated into [IRkernel][] when you open a Jupyter notebook using that kernel.

The primary, high level functions are:

```r
display(obj, ..., mimetypes=<from option>, error_handler=stop)

display_png(data = NULL, file = NULL, width = NULL, height = NULL)
# display_jpeg(…); display_pdf(…); display_svg(…)

display_html(data = NULL, file = NULL)
# display_javascript(…), display_json(…), display_markdown(…), display_latex(…)
```

Use `display` to display an object in all configured mime types (see **Configuration**),
and the `display_*` functions to display raw data you have in form of a file or a variable.

Manual use is possible via the `*_mimebundle` functions:

```r
prepare_mimebundle(obj, mimetypes=<from option>, metadata=NULL, error_handler=stop)
publish_mimebundle(data, metadata = NULL)
```

Where `prepare_mimebundle` uses `repr` to create a mimebundle containing representations of objects,
and `publish_mimebundle` sends such mimebundles to Jupyter.

[IRkernel]: https://irkernel.github.io/running/

Configuration
-------------

You can add your own mime types that should be displayed via:

```r
options(jupyter.display_mimetypes = union(getOption('jupyter.display_mimetypes'), ...))
```

If you want to create your own kernel reacting to `display` / `publish_mimebundle` calls, you can use:

```r
options(jupyter.base_display_func = function(data, metadata = NULL) ...)
```
