#' Display a specific image output
#'
#' Either \code{data} or \code{file} must be passed.
#' 
#' @param data    The data as a \code{\link[base]{raw}} vector (\code{\link[base]{character}} vector for \code{display_svg})
#' @param file    The path to a file or a \code{\link[base]{connection}} containing the content
#' @param width   The width to display the image
#' @param height  The height to display the image
#' 
#' @seealso \code{\link{display_<text>}}
#' 
#' @examples \dontrun{## (Run inside of an IRkernel)
#' display_png(file = 'image.png')
#' display_svg('
#' <svg xmlns="http://www.w3.org/2000/svg" viewBox="-1 -1 2 2">
#'   <circle r="1"/>
#' </svg>
#' ')
#' display_jpeg(file = url('https://dummyimage.com/600x400.jpg', 'wb'), width = 100)}
#' 
#' @name display_<image>
#' @rdname display-images
NULL

#' @rdname display-images
#' @export
display_png <- function(data = NULL, file = NULL, width = NULL, height = NULL)
    display_img('image/png', TRUE, data, file, width, height)

#' @rdname display-images
#' @export
display_jpeg <- function(data = NULL, file = NULL, width = NULL, height = NULL)
    display_img('image/jpeg', TRUE, data, file, width, height)

#' @rdname display-images
#' @export
display_pdf <- function(data = NULL, file = NULL, width = NULL, height = NULL)
    display_img('application/pdf', TRUE, data, file, width, height)

#' @rdname display-images
#' @export
display_svg <- function(data = NULL, file = NULL, width = NULL, height = NULL)
    display_img('image/svg+xml', FALSE, data, file, width, height)

display_img <- function(mime, binary, data, file, width, height) 
    display_raw(mime, binary, data, file, img_metadata(mime, width, height))
