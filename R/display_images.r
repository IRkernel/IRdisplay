#' Display PNG output
#'
#' Either data or file must be passed.
#' @param data    The PNG data as a raw vector
#' @param file    The path to a PNG file or a connection
#' @param width   The width to display the image
#' @param height  The height to display the image
#' 
#' @export
display_png <- function(data = NULL, file = NULL, width = NULL, height = NULL)
    display_raw('image/png', TRUE, data, file, img_metadata(width, height))

#' Display JPEG output
#'
#' Either data or file must be passed.
#' @param data    The JPEG data as a raw vector
#' @param file    The path to a JPEG file or a connection
#' @param width   The width to display the image
#' @param height  The height to display the image
#' 
#' @export
display_jpeg <- function(data = NULL, file = NULL, width = NULL, height = NULL)
    display_raw('image/jpeg', TRUE, data, file, img_metadata(width, height))

#' Display PDF output
#'
#' Either data or file must be passed.
#' @param data    The PDF data as a raw vector
#' @param file    The path to a PDF file or a connection
#' @param width   The width to display the document
#' @param height  The height to display the document
#' 
#' @export
display_pdf <- function(data = NULL, file = NULL, width = NULL, height = NULL)
    display_raw('application/pdf', TRUE, data, file, img_metadata(width, height))

#' Display SVG output
#'
#' Either data or file must be passed.
#' @param data    The SVG data as character vector
#' @param file    The path to a SVG file or a connection
#' @param width   The width to display the image
#' @param height  The height to display the image
#' 
#' @export
display_svg <- function(data = NULL, file = NULL, width = NULL, height = NULL)
    display_raw('image/svg+xml', FALSE, data, file, img_metadata(width, height))
