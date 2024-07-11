#' Epicentre color palettes
#'
#' Color palettes for Epicentre
#'
#' @format ## `epicentre_pal`
#' A data frame with 10 rows and 4 columns:
#' \describe{
#'   \item{palette}{Name of palette}
#'   \item{palette_type}{type of palette: sequential/qualitative/divergent}
#'   \item{color_name}{Name of the color}
#'   \item{hex}{Hex code of the color}
#'   ...
#' }
"epicentre_pal"

#' List all available palettes
#'
#' List the names of all palettes available in the package
#'
#' @param type The type of colour palette. One of: "qualitative", "sequential", "diverging".
#'
#' @return A vector of character strings of all available palettes.
#'
#' @examples
#' list_palettes()
#'
#' @rdname list_palettes
#' @export
list_palettes <- function(type) {

  # Checks for type
  if(missing(type)) {
    type <- c("qualitative", "sequential", "diverging")
  } else if(!type %in% c("qualitative", "sequential", "diverging")) {
    stop("Palette type invalid - choose one of: qualitative, sequential, or diverging")
  }

  # Get palettes
  pals <- unique(
    epithemes::epicentre_pal[epithemes::epicentre_pal$palette_type %in% type,
                  c("palette", "palette_type")]
  )

  out <- sort(pals$palette)

  return(out)

}

#' Create a palette
#'
#' Makes a discrete colour palette of length \code{n} from the chosen palette.
#'
#' @param palette The name of the chosen palette. Choices are from: \code{"epicentre-main"}.
#' @param n The integer length of the desired palette. Default value is the length of the specified palette.
#' @param interpolate Boolean. Indicates whether sequential or diverging palettes should interpolate between colours; this argument is not available for qualitative palettes. Default value is TRUE.
#'
#' @return A vector of colours.
#'
#' @examples
#' epithemes::epipal(palette = "epicentre-main", n = 6)
#'
#' @rdname epicentre_pal
#' @export
epipal <- function(palette, n, interpolate = TRUE) {

  # Checks for palette name
  if(!palette %in% epithemes::epicentre_pal$palette) {
    stop("Palette not found - check palette name.")
  }

  # Load basic palette
  pal <- epithemes::epicentre_pal$hex[epithemes::epicentre_pal$palette == palette]
  pal_type <- unique(epithemes::epicentre_pal$palette_type[epithemes::epicentre_pal$palette == palette])

  # Checks for n
  if(missing(n)) {
    n <- length(pal)
  } else if(!is.numeric(n) | (is.numeric(n) & n/floor(n) != 1)) {
    stop("Parameter n is not an integer - assign integer value to n.")
  }

  # Checks for interpolate
  if(pal_type == "qualitative") {
    interpolate <- FALSE
  }

  # Make the palette!
  if(interpolate) {
    out <- grDevices::colorRampPalette(pal)(n)
  } else {
    out <- pal[1:n]
  }

  structure(out, class = "palette", name = palette)

}

#' Print a palette
#'
#' Prints the chosen palette of length \code{n}.
#'
#' @param palette The name of the chosen palette. Choices are from: \code{"epicentre-main"}.
#' @param n The integer length of the desired palette. Default value is the length of the specified palette.
#'
#' @return A plot showing the colours of the chosen colour palette.
#'
#' @importFrom graphics image
#'
#' @rdname print_palette
#' @examples
#' print_palette(palette = "epicentre-main")
#' print_palette(palette = "epicentre-main", n = 5)
#' print_palette(palette = "epicentre-main", n = 5)
#' @export
print_palette <- function(palette, n) {

  col <- epithemes::epipal(palette = palette, n = n)

  image(1:length(col), 1, as.matrix(1:length(col)), col = col,
        main = palette, cex.main = 2,
        ylab = "", xlab = " ", xaxt = "n", yaxt = "n",  bty = "n")

}

#' Extract Hex value from a palette
#'
#' @param palette The name of the palette
#' @param color_name The name of the color to extract
#'
#' @return a length 1 names vector of color_name and the according hex value
#' @rdname get_hex
#' @export
#'
#' @examples
#' epithemes::get_hex("epicentre-main", "primary")
#' @export
get_hex <- function(palette, color_name) {

  col_info <- epithemes::epicentre_pal[epithemes::epicentre_pal$palette == palette & epithemes::epicentre_pal$color_name == color_name, ]

  val <- col_info$hex

  names(val) <- col_info$color_name

  return(val)
}

#' Make a ggplot colour scale
#'
#' Makes a discrete colour scale of length \code{n} from the chosen palette.
#'
#' @param palette The name of the chosen palette. Choices are from: \code{"epicentre-main"}.
#' @param n The integer length of the desired palette. Default value is the length of the specified palette.
#'
#' @return A discrete scale to use for colour in ggplot.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = iris,
#' aes(x = Petal.Length, y = Petal.Width, col = Species)) +
#' geom_point(size = 3) +
#' epithemes::scale_color_epi(palette = "epicentre-main", n = 3)
#'
#' @importFrom ggplot2 ggplot aes scale_color_manual
#'
#' @rdname scale_color_epi
#' @export
scale_color_epi <- function(palette, n) {

  out <- ggplot2::scale_color_manual(values = epithemes::epipal(palette = palette, n = n))

  return(out)

}

#' Make a ggplot fill scale
#'
#' Makes a discrete fill scale of length \code{n} from the chosen palette.
#'
#' @param palette The name of the chosen palette. Choices are from: \code{"epicentre-main"}.
#' @param n The integer length of the desired palette. Default value is the length of the specified palette.
#'
#' @return A discrete scale to use for fill in ggplot.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = iris,
#' aes(x = Species, y = Petal.Width, fill = Species)) +
#' geom_violin() +
#' epithemes::scale_fill_epi(palette = "epicentre-main", n = 3)
#'
#' @importFrom ggplot2 ggplot aes scale_fill_manual
#'
#' @rdname scale_fill_epi
#' @export
scale_fill_epi <- function(palette, n) {

  out <- ggplot2::scale_fill_manual(values = epithemes::epipal(palette = palette, n = n))

  return(out)

}
