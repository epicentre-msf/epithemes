% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palettes.R
\docType{data}
\name{epicentre_pal}
\alias{epicentre_pal}
\alias{epipal}
\title{Epicentre color palettes}
\format{
## `epicentre_pal`
A data frame with 10 rows and 4 columns:
\describe{
  \item{palette}{Name of palette}
  \item{palette_type}{type of palette: sequential/qualitative/divergent}
  \item{color_name}{Name of the color}
  \item{hex}{Hex code of the color}
  ...
}
}
\usage{
epicentre_pal

epipal(palette, n, interpolate = TRUE)
}
\arguments{
\item{palette}{The name of the chosen palette. Choices are from: \code{"main"}.}

\item{n}{The integer length of the desired palette. Default value is the length of the specified palette.}

\item{interpolate}{Boolean. Indicates whether sequential or diverging palettes should interpolate between colours; this argument is not available for qualitative palettes. Default value is TRUE.}
}
\value{
A vector of colours.
}
\description{
Color palettes for Epicentre

Makes a discrete colour palette of length \code{n} from the chosen palette.
}
\examples{
epithemes::epipal(palette = "main", n = 6)

}
\keyword{datasets}
