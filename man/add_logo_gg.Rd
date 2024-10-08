% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes.R
\name{add_logo_gg}
\alias{add_logo_gg}
\title{Add a logo to ggplot}
\usage{
add_logo_gg(gg, logo_name, position = "bottom-left", x = NULL, y = NULL, size)
}
\arguments{
\item{gg}{a ggplot object}

\item{logo_name}{`chr` a valid logo name. List available at `epithemes::list_logo()`}

\item{position}{`chr`. A quick way to position the label. options are `c("bottom-right", "bottom-left", "top-left", "top-right")`}

\item{x}{`num` If `position` does not suit you, use `x` and `y` args to specify the logo position in `npc` units}

\item{y}{`num` If `position` does not suit you, use `x` and `y` args to specify the logo position in `npc` units}

\item{size}{`num` width size of the logo in `lines`. Aspect ratio is maitained.}
}
\value{
a ggplot object with a logo at the right position
}
\description{
Add a logo to ggplot
}
\examples{
library(ggplot2)
gg_plot <- ggplot(data = iris) +
  geom_histogram(aes(x = Sepal.Length)) +
  epitheme_gg()

  add_logo_gg(gg_plot,
  "epicentre_msf_transparent.png",
  size = 10,
  position = "bottom-left"
  )

}
