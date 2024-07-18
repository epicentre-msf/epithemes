#' Epurated Ggplot theme (inspired from hrbrthemes)
#'
#' @param base_family base font family
#' @param base_size base font size
#' @param plot_title_family,plot_title_size,plot_title_face,plot_title_margin plot title family, face, size and margins
#' @param subtitle_family,subtitle_size,subtitle_face,subtitle_margin plot subtitle family, face, size and margins
#' @param strip_text_family,strip_text_size,strip_text_face Facet label family, face, size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_text_size font size of axis text
#' @param axis_title_family,axis_title_size,axis_title_face axis title font family, face and size
#' @param axis_title_just axis title font justification
#' @param plot_margin plot margin (specify with ggplot2::margin())
#' @param legend_position `chr` legend position on plot. One of `c("top", "bottom", "left", "right", "none")`
#' @param grid_col,axis_col grid & axis colors; both default to ⁠#cccccc⁠
#' @param grid panel grid (TRUE, FALSE, or a combination of X, x, Y, y)
#' @param axis add x or y axes? TRUE, FALSE, "xy"
#' @param ticks ticks if TRUE add ticks
#'
#' @return a ggplot theme
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(data = iris) +
#'   geom_histogram(aes(x = Sepal.Length)) +
#'   epitheme_gg()
#'
epitheme_gg <- function(
    base_family = "sans",
    base_size = 10,
    plot_title_family = base_family,
    plot_title_size = 14,
    plot_title_face = "bold",
    plot_title_margin = 10,
    subtitle_family = base_family,
    subtitle_size = 9,
    subtitle_face = "italic",
    subtitle_margin = 15,
    strip_text_family = base_family,
    strip_text_size = 6,
    strip_text_face = "plain",
    caption_family = base_family,
    caption_size = 9,
    caption_face = "italic",
    caption_margin = 10,
    axis_text_size = base_size,
    axis_title_family = subtitle_family,
    axis_title_size = 10,
    axis_title_face = "plain",
    axis_title_just = "rt",
    plot_margin = ggplot2::margin(10, 10, 10, 10),
    legend_position = "right",
    grid_col = "#cccccc",
    grid = TRUE,
    axis_col = "#cccccc",
    axis = FALSE,
    ticks = FALSE
) {
  ret <- ggplot2::theme_minimal(
    base_family = base_family,
    base_size = base_size
  ) +

    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )

  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + ggplot2::theme(
      panel.grid = ggplot2::element_line(
        color = grid_col,
        linewidth = 0.2
      ),
      panel.grid.major = ggplot2::element_line(
        color = grid_col,
        linewidth = 0.2
      ),
      panel.grid.minor = ggplot2::element_line(
        color = grid_col,
        linewidth = 0.15
      )
    )

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) {
        ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      }
      if (regexpr("Y", grid)[1] < 0) {
        ret <- ret + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      }
      if (regexpr("x", grid)[1] < 0) {
        ret <- ret + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      }
      if (regexpr("y", grid)[1] < 0) {
        ret <- ret + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
      }
    }
  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(
      color = "#2b2b2b",
      size = 0.15
    ))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(
          color = axis_col,
          linewidth = 0.15
        ))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(
          color = axis_col,
          linewidth = 0.15
        ))
      }
    } else {
      ret <- ret + ggplot2::theme(
        axis.line.x = ggplot2::element_line(
          color = axis_col,
          linewidth = 0.15
        ),
        axis.line.y = ggplot2::element_line(
          color = axis_col,
          linewidth = 0.15
        )
      )
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }
  if (!ticks) {
    ret <- ret + ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  } else {
    ret <- ret + ggplot2::theme(
      axis.ticks = ggplot2::element_line(size = 0.15),
      axis.ticks.x = ggplot2::element_line(size = 0.15),
      axis.ticks.y = ggplot2::element_line(size = 0.15),
      axis.ticks.length = grid::unit(5, "pt")
    )
  }

  xj <- switch(
    tolower(substr(axis_title_just, 1, 1)),
    b = 0,
    l = 0,
    m = 0.5,
    c = 0.5,
    r = 1,
    t = 1
  )
  yj <- switch(
    tolower(substr(axis_title_just, 2, 2)),
    b = 0,
    l = 0,
    m = 0.5,
    c = 0.5,
    r = 1,
    t = 1
  )

  ret <- ret + ggplot2::theme(

    legend.position = legend_position,
    axis.text.x = ggplot2::element_text(
      size = axis_text_size,
      margin = ggplot2::margin(t = 0)
    ),
    axis.text.y = ggplot2::element_text(
      size = axis_text_size,
      margin = ggplot2::margin(r = 0)
    ),
    axis.title = ggplot2::element_text(
      size = axis_title_size,
      family = axis_title_family
    ),
    axis.title.x = ggplot2::element_text(
      hjust = xj,
      size = axis_title_size,
      family = axis_title_family,
      face = axis_title_face
    ),
    axis.title.y = ggplot2::element_text(
      hjust = yj,
      size = axis_title_size,
      family = axis_title_family,
      face = axis_title_face
    ),
    axis.title.y.right = ggplot2::element_text(
      hjust = yj,
      size = axis_title_size,
      angle = 90,
      family = axis_title_family,
      face = axis_title_face
    ),
    strip.text = ggplot2::element_text(
      hjust = 0,
      size = strip_text_size,
      face = strip_text_face,
      family = strip_text_family
    ),
    panel.spacing = grid::unit(2, "lines"),
    plot.title = ggplot2::element_text(
      hjust = 0,
      size = plot_title_size,
      margin = ggplot2::margin(b = plot_title_margin),
      family = plot_title_family,
      face = plot_title_face
    ),
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      size = subtitle_size,
      margin = ggplot2::margin(b = subtitle_margin),
      family = subtitle_family,
      face = subtitle_face
    ),
    plot.caption = ggplot2::element_text(
      hjust = 1,
      size = caption_size,
      margin = ggplot2::margin(t = caption_margin),
      family = caption_family,
      face = caption_face
    ),
    plot.margin = plot_margin
  )
}

# Gt theme ----------------------------------------------------------------

#' Table styling for gt table
#'
#' @param table a table
#' @param convert_gt `lgl` Convert the `table` to gt ?. Default is `TRUE`
#' @param source_note `chr` footnote to add to the table
#' @param fmt_percent `var` variables to format to percentages
#' @param pct_decimal `num` decimals for percentages
#' @param fmt_na `chr` Markdown string to format `NA` values
#' @param stripped `lgl` Show stripped rows?
#' @return a gt object formatted
#' @export
#'
#'
#' @examples
#'
#' iris|>
#'
#' dplyr::summarise(
#'   .by = Species,
#'   n = dplyr::n(),
#'   max_sepal = max(Sepal.Length, na.rm = TRUE),
#'   n_setosa = sum(Species == "setosa", na.rm = TRUE),
#'   setosa_pct = round(digits = 2, (n_setosa / n) * 100)
#' ) |>
#'
#'   epitheme_gt() |>
#'
#'   gt::tab_footnote("This dataset is pretty boring")
#'
#' @import gt dplyr
#' @export
#'

epitheme_gt <- function(table,
                        convert_gt = TRUE,
                        source_note = NULL,
                        fmt_percent = NULL,
                        pct_decimal = 1,
                        fmt_na =  "*-*",
                        stripped = TRUE) {

  if(convert_gt){gt <- gt <- gt::gt(table) } else { gt <- table}

  gt <- gt |>

    # style of col labels
    gt::tab_style(
      style = list(
        gt::cell_text(
          weight = "bold",
          size = "14px",
          align = "center"
        )
      ),
      locations = gt::cells_column_labels()
    ) |>
    # style of body cells
    gt::tab_style(
      style = list(
        gt::cell_text(
          size = "14px",
          align = "center"
        ),
        gt::cell_borders(
          sides = "all",
          color = "white"
        )
      ),
      locations = gt::cells_body()
    ) |>
    # style of footnotes
    gt::tab_style(
      style = list(gt::cell_text(
        size = "12px",
        style = "italic"
      )),
      locations = gt::cells_footnotes()
    ) |>
    # style source notes
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        style = "italic",
        align = "right",
        size = "11px"
      )
    ) |>

    gt::fmt_percent(fmt_percent, decimals = pct_decimal) |>

    #style missing values
    gt::sub_missing(missing_text = gt::md(fmt_na)) |>

    #disable quarto modification
    gt::tab_options(quarto.disable_processing = TRUE,
                    #column_labels.border.top.color = "#2E4473",
                    #column_labels.border.bottom.color = "#2E4473",
                    #table.border.bottom.color = "#2E4473",
                    #table_body.border.bottom.color = "#2E4473"

                    )

  if(stripped){
    gt <- gt |>
      #strip table
      gt::tab_style(style = cell_fill(color = "#A1B8CF", alpha = .2),
                    locations = cells_body(rows = seq(1, nrow(table), 2)))
  }

  if(length(source_note)) {gt <- gt |> gt::tab_source_note(gt::md(source_note)) }

  return(gt)

}

#' List available logos
#'
#' @return character vector of logos names
#' @export
#'
#' @examples
#' list_logo()
list_logo <- function(){ list.files("inst/logos")}

#' Visualise a logo
#'
#' @param logo_name Name of a logo. Available logos are found with list_logo()
#'
#' @return plots a rastergrob grob of given logo
#' @export
#'
#' @examples
view_logo <- function(logo_name){

  if( !(logo_name %in% epithemes::list_logo())) {stop("Invalid logo_name, use epithemes::list_logo() to see available logos")}

  img <- png::readPNG(here::here("inst", "logos", logo_name))

  grid::grid.newpage()
  grid::grid.raster(img)

}

#' Add a logo to ggplot
#'
#' @param gg a ggplot object
#' @param logo_name `chr` a valid logo name. List available at `epithemes::list_logo()`
#' @param position `chr`. A quick way to position the label. options are `c("bottom-right", "bottom-left", "top-left", "top-right")`
#' @param x `num` If `position` does not suit you, use `x` and `y` args to specify the logo position in `npc` units
#' @param y `num` If `position` does not suit you, use `x` and `y` args to specify the logo position in `npc` units
#' @param size `num` width size of the logo in `lines`. Aspect ratio is maitained.
#'
#' @return a ggplot object with a logo at the right position
#' @export
#'
#' @examples
#' library(ggplot2)
#' gg_plot <- ggplot(data = iris) +
#'   geom_histogram(aes(x = Sepal.Length)) +
#'   epitheme_gg()

add_logo_gg <- function(gg,
                        logo_name,
                        position = "bottom-left",
                        x = NULL,
                        y = NULL,
                        size
){
  match.arg(position, c("bottom-right", "bottom-left", "top-left", "top-right"))

  if( !(logo_name %in% epithemes::list_logo())) {stop("Invalid logo_name, use epithemes::list_logo() to see available logos")}

  pos <- data.frame(position = c("bottom-left", "bottom-right", "top-left", "top-right"),
                    x = c(ggplot2::unit(.1, "npc"), ggplot2::unit(.9, "npc"), ggplot2::unit(.1, "npc"), ggplot2::unit(.9, "npc")),
                    y = c(ggplot2::unit(-.08, "npc"), ggplot2::unit(-.08, "npc"), ggplot2::unit(1.05, "npc"), ggplot2::unit(1.05, "npc"))
  )

  if(!is.null(x) & is.null(y) ) { stop("Please provide a y value")}
  if(!is.null(y) & is.null(x) ) { stop("Please provide a x value")}

  if(!is.null(x)){

    if(y > 0 ){
      top <- 3
      bottom <- 1

    } else {
      top <- 1
      bottom <- 3
    }

    x_val <- ggplot2::unit(x, "npc")
    y_val <- ggplot2::unit(y, "npc")

  } else {

    if(grepl("top", position)){ top <- 3 } else { top <- 1}
    if(grepl("bottom", position)){ bottom <- 3 } else { bottom <- 1}

    x_val <- pos[pos$position == position,]$x
    y_val <- pos[pos$position == position,]$y

  }

  width <- ggplot2::unit(size, "lines")

  logo <- grid::rasterGrob(png::readPNG(here::here("inst", "logos", logo_name) ),
                           interpolate = TRUE,
                           x = x_val,
                           y = y_val,
                           width = width
  )

  gg +
    ggplot2::theme(plot.margin = ggplot2::unit(c(top, 1, bottom, 1), "lines")) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::annotation_custom(logo)

}
