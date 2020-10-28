#' ttecon theme
#'
#' Theme for ttecon figures.
#'
#' @param style Change the font family.
#' Defaults to "slide" for TeX Gyre Heros
#' Change to "paper" for Latin Modern Roman
#' @param palette Change the palette
#' Defaults to "green" for green-shaded lines
#' Change to "brown" for brown-shaded lines
#' @param xaxis x-axis line if `TRUE`
#' Defaults to `TRUE`
#' The line is colored "green" for a green palette and is colored "brown" for a brown palette.
#' @param yaxis y-axis line if `TRUE`
#' Defaults to `FALSE`
#' The line is colored "green" for a green palette and is colored "brown" for a brown palette.
#' @param xgrid Turn on major x-grid-lines if `TRUE`
#' Defaults to `FALSE`
#' The line is colored "gradient green" for a green palette and is colored "gradient brown" for a brown palette.
#' @param ygrid Turn on major y-grid-lines if `TRUE`
#' Defaults to `TRUE`
#' The line is colored "gradient green" for a green palette and is colored "gradient brown" for a brown palette.
#' @param axis_color Defines the color for axis titles, lines and text.
#' If `NULL` then use the palette color if style is "slide" and black otherwise.
#' Defaults to `NULL`
#' @param axis_title_just axis title font justification, one of `[blmcrt]`
#' Defaults to `"rt"`
#' @param axis_title allow axis titles if `TRUE`
#' Defaults to `TRUE`
#' @param legend allow legend if `TRUE`
#' Defaults to `TRUE`
#'
#' @return ggplot theme
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, colour = factor(cyl))) +
#'   geom_point(size = 2) +
#'   labs(
#'     title = "Car weight vs efficiency",
#'     subtitle = "Using sensible metrics",
#'     x = "Efficiency (km/l)",
#'     y = "Weight (1000 kg)",
#'     colour = "Cylinders",
#'     caption = "Brought to you by the letter 'T'"
#'   ) +
#'   theme_tt(style = "paper") +
#'   scale_color_tt()
#' @export
theme_tt <- function(style = "slide", palette = "green",
                     xaxis = TRUE, yaxis = FALSE, axis_color = NULL,
                     xgrid = FALSE, ygrid = TRUE, legend = TRUE,
                     axis_title = TRUE, axis_title_just = "rt") {
  if (style == "slide") {
    font <- "heros"
  } else {
    font <- "lmroman"
  }

  if (palette == "green") {
    shaded_gray <- ttcolor("gradient green")
    if (is.null(axis_color) & style == "slide") axis_color <- ttcolor("green")
  } else {
    shaded_gray <- ttcolor("gradient brown")
    if (is.null(axis_color) & style == "slide") axis_color <- ttcolor("brown")
  }

  tttheme <- ggplot2::theme(

    # Text format:
    # This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(
      family = font,
      size = 14,
      face = "bold",
      color = "#000000"
    ),
    # This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 13,
      margin = ggplot2::margin(5, 0, 5, 0)
    ),
    plot.caption = ggplot2::element_text(
      hjust = 1, size = 10,
      margin = ggplot2::margin(t = 10),
      family = font, face = "italic"
    ),

    # Legend format
    # This sets the position and alignment of the legend,
    # removes a title and backround for it and sets the requirements for any text within the legend.
    # The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = font,
      size = 12,
      color = "#000000"
    ),

    # Axis format
    # This sets the text font, size and colour for the axis test,
    # as well as setting the margins and removes lines and ticks.
    # In some cases, axis lines and axis ticks are things we would want to have in the chart.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      family = font,
      size = 12,
      color = axis_color
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Grid lines
    # This removes all minor gridlines and adds major y gridlines.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = shaded_gray, size = 0.15),
    panel.grid.major.x = ggplot2::element_blank(),

    # Blank background
    # This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),

    # Strip background (#This sets the panel background for facet-wrapped plots to white,
    # removing the standard grey ggplot background colour and sets the title size of the facet-wrap title)
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 12, hjust = 0),
    strip.placement = "outside"
  )

  if (xaxis) tttheme <- tttheme + ggplot2::theme(axis.line.x = ggplot2::element_line(color = axis_color, size = 0.21))
  if (yaxis) tttheme <- tttheme + ggplot2::theme(axis.line.y = ggplot2::element_line(color = axis_color, size = 0.21))

  if (xgrid) tttheme <- tttheme + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = shaded_gray, size = 0.15))
  if (!ygrid) tttheme <- tttheme + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  if (!legend) tttheme <- tttheme + ggplot2::theme(legend.position = "none")

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  if (axis_title) {
    tttheme <- tttheme + ggplot2::theme(axis.title = ggplot2::element_text(size = 11, family = font, color = axis_color)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, face = "plain")) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, face = "plain"))
  }

  tttheme
}
