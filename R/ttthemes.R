#' ttecon theme
#'
#' Theme for ttecon figures.
#'
#' @param style Change the theme style.
#' Default to `"slide"` for default slide fonts and themed axis title and text.
#' Change to `"paper"` for default paper fonts and black axis title and text.
#' @param palette Change the palette.
#' Default to `"green"` for green-shaded lines.
#' Change to `"brown"` for brown-shaded lines.
#' @param font Change the general font family.
#' Default to `NULL` to use the defaults based on `style`.
#' @param title_font Change the title font family.
#' Default to `NULL` to use the defaults based on `style`.
#' @param subtitle_font Change the subtitle font family.
#' Default to `NULL` to use the defaults based on `style`.
#' @param xaxis x-axis line if `TRUE`.
#' Default to `TRUE`,
#' The line is colored "green" for a green palette and is colored "brown" for a brown palette.
#' @param yaxis y-axis line if `TRUE`.
#' Default to `FALSE`.
#' The line is colored "green" for a green palette and is colored "brown" for a brown palette.
#' @param xgrid Turn on major x-grid-lines if `TRUE`.
#' Default to `FALSE`.
#' The line is colored "gradient green" for a green palette and is colored "gradient brown" for a brown palette.
#' @param ygrid Turn on major y-grid-lines if `TRUE`.
#' Default to `TRUE`.
#' The line is colored "gradient green" for a green palette and is colored "gradient brown" for a brown palette.
#' @param xgrid_minor Turn on minor x-grid-lines if `TRUE`.
#' Default to `FALSE`.
#' The line is colored "gradient green" for a green palette and is colored "gradient brown" for a brown palette.
#' @param ygrid_minor Turn on minor y-grid-lines if `TRUE`.
#' Default to `FALSE`.
#' The line is colored "gradient green" for a green palette and is colored "gradient brown" for a brown palette.
#' @param axis_text_color define the color style for axis text.
#' If `"style"` then use a saturated palette color.
#' Default to `"style"`.
#' @param axis_title_color define the color style for axis title.
#' If `"style"` then use a saturated palette color.
#' Default to `"style"`.
#' @param axis_title_just Axis title font justification, one of `[blmcrt]`
#' Default to `"mm"`.
#' @param axis_title Allow axis titles if `TRUE`.
#' Default to `TRUE`.
#' @param legend Allow legend if `TRUE`.
#' Default to `FALSE`.
#' @param legend_title Allow legend title if `TRUE`.
#' Default to `FALSE`.
#' @param xticks Add x-ticks if `TRUE`.
#' Default to `FALSE`.
#' @param yticks Add y-ticks if `TRUE`.
#' Default to `FALSE`.
#'
#' @return ggplot theme
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, color = factor(cyl))) +
#'   geom_point(size = 2) +
#'   labs(
#'     title = "Car weight vs efficiency",
#'     subtitle = "Using sensible metrics",
#'     x = "Efficiency (km/l)",
#'     y = "Weight (1000 kg)",
#'     color = "Cylinders",
#'     caption = "Brought to you by the letter 'T'"
#'   ) +
#'   theme_tt(style = "paper") +
#'   scale_color_tt()
#' @export
theme_tt <- function(style = "slide", palette = "green",
                     font = NULL, title_font = NULL, subtitle_font = NULL,
                     xaxis = TRUE, yaxis = FALSE,
                     axis_text_color = "style", axis_title_color = "style",
                     xgrid = FALSE, ygrid = TRUE,
                     xgrid_minor = FALSE, ygrid_minor = FALSE,
                     legend = FALSE, legend_title = FALSE,
                     axis_title = TRUE, axis_title_just = "mm",
                     xticks = FALSE, yticks = FALSE) {
  if (is.null(font)) {
    if (style == "slide") {
      font <- "source-sans-pro"
    } else {
      font <- "lmroman"
    }
  }

  if (is.null(title_font)) {
    if (style == "slide") {
      title_font <- "alegreya-sans"
      title_face <- "bold"
    } else {
      title_font <- "lmroman"
      title_face <- "bold"
    }
  }

  if (is.null(subtitle_font)) {
    if (style == "slide") {
      subtitle_font <- "alegreya-sans"
    } else {
      subtitle_font <- "lmroman"
    }
  }

  if (palette == "green") {
    shaded_gray <- ttcolor("gradient green")
    axis_color <- ttcolor("green")
    if (axis_text_color == "style") axis_text_color <- shades::saturation(ttcolor("green"), 0.21)
    if (axis_title_color == "style") axis_title_color <- shades::saturation(ttcolor("green"), 0.07)
  } else {
    shaded_gray <- ttcolor("gradient brown")
    axis_color <- ttcolor("brown")
    if (axis_text_color == "style") axis_text_color <- shades::saturation(ttcolor("brown"), 0.21)
    if (axis_title_color == "style") axis_title_color <- shades::saturation(ttcolor("brown"), 0.07)
  }

  if (style == "paper") {
    axis_text_color <- "black"
    axis_title_color <- "black"
  }

  tttheme <- ggplot2::theme(

    # Text format:
    # This sets the font, size, type and color of text for the chart's title
    plot.title = ggplot2::element_text(
      family = title_font, face = title_face,
      size = 14,
      color = "#000000"
    ),
    # This sets the font, size, type and color of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(
      family = subtitle_font, face = "plain",
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
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = font,
      size = 11,
      color = "#000000"
    ),

    # Axis format
    # This sets the text font, size and color for the axis test,
    # as well as setting the margins and removes lines and ticks.
    # In some cases, axis lines and axis ticks are things we would want to have in the chart.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      family = font,
      size = 12,
      color = axis_text_color
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0)),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Grid lines
    # This removes all minor gridlines and adds major y gridlines.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = shaded_gray, size = 0.21),
    panel.grid.major.x = ggplot2::element_blank(),

    # Blank background
    # This sets the panel background as blank, removing the border and standard grey ggplot background color from the plot
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),

    # Strip background (#This sets the panel background for facet-wrapped plots to white,
    # removing the standard grey ggplot background color and sets the title size of the facet-wrap title)
    strip.background = ggplot2::element_rect(fill = "white", color = "black"),
    strip.text = ggplot2::element_text(size = 11, hjust = 0.5, family = font),
    strip.placement = "outside",
    panel.spacing = grid::unit(-0.25, "lines")
  )

  if (xaxis) tttheme <- tttheme + ggplot2::theme(axis.line.x = ggplot2::element_line(color = axis_color, size = 0.21))
  if (yaxis) tttheme <- tttheme + ggplot2::theme(axis.line.y = ggplot2::element_line(color = axis_color, size = 0.21))

  if (xgrid) tttheme <- tttheme + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = shaded_gray, size = 0.21))
  if (!ygrid) tttheme <- tttheme + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

  if (xgrid_minor) tttheme <- tttheme + ggplot2::theme(panel.grid.minor.x = ggplot2::element_line(color = shaded_gray, size = 0.15))
  if (ygrid_minor) tttheme <- tttheme + ggplot2::theme(panel.grid.minor.y = ggplot2::element_line(color = shaded_gray, size = 0.15))

  if (!legend) tttheme <- tttheme + ggplot2::theme(legend.position = "none")

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  if (axis_title) {
    tttheme <- tttheme + ggplot2::theme(axis.title = ggplot2::element_text(size = 11, family = font, color = axis_title_color)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, face = "plain")) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, face = "plain")) +
      ggplot2::theme(axis.title.y.right = ggplot2::element_text(hjust = yj, angle = 90, face = "plain"))
  }
  if (xticks) {
    tttheme <- tttheme + ggplot2::theme(axis.ticks.x = ggplot2::element_line(color = axis_title_color, size = 0.21))
    tttheme <- tttheme + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  if (yticks) {
    tttheme <- tttheme + ggplot2::theme(axis.ticks.y = ggplot2::element_line(color = axis_title_color, size = 0.21))
    tttheme <- tttheme + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  if (legend_title) {
    tttheme <- tttheme + ggplot2::theme(legend.title = ggplot2::element_text(
      family = font,
      size = 11,
      hjust = 0.5
    ))
  }

  tttheme
}
