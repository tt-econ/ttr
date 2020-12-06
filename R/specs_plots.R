#' Draw a coefficient plot.
#'
#' `specs_coef_plot` produces the coefficient plot in a specification plot or standalone.
#'
#' @param coef_grid A dataframe with the following components:
#'  `index` (if part of a specification plot) or `coef_name` (if standalone),
#'  `coef`, `error_low`, `error_high`, `p`
#'  (character with values: `"p<0.01"`, `"p<0.05"`, `"p<0.10"`, `p>0.10"`).
#' @param palette Change the palette.
#' Default to `"green"` for green-shaded plot.
#' Change to `"brown"` for brown-shaded plot.
#' @param style Change the theme style.
#' Default to `"slide"` for default slide fonts and themed axis title and text.
#' Change to `"paper"` for default paper fonts and black axis title and text.
#' @param point_size A numeric scalar indicating the size of the coefficient estimate points.
#' Default to `NULL` and set according to the number of specs.
#' @param error_geom A string indicating the type of geom that should be used to
#'  depict confidence intervals on coefficient estimates.
#' Currently supported are `"ribbon"`, `"errorbar"`, and `"none"`.
#' Default to `NULL` and set to "errorbar"` if fewer than 100 specs and to `"ribbon"` otherwise.
#' @param error_alpha A numeric scalar indicating the alpha of the error geom.
#' Default to `NULL` and set to `0.5` for `"errorbar"` and `0.21` for `"ribbon"`.
#' @param error_width A numeric scalar indicating the width of the error bar.
#' Default to 0 and only applicable when `error_geom` is set to `"errorbar"`.
#' @param coef_ylim A numeric vector of length two indicating the
#'  minimum and maximum values of the y-axis in the coefficient plot.
#' Default to `NULL` to use `ggplot2` default.
#' @param coef_ylabel A string specifying the y-axis label on the coefficient panel.
#'  Default to `"Coefficient estimate"'.
#' @param hline A numeric scalar indicating a horizontal line at value `hline`.
#'  Default to `NULL` for none.
#' @param pcolors A named vector of colors for points by `p` value ("p<0.01", "p<0.05", "p<0.10", "p>0.10").
#'  Default to `NULL` for default colors based on `ttr::ttcolors`.
#' @param standalone A boolean to indicate whether this is a standalone plot or
#'  part of a specification plot with a control grid panel.
#'  Default to `FALSE'.
#' @return A `ggplot2` object depicting the coefficient plot
#' @export
specs_coef_plot <- function(coef_grid, palette = "green", style = "slide",
                            coef_ylabel = "Coefficient estimate", coef_ylim = NULL,
                            point_size = NULL, error_alpha = NULL,
                            error_geom = NULL, error_width = 0,
                            hline = NULL,
                            pcolors = NULL,
                            standalone = FALSE) {
  ncoefs <- nrow(coef_grid)
  mygray <- shades::saturation(ttcolor(palette), 0.21)[[1]]
  if (is.null(pcolors)) {
    pcolors <- c(
      "p<0.01" = unname(ttr::ttcolor("orange")),
      "p<0.05" = unname(ttr::ttcolor("brown")),
      "p<0.10" = unname(ttr::ttcolor("green")),
      "p>0.10" = unname(ttr::ttcolor("blue")),
      mygray = mygray
    )
  } else {
    pcolors["mygray"] <- mygray
  }

  if (is.null(point_size)) {
    if (ncoefs <= 10) point_size <- 3
    if (ncoefs > 10 & ncoefs <= 60) point_size <- 2
    if (ncoefs > 60) point_size <- 1
  }

  if (is.null(error_geom)) {
    error_geom <- ifelse(ncoefs >= 100, "ribbon", "errorbar")
  }

  if (is.null(error_alpha)) {
    error_alpha <- ifelse(error_geom == "ribbon", 0.21, 0.5)
  }

  if (standalone) {
    coef_grid$x <- coef_grid$coef_name
  } else {
    coef_grid$x <- coef_grid$index
  }

  coef_plot <- ggplot2::ggplot(coef_grid, ggplot2::aes(x = coef_grid$x, y = coef_grid$coef)) +
    {
      if (!is.null(hline)) ggplot2::geom_hline(yintercept = hline, color = mygray, size = 0.21)
    } +
    {
      if (error_geom == "ribbon") {
        ggplot2::geom_ribbon(ggplot2::aes(ymin = coef_grid$error_low, ymax = coef_grid$error_high),
          alpha = error_alpha, linetype = 0, fill = mygray
        )
      } else if (error_geom == "errorbar") {
        ggplot2::geom_errorbar(ggplot2::aes(ymin = coef_grid$error_low, ymax = coef_grid$error_high),
          color = mygray, alpha = error_alpha, width = error_width
        )
      }
    } +
    ggplot2::geom_point(size = point_size, alpha = 0.85, ggplot2::aes(color = coef_grid$p)) +
    ggplot2::guides(color = F) +
    ggplot2::ylab(coef_ylabel) +
    ggplot2::scale_color_manual(values = pcolors) +
    ttr::theme_tt(palette = palette, style = style)

  if (standalone == FALSE) {
    coef_plot <- coef_plot + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )
  }

  if (!is.null(coef_ylim)) {
    coef_plot <- coef_plot +
      ggplot2::coord_cartesian(ylim = coef_ylim)
  }

  return(coef_plot)
}

#' Draw a control grid plot.
#'
#' `specs_control_plot` produces the control grid plot in a specification plot.
#'
#' @param control_grid A dataframe with `index` (of the specs), `key` (name of a spec key) and `value` (0 or 1).
#' @param palette Change the palette.
#' Default to `"green"` for green-shaded plot.
#' Change to `"brown"` for brown-shaded plot.
#' @param style Change the theme style.
#' Default to `"slide"` for default slide fonts and themed axis title and text.
#' Change to `"paper"` for default paper fonts and black axis title and text.
#' @param control_geom A string indicating the geom that should be used to
#'  indicate the presence of controls.
#' Currently supported are `"circle"` and `"rect"`.
#' Default to `"rect"`.
#' @param control_spacing A numeric scalar indicating how large the control geoms
#'  should be.
#' For `control_geom=="circle"`, this is the diameter of the circle.
#' For `control_geom=="rect"`, this is the width of the rectangle.
#' Default to `NULL` and set to `0.75` if fewer than 40 specs and `1` otherwise.
#' @param control_text_size A numeric scalar indicating how large the
#'  control name text should be.
#' Default to 9.
#' @param trim_top A numeric scalar indicating how close the bottom panel
#'  (displaying presence of controls) should be to the
#'  top panel (displaying presence of coefficients).
#' Useful when dealing with large CIs.
#' Default to `0`.
#' @return A `ggplot2` object depicting the control grid plot
#' @export
specs_control_plot <- function(control_grid, palette = "green", style = "slide",
                               control_geom = "rect",
                               control_spacing = NULL,
                               control_text_size = 9,
                               trim_top = 0) {
  control_grid$value <- as.factor(control_grid$value)
  control_grid$key <- as.factor(control_grid$key)
  control_grid$y <- -as.numeric(control_grid$key)

  ncoefs <- length(unique(control_grid$index))
  if (is.null(control_spacing)) {
    control_spacing <- ifelse(ncoefs >= 40, 1, 0.75)
  }
  half_space <- control_spacing / 2
  mygray <- shades::saturation(ttcolor(palette), 0.21)[[1]]

  control_plot <- ggplot2::ggplot(control_grid) +
    ggplot2::scale_fill_manual(values = c("#FFFFFF", mygray)) +
    ggplot2::guides(fill = F) +
    ttr::theme_tt(palette = palette, style = style, ygrid = FALSE) +
    ggplot2::theme(
      plot.margin = grid::unit(c(-trim_top, 0, 0, 0), "lines"),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = control_text_size)
    ) +
    ggplot2::scale_y_continuous(
      breaks = unique(control_grid$y),
      labels = unique(control_grid$key),
      limits = c(min(control_grid$y) - 1, max(control_grid$y) + 1)
    ) +
    ggplot2::coord_cartesian(xlim = c(1 - half_space, ncoefs + half_space))

  if (control_geom == "rect") {
    control_plot <- control_plot +
      ggplot2::geom_rect(ggplot2::aes(
        xmin = control_grid$index - half_space,
        xmax = control_grid$index + half_space,
        ymin = control_grid$y - half_space,
        ymax = control_grid$y + half_space,
        fill = control_grid$value
      ), alpha = 0.7)
  } else if (control_geom == "circle") {
    control_plot <- control_plot +
      ggforce::geom_circle(ggplot2::aes(
        x0 = control_grid$index, y0 = control_grid$y, fill = control_grid$value,
        r = half_space
      ), color = mygray, alpha = 0.5)
  }
  return(control_plot)
}

#' Combine the coefficient and control grid plots.
#'
#' `specs_combine_plots` produces the combined specification plot.
#'
#' @param coef_plot A `ggplot2` plot: coefficient plot for the upper panel.
#' @param control_plot A `ggplot2` plot: control grid plot for the lower panel.
#' @param ncoefs A numeric scalar indicating how many specifications there are.
#' @param ratio A numeric scalar indicating the height ratio of the
#'  coefficient plot relative to the control grid plot.
#' @param control_spacing A numeric scalar indicating how large the control geoms
#'  should be.
#' For `control_geom=="circle"`, this is the diameter of the circle.
#' For `control_geom=="rect"`, this is the width of the rectangle.
#' Default to `NULL` and set to `0.75` if fewer than 40 specs and 1 otherwise.
#' @return A `ggplot2` object depicting the specification plot
#' @export
specs_combine_plots <- function(coef_plot, control_plot, ncoefs, ratio = 2,
                                control_spacing = NULL) {
  if (is.null(control_spacing)) {
    control_spacing <- ifelse(ncoefs >= 40, 1, 0.75)
  }
  half_space <- control_spacing / 2

  coef_plot <- coef_plot +
    ggplot2::coord_cartesian(xlim = c(1 - half_space, ncoefs + half_space))

  specs_plot <- cowplot::plot_grid(coef_plot, control_plot,
    nrow = 2,
    align = "v", axis = "b", rel_heights = c(ratio, 1)
  )
  return(specs_plot)
}

#' Draw a specification plot.
#'
#' `specs_plot` produces the specification plot with 2 panels: coefficient and control grid.
#'
#' @param coefs A dataframe with the following components:
#'  names of indicators for specs, `coef`, `error_low`, `error_high`, `pval`
#' @param palette Change the palette.
#' Default to `"green"` for green-shaded plot.
#' Change to `"brown"` for brown-shaded plot.
#' @param style Change the theme style.
#' Default to `"slide"` for default slide fonts and themed axis title and text.
#' Change to `"paper"` for default paper fonts and black axis title and text.
#' @param ratio A numeric scalar indicating the height ratio of the
#'  coefficient plot relative to the control grid plot.
#' @param point_size A numeric scalar indicating the size of the coefficient estimate points.
#' Default to `NULL` and set according to the number of specs.
#' @param error_geom A string indicating the type of geom that should be used to
#'  depict confidence intervals on coefficient estimates.
#' Currently supported are `"ribbon"`, `"errorbar"`, and `"none"`.
#' Default to `NULL` and set to "errorbar"` if fewer than 100 specs and to `"ribbon"` otherwise.
#' @param error_alpha A numeric scalar indicating the alpha of the error geom.
#' Default to `NULL` and set to `0.5` for `"errorbar"` and `0.21` for `"ribbon"`.
#' @param error_width A numeric scalar indicating the width of the error bar.
#' Default to 0 and only applicable when `error_geom` is set to `"errorbar"`.
#' @param coef_ylim A numeric vector of length two indicating the
#'  minimum and maximum values of the y-axis in the coefficient plot.
#' Default to `NULL` to use `ggplot2` default.
#' @param coef_ylabel A string specifying the y-axis label on the coefficient panel.
#'  Default to `"Coefficient estimate"'.
#' @param hline A numeric scalar indicating a horizontal line at value `hline`.
#'  Default to `NULL` for none.
#' @param control_geom A string indicating the geom that should be used to
#'  indicate the presence of controls.
#' Currently supported are `"circle"` and `"rect"`.
#' Default to `"rect"`.
#' @param control_spacing A numeric scalar indicating how large the control geoms
#'  should be.
#' For `control_geom=="circle"`, this is the diameter of the circle.
#' For `control_geom=="rect"`, this is the width of the rectangle.
#' Default to `NULL` and set to `0.75` if fewer than 40 specs and `1` otherwise.
#' @param control_text_size A numeric scalar indicating how large the
#'  control name text should be.
#' Default to 9.
#' @param trim_top A numeric scalar indicating how close the bottom panel
#'  (displaying presence of controls) should be to the
#'  top panel (displaying presence of coefficients).
#' Useful when dealing with large CIs.
#' Default to `-1`.
#' @return A `ggplot2` object depicting the specification plot with both coefficient and control panels
#' @export
specs_plot <- function(coefs,
                       palette = "green", style = "slide", ratio = 2,
                       coef_ylabel = "Coefficient estimate", coef_ylim = NULL,
                       point_size = NULL, error_alpha = NULL,
                       error_geom = NULL, error_width = 0,
                       hline = NULL,
                       control_geom = "rect",
                       control_spacing = NULL,
                       control_text_size = 9,
                       trim_top = 0) {
  if (!"index" %in% colnames(coefs)) coefs$index <- 1:nrow(coefs)
  if ("p" %in% colnames(coefs)) coefs$p <- NULL
  prior_coef_col <- base::which(colnames(coefs) == "coef") - 1

  coefs$p <- "p>0.10"
  coefs[coefs$pval <= 0.01, "p"] <- "p<0.01"
  coefs[coefs$pval > 0.01 & coefs$pval <= 0.05, "p"] <- "p<0.05"
  coefs[coefs$pval > 0.05 & coefs$pval <= 0.1, "p"] <- "p<0.10"

  control_grid <- coefs[, 1:prior_coef_col]
  control_grid$index <- coefs$index
  control_grid <- data.table::melt(data.table::as.data.table(control_grid),
    id = "index", variable.name = "key"
  )

  coef_grid <- coefs[, c("coef", "error_low", "error_high", "index", "p")]

  coef_plot <- ttr::specs_coef_plot(coef_grid,
    palette = palette, style = style,
    coef_ylabel = coef_ylabel, coef_ylim = coef_ylim,
    point_size = point_size, error_alpha = error_alpha,
    error_geom = error_geom, error_width = error_width,
    hline = hline,
    standalone = FALSE
  )
  control_plot <- ttr::specs_control_plot(control_grid,
    palette = palette, style = style,
    control_geom = control_geom,
    control_spacing = control_spacing,
    control_text_size = control_text_size,
    trim_top = trim_top
  )
  specs_plot <- specs_combine_plots(coef_plot, control_plot, nrow(coef_grid),
    ratio = ratio,
    control_spacing = control_spacing
  )
  return(specs_plot)
}
