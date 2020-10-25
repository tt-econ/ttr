ttcolor_defn <- c(
  `black`           = "#000000",
  `green`           = "#004d3e",
  `gradient green`  = "#DBE6E3",
  `light green`     = "#009a7b",
  `pink`            = "#ff6db6",
  `light pink`      = "#ffb6db",
  `purple`          = "#490092",
  `blue`            = "#006ddb",
  `light purple`    = "#b66dff",
  `light blue`      = "#6db6ff",
  `very light blue` = "#b6dbff",
  `red`             = "#920000",
  `brown`           = "#59260b",
  `gradient brown`  = "#E7E0DC",
  `light brown`     = "#923f12",
  `orange`          = "#db6d00",
  `bright green`    = "#24ff24",
  `yellow`          = "#ffff6d",
  `white`           = "#ffffff"
)

#' Function to extract tt colors as hex codes
#'
#' Each color name will be matched with a corresponding hex code.
#' There can be repeated names.
#'
#' @param ... Character names of ttcolors
#'
#' @export
ttcolor <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(ttcolor_defn)
  }

  ttcolor_defn[cols]
}

ttpalettes <- list(
  `contrast` = ttcolor("green", "orange", "blue", "brown"),

  `main` = ttcolor("green", "blue", "orange", "brown"),

  `green` = ttcolor("green", "gradient green"),

  `brown` = ttcolor("brown", "gradient brown"),

  `light` = ttcolor("light green", "light blue", "light pink", "light purple", "light brown"),

  `light contrast` = ttcolor("light green", "light pink", "light blue", "light purple", "light brown")
)

ttpalettes[["light green"]] <- ttpalettes[["light"]]

ttpalettes[["light brown"]] <- rev(ttpalettes[["light"]])

ttpalettes[["main green"]] <- ttpalettes[["main"]]

ttpalettes[["main brown"]] <- rev(ttpalettes[["main"]])

ttpalettes[["contrast green"]] <- ttpalettes[["contrast"]]

ttpalettes[["contrast brown"]] <- rev(ttpalettes[["contrast"]])

#' ttpalette(n) function
#'
#' Return function(n) to interpolate a ttcolor palette
#'
#' @param palette Character name of palette. Run `names(ttpalettes)` for available options.
#' @param nmax Maximum number of different colors the palette should contain. If not provided, is calculated automatically from the data.
#' @param reverse If TRUE, reverses the order of the colors in the color scale
#' @param order Numeric vector listing the order in which the colors should be used. Default is 1:nmax.
#' @param ... Additional arguments to pass to grDevices::colorRampPalette()
#' @return Color palette function(n)
#' @export
#'
ttpalette <- function(palette = "main", reverse = FALSE, nmax = NULL, order = NULL, ...) {
  function(n) {
    pal <- ttpalettes[[palette]]

    len <- length(pal)
    if (is.null(nmax)) {
      nmax <- n
    }
    if (is.null(order)) {
      order <- 1:nmax
    }

    if (n > nmax) {
      warning("Insufficient values in palette. ",
        n, " needed but only ", nmax, " provided.",
        call. = FALSE
      )
    }
    if (nmax > length(pal)) {
      warning("Original color palette does not have sufficient colors. ",
        "Colors are being interpolated with grDevices::colorRampPalette.",
        call. = FALSE
      )

      pal <- grDevices::colorRampPalette(pal, ...)(nmax)
    } else {
      pal <- grDevices::colorRampPalette(pal[1:nmax], ...)(nmax)
    }
    if (reverse) pal <- rev(pal)
    pal[order]
  }
}

#' Color scale constructor for ttcolors
#'
#' To be used in a ggplot, with option `discrete` set to `TRUE` for discrete values
#'
#' @param palette Character name of palette. Run `names(ttpalettes)` for available options.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param nmax Maximum number of different colors the palette should contain. If not provided, is calculated automatically from the data.
#' @param reverse If TRUE, reverses the order of the colors in the color scale
#' @param order Numeric vector listing the order in which the colors should be used. Default is 1:nmax.
#' @param aesthetics Default to `color` for `scale_color_tt` and `fill` for `scale_fill_tt`
#' @param ... Additional arguments passed to `discrete_scale()` or
#'            `scale_color_gradientn()`, used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_color_tt <- function(palette = "main", aesthetics = "color", discrete = TRUE, reverse = FALSE, nmax = NULL, order = NULL, ...) {
  if (!palette %in% names(ttpalettes)) {
    stop(
      "Unknown palette: Palette has to be one of: ",
      paste(names(ttpalettes), collapse = ", ")
    )
  }

  pal <- ttpalette(palette = palette, reverse = reverse, nmax = nmax, order = order)
  if (discrete) {
    ggplot2::discrete_scale(aesthetics, paste0("tt", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for ttcolors
#'
#' To be used in a ggplot, with option `discrete` set to `TRUE` for discrete values
#'
#' @inheritParams scale_color_tt
#' @export
#'
scale_fill_tt <- function(palette = "main", aesthetics = "fill", discrete = TRUE, reverse = FALSE, nmax = NULL, order = NULL, ...) {
  args <- as.list(match.call())
  args[[1]] <- NULL
  if (is.null(args[["aesthetics"]])) {
    args$aesthetics <- "fill"
  }
  do.call(scale_color_tt, args)
}
