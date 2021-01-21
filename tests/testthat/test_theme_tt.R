context("theme_tt")
library("ggplot2")
library("ttr")


test_that("default", {
  theme_default <- ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, color = factor(cyl))) +
                    geom_point(size = 2) +
                      labs(
                      title = "Car weight vs efficiency",
                      subtitle = "Using sensible metrics",
                      x = "Efficiency (km/l)",
                      y = "Weight (1000 kg)",
                      color = "Cylinders",
                      caption = "Brought to you by the letter 'T'"
                      ) +
                    theme_tt() +
                    scale_color_tt()

  vdiffr::expect_doppelganger("theme_default", theme_default)
})


test_that("style", {
  theme_style <- ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, color = factor(cyl))) +
                   geom_point(size = 2) +
                   labs(
                    title = "Car weight vs efficiency",
                    subtitle = "Using sensible metrics",
                    x = "Efficiency (km/l)",
                    y = "Weight (1000 kg)",
                    color = "Cylinders",
                    caption = "Brought to you by the letter 'T'"
                    ) +
                    theme_tt(style = "paper") +
                    scale_color_tt()

  vdiffr::expect_doppelganger("theme_style", theme_style)
})

test_that("palette", {
  theme_palette <- ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, color = factor(cyl))) +
                   geom_point(size = 2) +
                   labs(
                    title = "Car weight vs efficiency",
                    subtitle = "Using sensible metrics",
                    x = "Efficiency (km/l)",
                    y = "Weight (1000 kg)",
                    color = "Cylinders",
                    caption = "Brought to you by the letter 'T'"
                    ) +
                    theme_tt(palette = "brown") +
                    scale_color_tt()    

  vdiffr::expect_doppelganger("theme_palette", theme_palette)
})

test_that("legend", {
  theme_legend <- ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, color = factor(cyl))) +
                   geom_point(size = 2) +
                   labs(
                    title = "Car weight vs efficiency",
                    subtitle = "Using sensible metrics",
                    x = "Efficiency (km/l)",
                    y = "Weight (1000 kg)",
                    color = "Cylinders",
                    caption = "Brought to you by the letter 'T'"
                    ) +
                    theme_tt(legend = TRUE) +
                    scale_color_tt() 

  vdiffr::expect_doppelganger("theme_legend", theme_legend)
})

test_that("font_scale", {
  theme_font_scale <- ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, color = factor(cyl))) +
                       geom_point(size = 2) +
                       labs(
                        title = "Car weight vs efficiency",
                        subtitle = "Using sensible metrics",
                        x = "Efficiency (km/l)",
                        y = "Weight (1000 kg)",
                        color = "Cylinders",
                        caption = "Brought to you by the letter 'T'"
                        ) +
                        theme_tt(font_scale = "large") +
                        scale_color_tt()  

  vdiffr::expect_doppelganger("theme_font_scale", theme_font_scale)
})








