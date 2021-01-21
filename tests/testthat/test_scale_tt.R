context("scale_tt")
library("ggplot2")
library("ttr")


test_that("palette", {
  scale_color_palette <- ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, color = factor(cyl))) +
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
                          scale_color_tt(palette = "light") 

  vdiffr::expect_doppelganger("scale_color_palette", scale_color_palette)
})

test_that("aesthetics", {
  scale_color_aesthetics <- ggplot(mtcars, aes(x = mpg * 0.43, y = wt * 0.4535924, color = factor(cyl))) +
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
                              scale_color_tt(aesthetics = "fill") 

  vdiffr::expect_doppelganger("scale_color_aesthetics", scale_color_aesthetics)
})






