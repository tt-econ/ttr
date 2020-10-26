context("geom_split_violin")
library("ggplot2")
library("ttr")

set.seed(20160229)
my_data <- data.frame(
  y = c(rnorm(1000), rnorm(1000, 0.5), rnorm(1000, 1), rnorm(1000, 1.5)),
  x = c(rep("a", 2000), rep("b", 2000)),
  m = c(rep("i", 1000), rep("j", 2000), rep("i", 1000))
)

test_that("normal", {
  plot <- ggplot(my_data, aes(x, y, fill = m)) +
    geom_split_violin() + theme_minimal(base_family = "heros")
  vdiffr::expect_doppelganger("rplots", plot)
})

test_that("title", {
  normal_title <- ggplot(my_data, aes(x, y, fill = m)) +
    labs(title = "graph of split plot") +
    geom_split_violin() + theme_minimal(base_family = "heros")
  vdiffr::expect_doppelganger("normal_title", normal_title)
})

test_that("color", {
  normal_color <- ggplot(my_data, aes(x, y, fill = m)) +
    scale_fill_brewer(palette = "Greens") +
    geom_split_violin() + theme_minimal(base_family = "heros")
  vdiffr::expect_doppelganger("normal_color", normal_color)
})

test_that("axis", {
  normal_axis <- ggplot(my_data, aes(x, y, fill = m)) +
    labs(x = "New X", y = "New Y") +
    geom_split_violin() + theme_minimal(base_family = "heros")
  vdiffr::expect_doppelganger("normal_axis", normal_axis)
})

test_that("legend", {
  normal_legend <- ggplot(my_data, aes(x, y, fill = m)) +
    geom_split_violin() + theme_minimal(base_family = "heros") +
    scale_fill_discrete(name = "Title", labels = c("A", "B"))
  vdiffr::expect_doppelganger("normal_legend", normal_legend)
})
