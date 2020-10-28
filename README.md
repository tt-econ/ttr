# Commonly used R functions for ttecon team
<!-- badges: start -->
[![R build status](https://github.com/ttecon/ttr/workflows/R-CMD-check/badge.svg)](https://github.com/ttecon/ttr/actions)
[![Codecov test coverage](https://codecov.io/gh/ttecon/ttr/branch/main/graph/badge.svg)](https://codecov.io/gh/ttecon/ttr?branch=main)
<!-- badges: end -->

## Overview

The `ttr` package includes commonly used R functions for ttecon team.


## Installation

```R
   # install.packages("devtools")
   devtools::install_github('https://github.com/ttecon/ttr')
   library(ttr)
```

## Maintenance

The production cycle is as follows:

1. Code is stored in the folder `/R`. There can be no subfolders.
2. To load for interactive testing in RStudio, use `devtools`:

      ```R
         # install.packages("devtools")
         devtools::load_all()
      ```

3. Unit tests are stored in `/tests/testthat` (file names should start with `test`) with supplementary files in other subfolders of `/tests`. The `testthat` package is bundled with `devtools`. Comparing test figures is done using `vdiffr`. In RStudio:

      ```R
         # install.packages("vdiffr")
         devtools::test()
      ```

4. For a thorough check of the package, there are two options:
   1. In RStudio:

         ```R
            devtools::check()
         ```

      This will also update all documentations.

   2. On the command line:

         ```bash
            R CMD build [path-to-package]
            R CMD check [resulting-tarball-file]
         ```

5. Code style is enforced using `styler`.

      ```R
         # install.packages("styler")
         styler::style_pkg()
      ```

6. Documentation is via `roxygen2`, provided through `devtools`.

      ```R
         devtools::document()
      ```

## Functionality

### `ggplot` support functions

- `geom_split_violin()` for a split version of `geom_violin()` which can be useful to compare variable distributions by group (for example, treatment and control groups)
- `ttcolor()` and `ttpalette()` to access ttecon colors and color palettes
- `scale_color_tt()` and `scale_fill_tt()` to use with `ggplot()`
- `theme_tt()` for figures in papers and slides
