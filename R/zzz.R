.onAttach <- function(libname, pkgname) {
  if (!"heros" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "heros",
      regular = system.file("fonts", "TeX-Gyre-Heros", "texgyreheros-regular.otf", package = "ttr"),
      bold = system.file("fonts", "TeX-Gyre-Heros", "texgyreheros-bold.otf", package = "ttr"),
      italic = system.file("fonts", "TeX-Gyre-Heros", "texgyreheros-italic.otf", package = "ttr"),
      bolditalic = system.file("fonts", "TeX-Gyre-Heros", "texgyreheros-bolditalic.otf", package = "ttr"),
    )
  }
  if (!"hero-condensed" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "heros-condensed",
      regular = system.file("fonts", "TeX-Gyre-Heros", "texgyreheroscn-regular.otf", package = "ttr"),
      bold = system.file("fonts", "TeX-Gyre-Heros", "texgyreheroscn-bold.otf", package = "ttr"),
      italic = system.file("fonts", "TeX-Gyre-Heros", "texgyreheroscn-italic.otf", package = "ttr"),
      bolditalic = system.file("fonts", "TeX-Gyre-Heros", "texgyreheroscn-bolditalic.otf", package = "ttr"),
    )
  }
  if (!"lmroman" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "lmroman",
      regular = system.file("fonts", "Latin-Modern", "lmroman10-regular.otf", package = "ttr"),
      bold = system.file("fonts", "Latin-Modern", "lmroman10-bold.otf", package = "ttr"),
      italic = system.file("fonts", "Latin-Modern", "lmroman10-italic.otf", package = "ttr"),
      bolditalic = system.file("fonts", "Latin-Modern", "lmroman10-bolditalic.otf", package = "ttr"),
    )
  }

  showtext::showtext_auto()
}
