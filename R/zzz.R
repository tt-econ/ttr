.onAttach <- function(libname, pkgname) {
  if (!"tgheros" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "tgheros",
      regular = system.file("fonts", "TeX-Gyre-Heros", "texgyreheros-regular.otf", package = "ttr"),
      bold = system.file("fonts", "TeX-Gyre-Heros", "texgyreheros-bold.otf", package = "ttr"),
      italic = system.file("fonts", "TeX-Gyre-Heros", "texgyreheros-italic.otf", package = "ttr"),
      bolditalic = system.file("fonts", "TeX-Gyre-Heros", "texgyreheros-bolditalic.otf", package = "ttr"),
    )
  }
  if (!"tgheros-condensed" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "tgheros-condensed",
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
      symbol = system.file("fonts", "Latin-Modern", "lmromancaps10-regular.otf", package = "ttr")
    )
  }
  if (!"source-sans-pro" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "source-sans-pro",
      regular = system.file("fonts", "Source-Sans-Pro", "SourceSansPro-Regular.ttf", package = "ttr"),
      bold = system.file("fonts", "Source-Sans-Pro", "SourceSansPro-SemiBold.ttf", package = "ttr"),
      italic = system.file("fonts", "Source-Sans-Pro", "SourceSansPro-Italic.ttf", package = "ttr"),
      bolditalic = system.file("fonts", "Source-Sans-Pro", "SourceSansPro-SemiBoldItalic.ttf", package = "ttr"),
    )
  }
  if (!"alegreya-sans" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "alegreya-sans",
      regular = system.file("fonts", "Alegreya-Sans", "AlegreyaSans-Regular.ttf", package = "ttr"),
      bold = system.file("fonts", "Alegreya-Sans", "AlegreyaSans-Bold.ttf", package = "ttr"),
      italic = system.file("fonts", "Alegreya-Sans", "AlegreyaSans-Italic.ttf", package = "ttr"),
      bolditalic = system.file("fonts", "Alegreya-Sans", "AlegreyaSans-BoldItalic.ttf", package = "ttr"),
    )
  }
  if (!"alegreya-sans-sc" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "alegreya-sans-sc",
      regular = system.file("fonts", "Alegreya-Sans-SC", "AlegreyaSansSC-Regular.ttf", package = "ttr"),
      bold = system.file("fonts", "Alegreya-Sans-SC", "AlegreyaSansSC-Bold.ttf", package = "ttr"),
      italic = system.file("fonts", "Alegreya-Sans-SC", "AlegreyaSansSC-Italic.ttf", package = "ttr"),
      bolditalic = system.file("fonts", "Alegreya-Sans-SC", "AlegreyaSansSC-BoldItalic.ttf", package = "ttr"),
      symbol = system.file("fonts", "Alegreya-Sans-SC", "AlegreyaSansSC-Light.ttf", package = "ttr")
    )
  }
  if (!"linux-libertine" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "linux-libertine",
      regular = system.file("fonts", "Linux-Libertine", "Linux-Libertine-400.ttf", package = "ttr"),
      bold = system.file("fonts", "Linux-Libertine", "Linux-Libertine-Semibold-600.ttf", package = "ttr"),
      italic = system.file("fonts", "Linux-Libertine", "Linux-Libertine-Italic-400.ttf", package = "ttr"),
      bolditalic = system.file("fonts", "Linux-Libertine", "Linux-Libertine-Semibold-Italic-600.ttf", package = "ttr"),
      symbol = system.file("fonts", "Linux-Libertine", "Linux-Libertine-Initials-400.ttf", package = "ttr")
    )
  }
  if (!"tgpagella" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      family = "tgpagella",
      regular = system.file("fonts", "TeX-Gyre-Pagella", "texgyrepagella-regular.otf", package = "ttr"),
      bold = system.file("fonts", "TeX-Gyre-Pagella", "texgyrepagella-bold.otf", package = "ttr"),
      italic = system.file("fonts", "TeX-Gyre-Pagella", "texgyrepagella-italic.otf", package = "ttr"),
      bolditalic = system.file("fonts", "TeX-Gyre-Pagella", "texgyrepagella-bolditalic.otf", package = "ttr"),
    )
  }
  showtext::showtext_auto()
}
