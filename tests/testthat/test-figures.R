figure_condition <- function(expr) {
  tryCatch(
    {
      force(expr)
      NULL
    },
    cttir_error = function(e) e
  )
}

expect_policy_error <- function(figures, field, pattern = NULL) {
  e <- figure_condition(validate_figure_policy(figures))
  expect_s3_class(e, "cttir_schema_error")
  expect_identical(e$code, "invalid_figure_policy")
  expect_identical(e$field, field)
  if (!is.null(pattern)) expect_match(conditionMessage(e), pattern)
  invisible(e)
}

template_path <- function() {
  path <- system.file("templates", "standard-0.3.0", "code", "R", "cttir_figures.R", package = "cttiR")
  expect_true(nzchar(path) && file.exists(path))
  path
}

figure_env <- function() {
  for (pkg in c("ggplot2", "patchwork", "viridisLite", "RColorBrewer", "colorspace", "jsonlite")) {
    testthat::skip_if_not_installed(pkg)
  }
  # Grob building needs a graphics device; use a file-less one for the test.
  withr::local_pdf(NULL, .local_envir = parent.frame())
  env <- new.env(parent = baseenv())
  sys.source(template_path(), envir = env)
  env
}

grob_labels <- function(x) {
  out <- if (!is.null(x$label)) as.character(x$label) else character()
  for (child in c(x$children, x$grobs)) out <- c(out, grob_labels(child))
  out
}

test_that("the pinned RColorBrewer table is bundled without importing RColorBrewer", {
  table <- brewer_palette_table()
  expect_identical(attr(table, "source_package_version"), "1.1-3")
  expect_identical(nrow(table), 35L)
  expect_identical(names(table), c("name", "maxcolors", "category", "colorblind"))
  dark2 <- table[table$name == "Dark2", ]
  expect_identical(dark2$maxcolors, 8L)
  expect_identical(dark2$category, "qual")
  expect_true(dark2$colorblind)
  expect_false(table$colorblind[table$name == "Set1"])
  expect_false("RColorBrewer" %in% names(getNamespaceImports("cttiR")))
  description <- read.dcf(system.file("DESCRIPTION", package = "cttiR"), fields = "Imports")
  expect_false(grepl("RColorBrewer", description[1, "Imports"]))

  skip_if_not_installed("RColorBrewer")
  skip_if_not(identical(utils::packageDescription("RColorBrewer")$Version, "1.1-3"))
  info <- RColorBrewer::brewer.pal.info
  expect_identical(table$name, rownames(info))
  expect_identical(table$maxcolors, as.integer(info$maxcolors))
  expect_identical(table$category, as.character(info$category))
  expect_identical(table$colorblind, info$colorblind)
})

test_that("default figure policies validate and normalize", {
  policy <- validate_figure_policy(NULL)
  expect_identical(policy$continuous_palette, "viridis")
  expect_identical(policy$continuous_option, "D")
  expect_identical(policy$categorical_palette, "Dark2")
  expect_identical(policy$diverging_palette, "BrBG")
  expect_identical(policy$checks, c("protanopia", "deuteranopia", "tritanopia", "grayscale"))
  expect_identical(policy$na_colour, "#808080")
  expect_identical(policy$pinned_provider_version, "RColorBrewer 1.1-3")
  expect_identical(policy$palette_metadata$categorical$maxcolors, 8L)
  expect_identical(policy$palette_metadata$categorical$category, "qual")
  expect_identical(policy$palette_metadata$diverging$maxcolors, 11L)
  expect_identical(policy$palette_metadata$diverging$category, "div")
  expect_true(policy$palette_metadata$diverging$colorblind)

  spec_figures <- default_spec("Figures", "methods", "Goal", "figures", provenance = list(catalog_id = "test"))$figures
  expect_identical(validate_figure_policy(spec_figures), policy)
  parsed <- jsonlite::fromJSON(json_text(spec_figures), simplifyVector = FALSE)
  expect_identical(validate_figure_policy(parsed), policy)
  expect_identical(validate_figure_policy(policy), policy)

  alt <- validate_figure_policy(list(
    continuous_palette = "E", categorical_palette = "Paired", diverging_palette = "PuOr",
    checks = list("grayscale"), named_colours = list(control = "#000000"), na_colour = "#BBBBBBCC"
  ))
  expect_identical(alt$continuous_palette, "cividis")
  expect_identical(alt$continuous_option, "E")
  expect_identical(alt$palette_metadata$categorical$maxcolors, 12L)
  expect_identical(alt$checks, "grayscale")
  expect_identical(alt$named_colours, list(control = "#000000"))
  expect_identical(validate_figure_policy(list(checks = list()))$checks, character())
})

test_that("invalid figure policies raise typed errors with JSON pointers", {
  expect_policy_error(list(categorical_palette = "Dark3"), "/figures/categorical_palette", "not in RColorBrewer 1.1-3")
  expect_policy_error(list(categorical_palette = "dark2"), "/figures/categorical_palette", "did you mean 'Dark2'")
  expect_policy_error(list(categorical_palette = "Blues"), "/figures/categorical_palette", "sequential")
  expect_policy_error(list(categorical_palette = "BrBG"), "/figures/categorical_palette", "diverging")
  expect_policy_error(list(categorical_palette = "Set1"), "/figures/categorical_palette", "colour-blind")
  expect_policy_error(list(categorical_palette = "Accent"), "/figures/categorical_palette", "colour-blind")
  expect_policy_error(list(diverging_palette = "Dark2"), "/figures/diverging_palette", "qualitative")
  expect_policy_error(list(diverging_palette = "Spectral"), "/figures/diverging_palette", "colour-blind")
  expect_policy_error(list(diverging_palette = "RdYlGn"), "/figures/diverging_palette", "colour-blind")
  expect_policy_error(list(continuous_palette = "turbo"), "/figures/continuous_palette", "rainbow")
  expect_policy_error(list(continuous_palette = "rainbow"), "/figures/continuous_palette", "rainbow")
  expect_policy_error(list(continuous_palette = "jet"), "/figures/continuous_palette", "rainbow")
  expect_policy_error(list(continuous_palette = "H"), "/figures/continuous_palette")
  expect_policy_error(list(continuous_palette = "magma"), "/figures/continuous_palette")
  for (bad in list("grey", "#80808", "808080", "#GGGGGG", c("#808080", "#000000"), NA_character_)) {
    expect_policy_error(list(na_colour = bad), "/figures/na_colour")
  }
  expect_policy_error(list(named_colours = list(`a/b` = "red")), "/figures/named_colours/a~1b")
  expect_policy_error(list(named_colours = list("#000000")), "/figures/named_colours")
  expect_policy_error(list(panel_composer = "cowplot"), "/figures/panel_composer")
  expect_policy_error(list(categorical_provider = "viridis"), "/figures/categorical_provider")
  expect_policy_error(list(checks = list("achromatopsia")), "/figures/checks")
  expect_policy_error(list(checks = c("grayscale", "grayscale")), "/figures/checks")
  expect_policy_error(list(colourblind_friendly_only = FALSE), "/figures/colourblind_friendly_only")
  expect_policy_error(list(redundant_encoding_required = NA), "/figures/redundant_encoding_required")
  expect_policy_error(list(unknown_setting = 1), "/figures/unknown_setting")
  expect_policy_error(list(pinned_provider_version = "RColorBrewer 1.1-2"), "/figures/pinned_provider_version")
  expect_policy_error("Dark2", "/figures")
})

test_that("the figure template is static reviewed code with explicit namespaces", {
  path <- template_path()
  text <- readLines(path, warn = FALSE, encoding = "UTF-8")
  expect_false(any(grepl("cttir", text, ignore.case = TRUE)))
  expect_false(any(grepl(":::", text, fixed = TRUE)))
  data <- utils::getParseData(parse(path, keep.source = TRUE))
  namespaces <- unique(data$text[data$token == "SYMBOL_PACKAGE"])
  allowed <- c("ggplot2", "patchwork", "viridisLite", "RColorBrewer", "colorspace", "grDevices", "stats", "utils", "jsonlite")
  expect_true(all(namespaces %in% allowed))
  calls <- unique(data$text[data$token == "SYMBOL_FUNCTION_CALL"])
  forbidden <- c("library", "require", "requireNamespace", "source", "sys.source", "eval", "evalq", "parse", "get", "assign", "system", "system2", "install.packages", "download.file", "Sys.setenv", "setwd")
  expect_identical(intersect(calls, forbidden), character())
  terminal <- data[data$terminal, ]
  defined <- terminal$text[which(terminal$token == "SYMBOL" & c(terminal$token[-1], "") == "LEFT_ASSIGN")]
  expect_true(all(c(
    "cf_palette_categorical", "cf_palette_ordered", "cf_scale_continuous", "cf_scale_diverging",
    "cf_shapes", "cf_linetypes", "cf_check_accessibility", "cf_compose", "cf_figure_spec", "cf_save"
  ) %in% defined))
})

test_that("categorical palettes are verified, stable and never recycled", {
  env <- figure_env()
  policy <- env$cf_policy_default()
  dark2 <- RColorBrewer::brewer.pal(8, "Dark2")

  three <- env$cf_palette_categorical(c("a", "b", "c"), policy)
  expect_identical(unname(as.character(three)), dark2[1:3])
  expect_identical(names(three), c("a", "b", "c"))
  expect_identical(attr(three, "palette"), "Dark2")
  expect_identical(attr(three, "unused_levels"), character())

  eight <- env$cf_palette_categorical(factor(character(), levels = paste0("g", 1:8)), policy)
  expect_identical(unname(as.character(eight)), dark2)
  expect_identical(anyDuplicated(eight), 0L)

  two <- env$cf_palette_categorical(c("treated", "control"), policy)
  expect_identical(unname(as.character(two)), dark2[1:2])

  expect_error(env$cf_palette_categorical(paste0("g", 1:9), policy), "never recycled or interpolated")
  expect_error(env$cf_palette_categorical(c("a", NA), policy), "without NA")
  expect_error(env$cf_palette_categorical(c("a", "a"), policy), "each level once")
  expect_error(env$cf_palette_categorical("z", policy, all_levels = c("a", "b")), "missing from 'all_levels'")
  expect_error(env$cf_palette_categorical("a", modifyList(policy, list(categorical_palette = "Set1"))), "colour-blind")
  expect_error(env$cf_palette_categorical("a", modifyList(policy, list(categorical_palette = "Blues"))), "category 'seq'")
  expect_warning(
    env$cf_palette_categorical("a", modifyList(policy, list(pinned_provider_version = "RColorBrewer 0.0-1"))),
    "differs from the project pin"
  )

  all_levels <- c("a", "b", "c", "d")
  full <- env$cf_palette_categorical(all_levels, policy)
  subset <- env$cf_palette_categorical(c("d", "b"), policy, all_levels = all_levels)
  expect_identical(subset[["d"]], full[["d"]])
  expect_identical(subset[["b"]], full[["b"]])
  expect_identical(attr(subset, "unused_levels"), c("a", "c"))
  expect_identical(attr(subset, "all_levels"), all_levels)

  named <- env$cf_palette_categorical(c("a", "b"), modifyList(policy, list(named_colours = list(b = "#000000"))))
  expect_identical(named[["b"]], "#000000")
  expect_identical(named[["a"]], dark2[[1]])
  expect_error(
    env$cf_palette_categorical(c("a", "b"), modifyList(policy, list(named_colours = list(b = dark2[[1]])))),
    "share one colour"
  )
  expect_error(
    env$cf_palette_categorical(c("a", "b"), modifyList(policy, list(na_colour = dark2[[2]]))),
    "NA colour duplicates"
  )
})

test_that("ordered categories use discretized viridis in explicit order", {
  env <- figure_env()
  policy <- env$cf_policy_default()
  levels <- c("low", "medium", "high", "very high")
  ordered <- env$cf_palette_ordered(levels, policy)
  expect_identical(names(ordered), levels)
  expect_identical(unname(as.character(ordered)), substr(viridisLite::viridis(4, option = "D"), 1, 7))
  subset <- env$cf_palette_ordered(c("high", "low"), policy, all_levels = levels)
  expect_identical(subset[["high"]], ordered[["high"]])
  expect_identical(attr(subset, "unused_levels"), c("medium", "very high"))
  cividis <- env$cf_palette_ordered(levels, modifyList(policy, list(continuous_palette = "cividis")))
  expect_identical(unname(as.character(cividis)), substr(viridisLite::viridis(4, option = "E"), 1, 7))
  expect_error(env$cf_palette_ordered(levels, modifyList(policy, list(continuous_palette = "turbo"))), "viridis")
})

test_that("continuous and diverging scales keep explicit limits, midpoint and NA colour", {
  env <- figure_env()
  policy <- env$cf_policy_default()
  data <- data.frame(x = 1:5, y = 1, value = c(0, 0.5, 1, 2, NA))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = value)) + ggplot2::geom_point() +
    env$cf_scale_continuous(policy, limits = c(0, 1), name = "Value (units)")
  colours <- ggplot2::layer_data(plot)$colour
  expect_identical(toupper(colours[[1]]), substr(viridisLite::viridis(1, option = "D"), 1, 7))
  expect_identical(toupper(colours[[3]]), "#FDE725")
  expect_identical(colours[[4]], colours[[3]])
  expect_identical(colours[[5]], "#808080")

  cividis <- ggplot2::ggplot(data, ggplot2::aes(x, y, fill = value)) + ggplot2::geom_tile() +
    env$cf_scale_continuous(modifyList(policy, list(continuous_palette = "cividis")), aesthetic = "fill", limits = c(0, 1), oob = "censor")
  fills <- ggplot2::layer_data(cividis)$fill
  expect_identical(toupper(fills[[1]]), substr(viridisLite::viridis(1, option = "E"), 1, 7))
  expect_identical(fills[[4]], "#808080")
  expect_error(env$cf_scale_continuous(policy, limits = c(1, 0)), "increasing")
  expect_error(env$cf_scale_continuous(policy, aesthetic = "size"), "colour' or 'fill")

  brbg <- RColorBrewer::brewer.pal(11, "BrBG")
  diverging <- env$cf_scale_diverging(policy, midpoint = 0, limits = c(-1, 4))
  expect_identical(diverging$limits, c(-4, 4))
  signed <- data.frame(x = 1:6, y = 1, value = c(-4, 0, 4, 9, NA, -1))
  plot <- ggplot2::ggplot(signed, ggplot2::aes(x, y, fill = value)) + ggplot2::geom_tile() + diverging
  fills <- toupper(ggplot2::layer_data(plot)$fill)
  expect_identical(fills[1:3], brbg[c(1, 6, 11)])
  expect_identical(fills[[4]], brbg[[11]])
  expect_identical(fills[[5]], "#808080")

  asymmetric <- env$cf_scale_diverging(policy, midpoint = 1, limits = c(0, 5), symmetric = FALSE)
  expect_identical(asymmetric$limits, c(0, 5))
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1, value = c(0, 1, 5)), ggplot2::aes(x, y, fill = value)) +
    ggplot2::geom_tile() + asymmetric
  expect_identical(toupper(ggplot2::layer_data(plot)$fill), brbg[c(1, 6, 11)])

  expect_error(env$cf_scale_diverging(policy, midpoint = 6, limits = c(0, 5)), "strictly inside")
  expect_error(env$cf_scale_diverging(policy, limits = c(0, 5)), "midpoint")
  expect_error(env$cf_scale_diverging(policy, midpoint = 0), "explicit 'limits'")
  expect_error(env$cf_scale_diverging(modifyList(policy, list(diverging_palette = "Dark2")), 0, c(-1, 1)), "category 'qual'")
  expect_error(env$cf_scale_diverging(modifyList(policy, list(diverging_palette = "Spectral")), 0, c(-1, 1)), "colour-blind")
})

test_that("categorical NA values use the distinct NA colour", {
  env <- figure_env()
  policy <- env$cf_policy_default()
  data <- data.frame(x = 1:4, y = 1:4, group = factor(c("a", "b", NA, "unknown"), levels = c("a", "b", "unknown")))
  colours <- env$cf_palette_categorical(levels(data$group), policy)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, colour = group)) + ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = colours, na.value = policy$na_colour)
  drawn <- ggplot2::layer_data(plot)$colour
  expect_identical(drawn[[3]], "#808080")
  expect_identical(drawn[[4]], colours[["unknown"]])
  expect_false(colours[["unknown"]] == "#808080")
})

test_that("redundant shapes and line types are distinct and stable", {
  env <- figure_env()
  shapes <- env$cf_shapes(paste0("g", 1:8))
  expect_identical(length(unique(shapes)), 8L)
  expect_identical(names(shapes), paste0("g", 1:8))
  subset <- env$cf_shapes(c("g7", "g2"), all_levels = paste0("g", 1:8))
  expect_identical(subset[["g7"]], shapes[["g7"]])
  expect_identical(attr(subset, "unused_levels"), paste0("g", c(1, 3:6, 8)))
  expect_error(env$cf_shapes(paste0("g", 1:9)), "facets or direct labels")
  lines <- env$cf_linetypes(c("a", "b", "c"))
  expect_identical(as.character(lines), c("solid", "dashed", "dotted"))
  expect_error(env$cf_linetypes(letters[1:7]), "line types")
})

test_that("accessibility checks report every simulation and record findings", {
  env <- figure_env()
  policy <- env$cf_policy_default()
  colours <- env$cf_palette_categorical(c("a", "b", "c"), policy)
  checks <- env$cf_check_accessibility(colours)
  expect_identical(checks$check, c("normal", "protanopia", "deuteranopia", "tritanopia", "grayscale"))
  expect_true(all(checks$n_colours == 3L))
  expect_true(all(c("min_delta_e", "closest_pair", "min_contrast", "status", "note") %in% names(checks)))
  expect_identical(checks$status[1:4], rep("pass", 4))
  expect_true(all(checks$min_delta_e[1:4] >= 10))
  expect_identical(checks$status[[5]], "warning")
  expect_match(checks$note[[5]], "non-colour encoding")
  expect_match(attr(checks, "method")$statement, "not a guarantee")

  with_na <- env$cf_check_accessibility(colours, na_colour = policy$na_colour)
  expect_true(all(with_na$n_colours == 4L))
  expect_true(any(grepl("(missing)", with_na$closest_pair, fixed = TRUE)))

  subset <- env$cf_check_accessibility(colours, checks = c("deuteranopia", "grayscale"))
  expect_identical(subset$check, c("normal", "deuteranopia", "grayscale"))
  expect_error(env$cf_check_accessibility(colours, checks = "achromatopsia"), "Accessibility checks")

  eight <- env$cf_check_accessibility(env$cf_palette_categorical(paste0("g", 1:8), policy))
  expect_true(any(eight$status[-1] == "warning"))

  same <- env$cf_check_accessibility(c(a = "#1B9E77", b = "#1B9E77"))
  expect_identical(same$min_delta_e[[1]], 0)
  expect_identical(same$status[[1]], "warning")
  faded <- env$cf_check_accessibility(c(a = "#1B9E7740"), min_contrast = 3)
  expect_lt(faded$min_contrast[[1]], checks$min_contrast[[1]])
  expect_identical(faded$status[[1]], "warning")
  unjudged <- env$cf_check_accessibility(c(a = "#FDE725", b = "#440154"), min_contrast = NA)
  expect_identical(unjudged$status[[1]], "pass")

  sharma1 <- rbind(c(50, 2.6772, -79.7751), c(50, 0, 0), c(50, 2.5, 0), c(60.2574, -34.0099, 36.2677), c(22.7233, 20.0904, -46.694))
  sharma2 <- rbind(c(50, 0, -82.7485), c(50, -1, 2), c(73, 25, -18), c(60.4626, -34.1751, 39.4387), c(23.0331, 14.973, -42.5619))
  expect_equal(env$cf_delta_e2000(sharma1, sharma2), c(2.0425, 2.3669, 27.1492, 1.2644, 2.0373), tolerance = 1e-4)
  hex <- RColorBrewer::brewer.pal(8, "Dark2")
  reference <- colorspace::coords(methods::as(colorspace::hex2RGB(hex), "LAB"))
  expect_lt(max(abs(env$cf_lab(hex) - reference)), 0.5)
  expect_equal(env$cf_contrast("#000000", "#FFFFFF"), 21)
  expect_identical(colorspace::protan("#FFFFFF"), "#FFFFFF")
})

test_that("patchwork composition keeps panel order, tags and guide policy", {
  env <- figure_env()
  panel <- function(title) {
    ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point() + ggplot2::ggtitle(title)
  }
  composed <- env$cf_compose(list(first = panel("first"), second = panel("second"), third = panel("third")), ncol = 2)
  expect_s3_class(composed, "patchwork")
  layout <- attr(composed, "cf_layout")
  expect_identical(layout$panel_ids, c("first", "second", "third"))
  expect_identical(layout$tags, c("A", "B", "C"))
  expect_identical(layout$guides, "keep")
  grob <- patchwork::patchworkGrob(composed)
  names <- grob$layout$name
  expect_identical(sum(grepl("^panel-[0-9]+$", names)), 3L)
  tags <- vapply(paste0("tag-", 1:3), function(n) grob_labels(grob$grobs[[which(names == n)]])[[1]], "")
  titles <- vapply(paste0("title-", 1:3), function(n) grob_labels(grob$grobs[[which(names == n)]])[[1]], "")
  expect_identical(unname(tags), c("A", "B", "C"))
  expect_identical(unname(titles), c("first", "second", "third"))
  expect_identical(grob$layout$l[names == "tag-1"], grob$layout$l[names == "tag-3"])
  expect_lt(grob$layout$t[names == "tag-2"], grob$layout$t[names == "tag-3"])

  untagged <- env$cf_compose(list(panel("a"), panel("b")), tags = FALSE, collect_guides = TRUE, widths = c(2, 1))
  expect_identical(attr(untagged, "cf_layout")$panel_ids, c("panel_1", "panel_2"))
  expect_identical(attr(untagged, "cf_layout")$tags, character())
  expect_identical(untagged$patches$layout$guides, "collect")
  expect_false(any(grepl("^tag-", patchwork::patchworkGrob(untagged)$layout$name)))

  expect_error(env$cf_compose(panel("single")), "nonempty list")
  expect_error(env$cf_compose(list(panel("a"), "not a plot")), "ggplot")
  expect_error(env$cf_compose(list(a = panel("a"), a = panel("b"))), "unique")
  expect_error(env$cf_compose(list(panel("a")), widths = c(-1)), "positive")
})

test_that("figures are saved at final size with a JSON sidecar", {
  env <- figure_env()
  skip_if_not_installed("png")
  policy <- env$cf_policy_default()
  set.seed(20)
  groups <- c("control", "low dose", "high dose")
  points <- data.frame(x = stats::rnorm(30), y = stats::rnorm(30), group = factor(rep(groups, 10), levels = groups))
  colours <- env$cf_palette_categorical(groups, policy)
  shapes <- env$cf_shapes(groups)
  scatter <- ggplot2::ggplot(points, ggplot2::aes(x, y, colour = group, shape = group)) +
    ggplot2::geom_point(size = 2) + ggplot2::scale_colour_manual(values = colours, na.value = policy$na_colour) +
    ggplot2::scale_shape_manual(values = shapes)
  grid <- expand.grid(x = 1:6, y = 1:4)
  grid$value <- grid$x * grid$y / 24
  heatmap <- ggplot2::ggplot(grid, ggplot2::aes(x, y, fill = value)) + ggplot2::geom_tile() +
    env$cf_scale_continuous(policy, aesthetic = "fill", limits = c(0, 1), name = "Score")
  figure <- env$cf_compose(list(scatter = scatter, heatmap = heatmap), ncol = 2)
  mapping <- list(
    colour = list(variable = "group", type = "categorical", levels = groups),
    fill = list(variable = "value", type = "continuous", limits = c(0, 1), units = "score")
  )
  expect_error(
    env$cf_figure_spec("fig01", mapping, policy, colours, width = 4, height = 2),
    "non-colour encoding"
  )
  spec <- env$cf_figure_spec("fig01", mapping, policy, colours, encodings = list(shape = shapes),
    width = 4, height = 2, units = "in", dpi = 100, provenance = list(data = "synthetic"))
  expect_identical(spec$accessibility$status, "warning")
  expect_identical(spec$accessibility$checks$check, c("normal", "protanopia", "deuteranopia", "tritanopia", "grayscale"))

  dir <- withr::local_tempdir()
  file <- file.path(dir, "fig01.png")
  expect_error(env$cf_save(figure, file, spec, width = 5), "differs from the figure spec")
  expect_false(file.exists(file))
  out <- env$cf_save(figure, file, spec)
  expect_true(file.exists(file))
  expect_identical(unname(out[["sidecar"]]), paste0(file, ".json"))
  image <- png::readPNG(file)
  expect_identical(dim(image)[1:2], c(200L, 400L))

  sidecar <- jsonlite::read_json(paste0(file, ".json"))
  expect_identical(sidecar$id, "fig01")
  expect_identical(sidecar$figure_spec_version, 1L)
  expect_identical(sidecar$output$width, 4L)
  expect_identical(sidecar$output$dpi, 100L)
  expect_identical(sidecar$output$background, "#FFFFFF")
  expect_identical(sidecar$output$file, "fig01.png")
  expect_identical(sidecar$colours$control, colours[["control"]])
  expect_identical(sidecar$encodings$shape$`high dose`, shapes[["high dose"]])
  expect_identical(sidecar$unused_levels, list())
  expect_identical(sidecar$mapping$colour$levels, as.list(groups))
  expect_identical(sidecar$palettes$categorical$name, "Dark2")
  expect_identical(sidecar$palettes$categorical$pinned, "RColorBrewer 1.1-3")
  expect_identical(sidecar$palettes$continuous$option, "D")
  expect_identical(sidecar$na$colour, "#808080")
  expect_identical(unlist(sidecar$panels$panel_ids), c("scatter", "heatmap"))
  expect_identical(unlist(sidecar$panels$tags), c("A", "B"))
  expect_identical(length(sidecar$accessibility$checks), 5L)
  expect_identical(sidecar$accessibility$checks[[1]]$check, "normal")
  expect_match(sidecar$accessibility$statement, "not a guarantee")
  expect_identical(sidecar$provenance$supplied$data, "synthetic")
  expect_true(all(c("ggplot2", "patchwork", "RColorBrewer", "viridisLite", "colorspace") %in% names(sidecar$provenance$packages)))

  continuous_only <- env$cf_figure_spec("fig02", mapping["fill"], policy, width = 3, height = 3)
  expect_identical(continuous_only$accessibility$status, "not_checked")
  expect_error(env$cf_figure_spec("../bad", mapping, policy, colours, encodings = list(shape = shapes), width = 1, height = 1), "identifier")
  expect_error(
    env$cf_figure_spec("fig03", list(fill = list(variable = "delta", type = "diverging")), policy, width = 1, height = 1),
    "midpoint and limits"
  )
})
