# Figure policy (file 29, gate G25).
#
# The package never imports RColorBrewer. Palette facts come from a bundled,
# pinned copy of `RColorBrewer::brewer.pal.info` (release 1.1-3), so semantic
# validation of a ProjectSpec `figures` object is deterministic and offline.
# Generated projects re-verify the same facts at runtime against their own
# installed RColorBrewer in the reviewed template `code/R/cttir_figures.R`.

brewer_pin <- list(package = "RColorBrewer", version = "1.1-3", file = "brewer-pal-info-1.1-3.json")

# Pinned RColorBrewer palette facts.
#
# Returns a data frame with one row per palette (`name`, `maxcolors`,
# `category`, `colorblind`) in the published `brewer.pal.info` order, with
# attributes `source_package` and `source_package_version`. Structural damage
# to the bundled file raises `cttir_source_unavailable` (`corrupt_resource`).
brewer_palette_table <- function() {
  doc <- read_document(resource_file("extdata", brewer_pin$file))
  corrupt <- function() {
    abort_cttir("The pinned RColorBrewer palette table is damaged.", "cttir_source_unavailable",
      "corrupt_resource",
      remediation = "Reinstall cttiR from an intact source archive."
    )
  }
  if (!identical(doc$source_package, brewer_pin$package) ||
      !identical(doc$source_package_version, brewer_pin$version) ||
      !is.list(doc$palettes) || !length(doc$palettes)) {
    corrupt()
  }
  rows <- lapply(doc$palettes, function(x) {
    ok <- is.list(x) && is.character(x$name) && length(x$name) == 1L && nzchar(x$name) &&
      is.numeric(x$maxcolors) && length(x$maxcolors) == 1L && x$maxcolors >= 3 &&
      x$maxcolors == round(x$maxcolors) && is.character(x$category) && length(x$category) == 1L &&
      x$category %in% c("div", "qual", "seq") && is.logical(x$colorblind) &&
      length(x$colorblind) == 1L && !is.na(x$colorblind)
    if (!ok) corrupt()
    data.frame(
      name = x$name, maxcolors = as.integer(x$maxcolors), category = x$category,
      colorblind = x$colorblind, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (anyDuplicated(out$name)) corrupt()
  rownames(out) <- NULL
  attr(out, "source_package") <- brewer_pin$package
  attr(out, "source_package_version") <- brewer_pin$version
  out
}

figure_policy_defaults <- function() {
  list(
    continuous_palette = "viridis", categorical_provider = "RColorBrewer",
    categorical_palette = "Dark2", diverging_palette = "BrBG", colourblind_friendly_only = TRUE,
    redundant_encoding_required = TRUE, panel_composer = "patchwork",
    checks = c("protanopia", "deuteranopia", "tritanopia", "grayscale"), na_colour = "#808080"
  )
}

# Supported continuous maps. Aliases are viridisLite option letters.
figure_continuous_options <- c(viridis = "D", cividis = "E")

figure_policy_error <- function(message, key = NULL, remediation = "Review the figure policy in the project specification.") {
  pointer <- if (is.null(key)) "/figures" else paste0("/figures/", key)
  abort_cttir(message, "cttir_schema_error", "invalid_figure_policy", pointer, remediation)
}

json_pointer_token <- function(x) gsub("/", "~1", gsub("~", "~0", x, fixed = TRUE), fixed = TRUE)

is_hex_colour <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x)
}

figure_text <- function(x, key) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    figure_policy_error(paste0("Figure setting '", key, "' must be one nonempty string."), key)
  }
  x
}

figure_flag <- function(x, key) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    figure_policy_error(paste0("Figure setting '", key, "' must be true or false."), key)
  }
  if (!isTRUE(x)) {
    figure_policy_error(
      paste0("Figure setting '", key, "' is mandatory in this figure policy version."), key,
      "Keep colour-blind-friendly palettes and redundant non-colour encodings enabled."
    )
  }
  x
}

# Check one Brewer palette against the pinned table and return its facts.
figure_brewer_entry <- function(name, key, category, colourblind_only, table) {
  row <- table[table$name == name, , drop = FALSE]
  if (!nrow(row)) {
    near <- table$name[tolower(table$name) == tolower(name)]
    hint <- if (length(near)) paste0(" Palette names are case-sensitive; did you mean '", near[[1L]], "'?") else ""
    figure_policy_error(
      paste0("Palette '", name, "' is not in RColorBrewer ", brewer_pin$version, ".", hint), key,
      paste("Choose an eligible", category, "palette, for example",
        paste(table$name[table$category == category & table$colorblind], collapse = ", "))
    )
  }
  kind <- c(qual = "qualitative", div = "diverging", seq = "sequential")
  if (!identical(row$category, category)) {
    figure_policy_error(
      paste0("Palette '", name, "' is ", kind[[row$category]], " but this setting requires a ",
        kind[[category]], " palette."), key,
      paste("Choose one of:", paste(table$name[table$category == category & table$colorblind], collapse = ", "))
    )
  }
  if (colourblind_only && !isTRUE(row$colorblind)) {
    figure_policy_error(
      paste0("Palette '", name, "' is not flagged colour-blind friendly in RColorBrewer ", brewer_pin$version, "."), key,
      paste("Choose one of:", paste(table$name[table$category == category & table$colorblind], collapse = ", "))
    )
  }
  list(
    provider = brewer_pin$package, name = row$name, category = row$category,
    maxcolors = row$maxcolors, colorblind = row$colorblind
  )
}

#' Validate and normalize a figure policy
#'
#' Semantic validation of the ProjectSpec `figures` object (file 29). The JSON
#' schema checks shapes; this function checks palette facts against the pinned
#' RColorBrewer 1.1-3 table, the supported viridis maps, hex colours and the
#' accessibility checks.
#'
#' @param figures The `figures` list of a ProjectSpec, or `NULL` for the Fast
#'   setup defaults. Missing fields take their defaults.
#' @return The normalized policy list, including `continuous_option`,
#'   `palette_metadata` and `pinned_provider_version`. These derived fields are
#'   not ProjectSpec schema fields; do not write them back into a spec.
#' @noRd
validate_figure_policy <- function(figures = NULL) {
  if (is.null(figures)) figures <- list()
  if (!is.list(figures) || is.object(figures) || (length(figures) && is.null(names(figures)))) {
    figure_policy_error("The figure policy must be an object of named settings.")
  }
  if (length(figures) && (any(!nzchar(names(figures))) || anyDuplicated(names(figures)))) {
    figure_policy_error("Figure policy keys must be unique and nonempty.")
  }
  defaults <- figure_policy_defaults()
  derived <- c("continuous_option", "palette_metadata", "pinned_provider_version")
  unknown <- setdiff(names(figures), c(names(defaults), "named_colours", derived))
  if (length(unknown)) {
    figure_policy_error(paste0("Unknown figure setting '", unknown[[1L]], "'."), json_pointer_token(unknown[[1L]]))
  }
  pinned <- paste(brewer_pin$package, brewer_pin$version)
  if (!is.null(figures$pinned_provider_version) &&
      !identical(figures$pinned_provider_version, pinned)) {
    figure_policy_error(
      paste0("The project pins '", format(figures$pinned_provider_version), "' but this build validates ", pinned, "."),
      "pinned_provider_version", "Keep the project pin; palettes are never changed during a database refresh."
    )
  }
  policy <- defaults
  for (key in intersect(names(figures), names(defaults))) {
    if (!is.null(figures[[key]])) policy[[key]] <- figures[[key]]
  }

  continuous <- figure_text(policy$continuous_palette, "continuous_palette")
  if (continuous %in% figure_continuous_options) {
    continuous <- names(figure_continuous_options)[figure_continuous_options == continuous]
  }
  if (!continuous %in% names(figure_continuous_options)) {
    rejected <- tolower(continuous) %in% c("rainbow", "turbo", "jet", "h", "hsv", "spectral")
    figure_policy_error(
      paste0("Continuous palette '", continuous, "' is not supported",
        if (rejected) "; rainbow-like maps are not perceptually ordered." else "."),
      "continuous_palette", "Use 'viridis' (option D) or 'cividis' (option E)."
    )
  }
  if (!identical(figure_text(policy$categorical_provider, "categorical_provider"), brewer_pin$package)) {
    figure_policy_error("The categorical palette provider must be RColorBrewer.", "categorical_provider")
  }
  colourblind_only <- figure_flag(policy$colourblind_friendly_only, "colourblind_friendly_only")
  figure_flag(policy$redundant_encoding_required, "redundant_encoding_required")
  if (!identical(figure_text(policy$panel_composer, "panel_composer"), "patchwork")) {
    figure_policy_error("Multi-panel figures must be composed with patchwork.", "panel_composer")
  }
  table <- brewer_palette_table()
  categorical <- figure_brewer_entry(
    figure_text(policy$categorical_palette, "categorical_palette"),
    "categorical_palette", "qual", colourblind_only, table
  )
  diverging <- figure_brewer_entry(
    figure_text(policy$diverging_palette, "diverging_palette"),
    "diverging_palette", "div", colourblind_only, table
  )

  checks <- policy$checks
  if (is.list(checks)) {
    if (!all(vapply(checks, function(x) is.character(x) && length(x) == 1L, logical(1)))) {
      figure_policy_error("Accessibility checks must be strings.", "checks")
    }
    checks <- as.character(unlist(checks, use.names = FALSE))
  }
  allowed <- defaults$checks
  if (!is.character(checks) || anyNA(checks) || !all(checks %in% allowed) || anyDuplicated(checks)) {
    figure_policy_error(
      "Accessibility checks must be unique values from protanopia, deuteranopia, tritanopia and grayscale.", "checks"
    )
  }

  if (!is_hex_colour(policy$na_colour)) {
    figure_policy_error("The NA colour must be a hex colour such as '#808080'.", "na_colour")
  }
  named <- figures$named_colours
  if (!is.null(named)) {
    if (!is.list(named) && !is.character(named)) {
      figure_policy_error("Named colours must map level names to hex colours.", "named_colours")
    }
    if (length(named) && (is.null(names(named)) || any(!nzchar(names(named))) || anyDuplicated(names(named)))) {
      figure_policy_error("Named colour levels must be unique and nonempty.", "named_colours")
    }
    for (level in names(named)) {
      if (!is_hex_colour(named[[level]])) {
        figure_policy_error(
          paste0("Named colour for level '", level, "' must be a hex colour."),
          paste0("named_colours/", json_pointer_token(level))
        )
      }
    }
    named <- as.list(named)
  }

  out <- list(
    continuous_palette = continuous,
    continuous_option = figure_continuous_options[[continuous]],
    categorical_provider = brewer_pin$package,
    categorical_palette = categorical$name,
    diverging_palette = diverging$name,
    colourblind_friendly_only = colourblind_only,
    redundant_encoding_required = TRUE,
    panel_composer = "patchwork",
    checks = checks,
    na_colour = policy$na_colour
  )
  if (!is.null(named)) out$named_colours <- named
  out$palette_metadata <- list(
    continuous = list(provider = "viridisLite", name = continuous, option = figure_continuous_options[[continuous]]),
    categorical = categorical,
    diverging = diverging
  )
  out$pinned_provider_version <- pinned
  out
}
