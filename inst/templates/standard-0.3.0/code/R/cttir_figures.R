# Accessible figure helpers for this research project.
#
# Reviewed static source, copied verbatim into the project. It attaches no
# packages and calls ggplot2, patchwork, viridisLite, RColorBrewer, colorspace,
# grDevices, stats, utils and jsonlite only through explicit namespaces.
#
# Figure policy:
# - continuous magnitudes: viridis (option "D") or cividis (option "E") with
#   explicit limits, units and a distinct NA colour; no rainbow/turbo maps;
# - unordered categories: an RColorBrewer qualitative palette that the
#   installed brewer.pal.info marks colour-blind friendly, used within its
#   maxcolors; colours are never recycled or interpolated, and a non-colour
#   encoding (shape, line type, direct label or facet) is also required;
# - ordered categories: discretized viridis in explicit level order;
# - signed deviations: an RColorBrewer diverging palette whose neutral class
#   sits exactly on a meaningful midpoint;
# - missing values: the policy NA colour; "unknown" is a separate explicit level;
# - multi-panel figures: patchwork.
#
# Colour-vision simulations (colorspace) are checks, not a guarantee for every
# observer. Record unresolved findings; do not claim universal accessibility
# from a palette name.

cf_policy_default <- function() {
  list(
    continuous_palette = "viridis", categorical_provider = "RColorBrewer",
    categorical_palette = "Dark2", diverging_palette = "BrBG",
    colourblind_friendly_only = TRUE, redundant_encoding_required = TRUE,
    panel_composer = "patchwork",
    checks = c("protanopia", "deuteranopia", "tritanopia", "grayscale"),
    na_colour = "#808080", pinned_provider_version = "RColorBrewer 1.1-3"
  )
}

# Read a figure policy from JSON and fill missing settings with defaults.
cf_read_policy <- function(path) {
  cf_check_policy(jsonlite::read_json(path, simplifyVector = TRUE))
}

cf_check_policy <- function(policy) {
  if (!is.list(policy)) stop("The figure policy must be a list.", call. = FALSE)
  defaults <- cf_policy_default()
  for (key in names(defaults)) {
    if (is.null(policy[[key]])) policy[[key]] <- defaults[[key]]
  }
  if (!cf_is_string(policy$continuous_palette) ||
      !policy$continuous_palette %in% c("viridis", "cividis")) {
    stop("The continuous palette must be 'viridis' or 'cividis'.", call. = FALSE)
  }
  if (!identical(policy$categorical_provider, "RColorBrewer")) {
    stop("The categorical palette provider must be RColorBrewer.", call. = FALSE)
  }
  if (!identical(policy$panel_composer, "patchwork")) {
    stop("Multi-panel figures must be composed with patchwork.", call. = FALSE)
  }
  cf_flag(policy$colourblind_friendly_only, "colourblind_friendly_only")
  cf_flag(policy$redundant_encoding_required, "redundant_encoding_required")
  checks <- unlist(policy$checks, use.names = FALSE)
  if (!is.character(checks) && length(checks)) {
    stop("Accessibility checks must be strings.", call. = FALSE)
  }
  policy$checks <- cf_check_names(as.character(checks))
  if (!cf_is_hex(policy$na_colour)) {
    stop("The NA colour must be a hex colour such as '#808080'.", call. = FALSE)
  }
  policy
}

cf_is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

cf_is_hex <- function(x) cf_is_string(x) && grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x)

cf_check_names <- function(checks) {
  allowed <- c("protanopia", "deuteranopia", "tritanopia", "grayscale")
  if (anyNA(checks) || !all(checks %in% allowed) || anyDuplicated(checks)) {
    stop("Accessibility checks must be unique values from: ",
      paste(allowed, collapse = ", "), ".", call. = FALSE)
  }
  checks
}

cf_flag <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("'", arg, "' must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

cf_positive <- function(value, arg, scalar = TRUE) {
  if (!is.numeric(value) || !length(value) || (scalar && length(value) != 1L) ||
      any(!is.finite(value)) || any(value <= 0)) {
    stop("'", arg, "' must be ", if (scalar) "one positive number." else "positive numbers.", call. = FALSE)
  }
  value
}

cf_package_version <- function(pkg) utils::packageDescription(pkg, fields = "Version")

cf_option <- function(policy) {
  options <- c(viridis = "D", cividis = "E")
  if (!cf_is_string(policy$continuous_palette) || !policy$continuous_palette %in% names(options)) {
    stop("The continuous palette must be 'viridis' or 'cividis'.", call. = FALSE)
  }
  options[[policy$continuous_palette]]
}

cf_aesthetic <- function(aesthetic) {
  if (identical(aesthetic, "color")) aesthetic <- "colour"
  if (!cf_is_string(aesthetic) || !aesthetic %in% c("colour", "fill")) {
    stop("The aesthetic must be 'colour' or 'fill'.", call. = FALSE)
  }
  aesthetic
}

# Levels are an explicit, ordered set of unique names. A factor contributes
# its levels(). Missing values are not a level: they use the NA colour.
cf_levels <- function(x, arg) {
  if (is.factor(x)) x <- levels(x)
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x))) {
    stop("'", arg, "' must be nonempty level names without NA; missing values ",
      "are drawn with the policy NA colour.", call. = FALSE)
  }
  if (anyDuplicated(x)) {
    stop("'", arg, "' must list each level once, in display order (for example levels(factor)).",
      call. = FALSE)
  }
  x
}

# Colours are assigned by position in all_levels, so a subset keeps the colours
# of the full figure. Levels absent from the subset are recorded as unused.
cf_resolve_levels <- function(levels, all_levels) {
  levels <- cf_levels(levels, "levels")
  all_levels <- if (is.null(all_levels)) levels else cf_levels(all_levels, "all_levels")
  extra <- setdiff(levels, all_levels)
  if (length(extra)) {
    stop("Levels missing from 'all_levels': ", paste(extra, collapse = ", "), ".", call. = FALSE)
  }
  list(levels = levels, all = all_levels, unused = setdiff(all_levels, levels))
}

# Verify a Brewer palette against the installed RColorBrewer::brewer.pal.info.
cf_brewer_info <- function(name, category, policy) {
  info <- RColorBrewer::brewer.pal.info
  if (!cf_is_string(name) || !name %in% rownames(info)) {
    stop("Palette '", format(name), "' is not provided by the installed RColorBrewer.", call. = FALSE)
  }
  row <- info[name, , drop = FALSE]
  found <- as.character(row$category)
  if (!identical(found, category)) {
    stop("Palette '", name, "' has category '", found, "' but '", category, "' is required.",
      call. = FALSE)
  }
  if (!isFALSE(policy$colourblind_friendly_only) && !isTRUE(row$colorblind)) {
    stop("Palette '", name, "' is not flagged colour-blind friendly in RColorBrewer::brewer.pal.info.",
      call. = FALSE)
  }
  installed <- paste("RColorBrewer", cf_package_version("RColorBrewer"))
  pinned <- policy$pinned_provider_version
  if (cf_is_string(pinned) && !identical(pinned, installed)) {
    warning("Installed ", installed, " differs from the project pin ", pinned,
      "; palette facts were re-verified at runtime, review the colours.", call. = FALSE)
  }
  list(name = name, category = found, maxcolors = as.integer(row$maxcolors),
    colorblind = isTRUE(row$colorblind), version = installed)
}

cf_map_attributes <- function(colours, lv, extra) {
  attr(colours, "all_levels") <- lv$all
  attr(colours, "unused_levels") <- lv$unused
  for (key in names(extra)) attr(colours, key) <- extra[[key]]
  colours
}

# Named level -> colour map for unordered categories. More levels than the
# palette's maxcolors is an error: use labelled facets or direct labels.
cf_palette_categorical <- function(levels, policy, all_levels = NULL) {
  lv <- cf_resolve_levels(levels, all_levels)
  info <- cf_brewer_info(policy$categorical_palette, "qual", policy)
  n <- length(lv$all)
  if (n > info$maxcolors) {
    stop(n, " levels exceed the ", info$maxcolors, " colours of the qualitative palette '",
      info$name, "'. Qualitative colours are never recycled or interpolated: use labelled ",
      "facets, direct labels, or an explicitly reviewed grouping of levels.", call. = FALSE)
  }
  # brewer.pal() returns at least three colours; qualitative palettes are
  # prefix-stable, so the first n colours are the n-colour palette.
  colours <- RColorBrewer::brewer.pal(max(3L, n), info$name)[seq_len(n)]
  names(colours) <- lv$all
  named <- policy$named_colours
  if (length(named)) {
    hit <- intersect(names(named), lv$all)
    for (level in hit) {
      if (!cf_is_hex(named[[level]])) {
        stop("Named colour for level '", level, "' must be a hex colour.", call. = FALSE)
      }
      colours[[level]] <- named[[level]]
    }
  }
  key <- toupper(substr(colours, 1L, 7L))
  if (anyDuplicated(key)) {
    stop("Two levels share one colour; review the named colours.", call. = FALSE)
  }
  if (toupper(substr(policy$na_colour, 1L, 7L)) %in% key) {
    stop("The NA colour duplicates a level colour; missing values must stay distinct.", call. = FALSE)
  }
  cf_map_attributes(colours[lv$levels], lv, list(
    mapping_type = "categorical", provider = "RColorBrewer", palette = info$name,
    provider_version = info$version
  ))
}

# Named level -> colour map for ordered categories: viridis discretized over
# all_levels in their given order.
cf_palette_ordered <- function(levels, policy, all_levels = NULL) {
  lv <- cf_resolve_levels(levels, all_levels)
  option <- cf_option(policy)
  colours <- substr(viridisLite::viridis(length(lv$all), option = option), 1L, 7L)
  names(colours) <- lv$all
  cf_map_attributes(colours[lv$levels], lv, list(
    mapping_type = "ordered", provider = "viridisLite", palette = policy$continuous_palette,
    option = option, provider_version = paste("viridisLite", cf_package_version("viridisLite"))
  ))
}

# Out-of-range values are clamped to the limits ("squish") by default so that
# they are not drawn with the NA colour, which is reserved for missing values.
cf_squish <- function(x, range = c(0, 1), only.finite = TRUE) {
  keep <- if (only.finite) is.finite(x) else !is.na(x)
  x[keep & x < range[1]] <- range[1]
  x[keep & x > range[2]] <- range[2]
  x
}

cf_limits <- function(limits) {
  if (!is.numeric(limits) || length(limits) != 2L || any(!is.finite(limits)) || limits[1] >= limits[2]) {
    stop("'limits' must be two finite increasing numbers.", call. = FALSE)
  }
  as.numeric(limits)
}

# Continuous viridis/cividis scale. A colourbar does not show the NA colour,
# and in grayscale a mid-grey NA matches mid-range values: state the NA colour
# in the caption or mark missing cells with a non-colour encoding.
cf_scale_continuous <- function(policy, aesthetic = "colour", limits = NULL,
  na_value = policy$na_colour, name = ggplot2::waiver(),
  oob = c("squish", "censor"), ...) {
  aesthetic <- cf_aesthetic(aesthetic)
  oob <- match.arg(oob)
  if (!is.null(limits)) limits <- cf_limits(limits)
  args <- list(name = name, option = cf_option(policy), limits = limits, na.value = na_value,
    guide = "colourbar", aesthetics = aesthetic, ...)
  if (identical(oob, "squish")) args$oob <- cf_squish
  do.call(ggplot2::scale_colour_viridis_c, args)
}

# Diverging scale: the palette's neutral centre class is anchored exactly at
# 'midpoint'. With symmetric = TRUE the limits are widened to be symmetric
# around the midpoint, so equal deviations get equal colour intensity. Between
# the Brewer classes ggplot2 interpolates in CIELAB (as scale_fill_distiller does).
cf_scale_diverging <- function(policy, midpoint, limits, aesthetic = "fill",
  name = ggplot2::waiver(), symmetric = TRUE,
  na_value = policy$na_colour, oob = c("squish", "censor"), ...) {
  aesthetic <- cf_aesthetic(aesthetic)
  oob <- match.arg(oob)
  if (missing(midpoint) || !is.numeric(midpoint) || length(midpoint) != 1L || !is.finite(midpoint)) {
    stop("A diverging scale needs one finite, meaningful 'midpoint'.", call. = FALSE)
  }
  if (missing(limits)) stop("A diverging scale needs explicit 'limits'.", call. = FALSE)
  limits <- cf_limits(limits)
  if (midpoint <= limits[1] || midpoint >= limits[2]) {
    stop("The midpoint must lie strictly inside the limits.", call. = FALSE)
  }
  if (cf_flag(symmetric, "symmetric")) limits <- midpoint + c(-1, 1) * max(abs(limits - midpoint))
  info <- cf_brewer_info(policy$diverging_palette, "div", policy)
  colours <- RColorBrewer::brewer.pal(info$maxcolors, info$name)
  if (length(colours) %% 2L != 1L) {
    stop("The diverging palette needs an odd number of classes to have a neutral centre.", call. = FALSE)
  }
  centre <- (length(colours) + 1L) %/% 2L
  position <- (midpoint - limits[1]) / (limits[2] - limits[1])
  values <- c(seq(0, position, length.out = centre), seq(position, 1, length.out = centre)[-1L])
  args <- list(name = name, colours = colours, values = values, limits = limits,
    na.value = na_value, guide = "colourbar", aesthetics = aesthetic, ...)
  if (identical(oob, "squish")) args$oob <- cf_squish
  do.call(ggplot2::scale_colour_gradientn, args)
}

# Redundant non-colour encodings, stable over all_levels.
cf_shape_set <- c(16L, 17L, 15L, 3L, 7L, 8L, 4L, 1L)
cf_linetype_set <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

cf_encoding <- function(levels, all_levels, set, what) {
  lv <- cf_resolve_levels(levels, all_levels)
  if (length(lv$all) > length(set)) {
    stop(length(lv$all), " levels exceed the ", length(set), " distinct ", what,
      " available; use labelled facets or direct labels.", call. = FALSE)
  }
  out <- set[seq_along(lv$all)]
  names(out) <- lv$all
  out <- out[lv$levels]
  attr(out, "unused_levels") <- lv$unused
  out
}

cf_shapes <- function(levels, all_levels = NULL) cf_encoding(levels, all_levels, cf_shape_set, "shapes")

cf_linetypes <- function(levels, all_levels = NULL) cf_encoding(levels, all_levels, cf_linetype_set, "line types")

# Colour arithmetic -------------------------------------------------------

# Composite (possibly translucent) colours over the opaque background.
cf_opaque <- function(colours, background) {
  rgba <- grDevices::col2rgb(colours, alpha = TRUE) / 255
  bg <- grDevices::col2rgb(background)[, 1] / 255
  alpha <- rep(rgba[4, ], each = 3L)
  rgb <- rgba[1:3, , drop = FALSE] * alpha + bg * (1 - alpha)
  grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ])
}

# sRGB -> CIELAB (D65 white) via grDevices.
cf_lab <- function(hex) {
  grDevices::convertColor(t(grDevices::col2rgb(hex)) / 255, from = "sRGB", to = "Lab")
}

# CIEDE2000 colour difference (Sharma, Wu and Dalal 2005) between matching
# rows of two n x 3 Lab matrices.
cf_delta_e2000 <- function(lab1, lab2) {
  rad <- pi / 180
  c1 <- sqrt(lab1[, 2]^2 + lab1[, 3]^2)
  c2 <- sqrt(lab2[, 2]^2 + lab2[, 3]^2)
  cbar7 <- ((c1 + c2) / 2)^7
  g <- 0.5 * (1 - sqrt(cbar7 / (cbar7 + 25^7)))
  a1 <- (1 + g) * lab1[, 2]
  a2 <- (1 + g) * lab2[, 2]
  c1p <- sqrt(a1^2 + lab1[, 3]^2)
  c2p <- sqrt(a2^2 + lab2[, 3]^2)
  hue <- function(b, a) {
    h <- atan2(b, a) / rad
    h[h < 0] <- h[h < 0] + 360
    h[a == 0 & b == 0] <- 0
    h
  }
  h1 <- hue(lab1[, 3], a1)
  h2 <- hue(lab2[, 3], a2)
  chroma0 <- c1p * c2p == 0
  dh <- h2 - h1
  dh <- ifelse(chroma0, 0, ifelse(dh > 180, dh - 360, ifelse(dh < -180, dh + 360, dh)))
  dl <- lab2[, 1] - lab1[, 1]
  dc <- c2p - c1p
  dhh <- 2 * sqrt(c1p * c2p) * sin(dh * rad / 2)
  lbar <- (lab1[, 1] + lab2[, 1]) / 2
  cbarp <- (c1p + c2p) / 2
  hsum <- h1 + h2
  hbar <- ifelse(chroma0, hsum, ifelse(
    abs(h1 - h2) <= 180, hsum / 2,
    ifelse(hsum < 360, (hsum + 360) / 2, (hsum - 360) / 2)
  ))
  tt <- 1 - 0.17 * cos((hbar - 30) * rad) + 0.24 * cos(2 * hbar * rad) +
    0.32 * cos((3 * hbar + 6) * rad) - 0.20 * cos((4 * hbar - 63) * rad)
  dtheta <- 30 * exp(-((hbar - 275) / 25)^2)
  rc <- 2 * sqrt(cbarp^7 / (cbarp^7 + 25^7))
  sl <- 1 + 0.015 * (lbar - 50)^2 / sqrt(20 + (lbar - 50)^2)
  sc <- 1 + 0.045 * cbarp
  sh <- 1 + 0.015 * cbarp * tt
  rt <- -sin(2 * dtheta * rad) * rc
  sqrt((dl / sl)^2 + (dc / sc)^2 + (dhh / sh)^2 + rt * (dc / sc) * (dhh / sh))
}

# WCAG 2.x relative luminance and contrast ratio.
cf_luminance <- function(hex) {
  x <- grDevices::col2rgb(hex) / 255
  x <- ifelse(x <= 0.04045, x / 12.92, ((x + 0.055) / 1.055)^2.4)
  colSums(x * c(0.2126, 0.7152, 0.0722))
}

cf_contrast <- function(hex, background) {
  a <- cf_luminance(hex)
  b <- cf_luminance(background)
  (pmax(a, b) + 0.05) / (pmin(a, b) + 0.05)
}

# Accessibility check of discrete colours, one row per condition: normal
# vision, the requested dichromacy simulations (colorspace, Machado et al.
# 2009 model, severity 1) and grayscale (colorspace::desaturate, amount 1).
#
# - min_delta_e: smallest pairwise CIEDE2000 difference. The default 10 is a
#   conservative heuristic for small plot marks (several times the ~1-2 unit
#   difference noticeable between large uniform patches); it is not a standard.
# - min_contrast: smallest WCAG contrast ratio against the background; the
#   default 3 follows WCAG 2.1 SC 1.4.11 for graphical objects. Use NA to
#   report but not judge contrast (for example adjacent heatmap tiles).
# Translucent colours are composited over the background first. A warning is
# a finding to record and mitigate (non-colour encodings, facets, labels);
# a pass is a simulation result, not a guarantee for every observer.
cf_check_accessibility <- function(colours, background = "#FFFFFF", min_delta_e = 10,
  min_contrast = 3, na_colour = NULL,
  checks = c("protanopia", "deuteranopia", "tritanopia", "grayscale")) {
  if (!is.character(colours) || !length(colours) || anyNA(colours)) {
    stop("'colours' must be a nonempty character vector without NA.", call. = FALSE)
  }
  labels <- names(colours)
  if (is.null(labels)) labels <- unname(colours)
  labels[!nzchar(labels)] <- unname(colours)[!nzchar(labels)]
  colours <- unname(colours)
  if (!is.null(na_colour)) {
    if (!cf_is_hex(na_colour)) stop("'na_colour' must be a hex colour.", call. = FALSE)
    colours <- c(colours, na_colour)
    labels <- c(labels, "(missing)")
  }
  cf_positive(min_delta_e, "min_delta_e")
  if (!(length(min_contrast) == 1L && is.na(min_contrast))) cf_positive(min_contrast, "min_contrast")
  checks <- cf_check_names(as.character(unlist(checks, use.names = FALSE)))
  background <- cf_opaque(background, "#FFFFFF")
  opaque <- cf_opaque(colours, background)
  simulations <- list(
    normal = list(label = "none", fun = function(x) x),
    protanopia = list(label = "colorspace::protan(severity = 1)", fun = function(x) colorspace::protan(x, severity = 1)),
    deuteranopia = list(label = "colorspace::deutan(severity = 1)", fun = function(x) colorspace::deutan(x, severity = 1)),
    tritanopia = list(label = "colorspace::tritan(severity = 1)", fun = function(x) colorspace::tritan(x, severity = 1)),
    grayscale = list(label = "colorspace::desaturate(amount = 1)", fun = function(x) colorspace::desaturate(x, amount = 1))
  )
  rows <- lapply(c("normal", checks), function(check) {
    sim <- simulations[[check]]
    shown <- sim$fun(opaque)
    shown_bg <- sim$fun(background)
    min_de <- NA_real_
    pair <- NA_character_
    if (length(shown) > 1L) {
      lab <- cf_lab(shown)
      index <- utils::combn(length(shown), 2L)
      de <- cf_delta_e2000(lab[index[1L, ], , drop = FALSE], lab[index[2L, ], , drop = FALSE])
      best <- which.min(de)
      min_de <- de[[best]]
      pair <- paste(labels[index[, best]], collapse = " | ")
    }
    contrast <- cf_contrast(shown, shown_bg)
    findings <- character()
    if (!is.na(min_de) && min_de < min_delta_e) {
      findings <- c(findings, sprintf("closest pair %s differs by CIEDE2000 %.1f < %.1f", pair, min_de, min_delta_e))
    }
    if (!is.na(min_contrast) && min(contrast) < min_contrast) {
      findings <- c(findings, sprintf(
        "%s has contrast %.2f:1 < %.1f:1 against the background",
        labels[[which.min(contrast)]], min(contrast), min_contrast
      ))
    }
    data.frame(
      check = check, simulation = sim$label, n_colours = length(shown),
      min_delta_e = round(min_de, 2), closest_pair = pair,
      min_contrast = round(min(contrast), 2), lowest_contrast = labels[[which.min(contrast)]],
      status = if (length(findings)) "warning" else "pass",
      note = if (length(findings)) {
        paste0(paste(findings, collapse = "; "), "; rely on the non-colour encoding and record this finding")
      } else {
        "meets the thresholds in simulation; not a guarantee for every observer"
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  attr(out, "method") <- list(
    delta_e = "CIEDE2000 on sRGB converted to CIELAB (D65) with grDevices::convertColor",
    min_delta_e = min_delta_e,
    contrast = "WCAG 2.x relative-luminance contrast ratio",
    min_contrast = min_contrast,
    simulation = paste("colorspace", cf_package_version("colorspace")),
    background = background,
    statement = "Colour-vision simulations are checks, not a guarantee for every observer."
  )
  out
}

# Panel composition -------------------------------------------------------

# Compose ggplot panels with patchwork. Panels keep their own coordinate
# systems and aspect ratios. Guides are collected only when the caller asserts
# that the panels share identical scales and meanings (collect_guides = TRUE).
# The layout record is attached as attribute "cf_layout"; compose last, after
# adding themes to the individual panels.
cf_compose <- function(plots, ncol = NULL, tags = TRUE, collect_guides = FALSE,
  widths = NULL, heights = NULL) {
  if (inherits(plots, "ggplot") || !is.list(plots) || !length(plots)) {
    stop("'plots' must be a nonempty list of ggplot panels.", call. = FALSE)
  }
  if (!all(vapply(plots, function(p) inherits(p, "ggplot"), logical(1)))) {
    stop("Every panel must be a ggplot object; wrap other graphics explicitly ",
      "with a reviewed patchwork wrapper first.", call. = FALSE)
  }
  cf_flag(tags, "tags")
  cf_flag(collect_guides, "collect_guides")
  ids <- names(plots)
  if (is.null(ids)) ids <- rep("", length(plots))
  ids[!nzchar(ids)] <- paste0("panel_", seq_along(plots))[!nzchar(ids)]
  if (anyDuplicated(ids)) stop("Panel names must be unique.", call. = FALSE)
  if (tags && length(plots) > 26L) stop("Use at most 26 tagged panels per figure.", call. = FALSE)
  if (!is.null(ncol) && (!is.numeric(ncol) || length(ncol) != 1L || ncol < 1 || ncol != round(ncol))) {
    stop("'ncol' must be NULL or one positive whole number.", call. = FALSE)
  }
  if (!is.null(widths)) cf_positive(widths, "widths", scalar = FALSE)
  if (!is.null(heights)) cf_positive(heights, "heights", scalar = FALSE)
  guides <- if (collect_guides) "collect" else "keep"
  composed <- patchwork::wrap_plots(unname(plots), ncol = ncol, widths = widths,
    heights = heights, guides = guides)
  if (tags) composed <- composed + patchwork::plot_annotation(tag_levels = "A")
  attr(composed, "cf_layout") <- list(
    composer = "patchwork", composer_version = cf_package_version("patchwork"),
    panel_ids = ids, panel_order = seq_along(ids),
    tags = if (tags) LETTERS[seq_along(ids)] else character(),
    ncol = ncol, widths = widths, heights = heights, guides = guides
  )
  composed
}

# Figure spec and export --------------------------------------------------

cf_layout_json <- function(layout) {
  if (is.null(layout)) return(NULL)
  for (key in c("panel_ids", "panel_order", "tags", "widths", "heights")) {
    if (!is.null(layout[[key]])) layout[[key]] <- I(layout[[key]])
  }
  layout
}

# Figure spec for the JSON sidecar. 'mapping' is a named list of aesthetics,
# each a list with 'variable' and 'type' (continuous, diverging, categorical,
# ordered) plus optional limits, midpoint, transform, units and levels.
# Without explicit 'checks', supplied discrete colours are checked together
# with the policy NA colour.
cf_figure_spec <- function(id, mapping, policy, colours = NULL, encodings = NULL,
  width, height, units = "in", dpi = 300,
  background = "#FFFFFF", checks = NULL, panels = NULL,
  provenance = NULL) {
  policy <- cf_check_policy(policy)
  if (!cf_is_string(id) || !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", id)) {
    stop("'id' must be a portable figure identifier.", call. = FALSE)
  }
  types <- c("continuous", "diverging", "categorical", "ordered")
  if (!is.list(mapping) || !length(mapping) || is.null(names(mapping)) || any(!nzchar(names(mapping)))) {
    stop("'mapping' must be a named list of aesthetic mappings.", call. = FALSE)
  }
  for (aes in names(mapping)) {
    m <- mapping[[aes]]
    if (!is.list(m) || !cf_is_string(m$variable) || !cf_is_string(m$type) || !m$type %in% types) {
      stop("Mapping '", aes, "' needs a 'variable' and a 'type' from: ",
        paste(types, collapse = ", "), ".", call. = FALSE)
    }
    if (identical(m$type, "diverging") &&
        (!is.numeric(m$midpoint) || length(m$midpoint) != 1L || is.null(m$limits))) {
      stop("Diverging mapping '", aes, "' must record its midpoint and limits.", call. = FALSE)
    }
    for (key in c("limits", "levels")) {
      if (!is.null(m[[key]])) mapping[[aes]][[key]] <- I(m[[key]])
    }
  }
  redundant <- c("shape", "linetype", "label", "facet", "pattern")
  if (isTRUE(policy$redundant_encoding_required) &&
      any(vapply(mapping, function(m) identical(m$type, "categorical"), logical(1))) &&
      !any(names(encodings) %in% redundant)) {
    stop("Categorical colour needs a non-colour encoding (", paste(redundant, collapse = ", "),
      ") in 'encodings'.", call. = FALSE)
  }
  cf_positive(width, "width")
  cf_positive(height, "height")
  cf_positive(dpi, "dpi")
  if (!cf_is_string(units) || !units %in% c("in", "cm", "mm", "px")) {
    stop("'units' must be one of in, cm, mm, px.", call. = FALSE)
  }
  background <- cf_opaque(background, "#FFFFFF")
  if (!is.null(colours) && (!is.character(colours) || is.null(names(colours)))) {
    stop("'colours' must be a named level -> colour map.", call. = FALSE)
  }
  if (is.null(checks) && length(colours)) {
    checks <- cf_check_accessibility(colours, background, na_colour = policy$na_colour,
      checks = policy$checks)
  }
  accessibility <- if (is.null(checks)) {
    list(status = "not_checked",
      note = "No discrete colours were checked; continuous legends need readable labels, contours or tabular data.")
  } else {
    list(
      status = if (all(checks$status == "pass")) "pass" else "warning",
      method = attr(checks, "method"),
      checks = checks,
      statement = "Colour-vision simulations are checks, not a guarantee for every observer."
    )
  }
  brewer <- cf_package_version("RColorBrewer")
  list(
    figure_spec_version = 1L,
    id = id,
    mapping = mapping,
    palettes = list(
      continuous = list(provider = "viridisLite", name = policy$continuous_palette,
        option = cf_option(policy), direction = 1L, version = cf_package_version("viridisLite")),
      categorical = list(provider = "RColorBrewer", name = policy$categorical_palette,
        version = brewer, pinned = policy$pinned_provider_version),
      diverging = list(provider = "RColorBrewer", name = policy$diverging_palette,
        direction = 1L, version = brewer, pinned = policy$pinned_provider_version)
    ),
    colours = if (length(colours)) as.list(stats::setNames(as.character(colours), names(colours))) else NULL,
    unused_levels = I(as.character(attr(colours, "unused_levels"))),
    na = list(colour = policy$na_colour,
      policy = "Missing values use the distinct NA colour; 'unknown' is a separate explicit level."),
    encodings = lapply(encodings, function(e) {
      if (is.atomic(e) && !is.null(names(e))) as.list(stats::setNames(as.vector(e), names(e))) else e
    }),
    output = list(width = width, height = height, units = units, dpi = dpi, background = background),
    panels = cf_layout_json(panels),
    accessibility = accessibility,
    provenance = list(
      r_version = R.version.string,
      packages = lapply(
        stats::setNames(nm = c("ggplot2", "patchwork", "RColorBrewer", "viridisLite", "colorspace")),
        cf_package_version
      ),
      supplied = provenance
    )
  )
}

# Save the figure at its final size and write '<file>.json' next to it. The
# export size must equal the spec, so the sidecar describes the actual file.
cf_save <- function(plot, file, spec, width = spec$output$width, height = spec$output$height,
  units = spec$output$units, dpi = spec$output$dpi) {
  if (!inherits(plot, "ggplot")) stop("'plot' must be a ggplot or patchwork object.", call. = FALSE)
  if (!cf_is_string(file)) stop("'file' must be one file path.", call. = FALSE)
  if (!is.list(spec) || !is.list(spec$output)) stop("'spec' must come from cf_figure_spec().", call. = FALSE)
  same <- isTRUE(all.equal(c(width, height, dpi), c(spec$output$width, spec$output$height, spec$output$dpi))) &&
    identical(units, spec$output$units)
  if (!same) {
    stop("The export size differs from the figure spec; update the spec so the sidecar ",
      "describes the actual output.", call. = FALSE)
  }
  if (is.null(spec$panels) && !is.null(attr(plot, "cf_layout"))) {
    spec$panels <- cf_layout_json(attr(plot, "cf_layout"))
  }
  ggplot2::ggsave(filename = file, plot = plot, width = width, height = height,
    units = units, dpi = dpi, bg = spec$output$background, limitsize = TRUE)
  spec$output$file <- basename(file)
  sidecar <- paste0(file, ".json")
  jsonlite::write_json(spec, sidecar, auto_unbox = TRUE, pretty = TRUE, null = "null",
    na = "null", digits = NA)
  invisible(c(image = file, sidecar = sidecar))
}
