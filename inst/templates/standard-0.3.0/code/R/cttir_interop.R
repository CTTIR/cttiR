# Reviewed static interoperability helpers for Bioconductor and Seurat objects.
#
# Adapters: interop.bioc_s4 1.0.0 (ci_validate_s4, ci_conversion_report),
# interop.se_tidy_view 1.0.0 (ci_se_tidy_view) and interop.seurat_v5 1.0.0
# (ci_seurat_layers, ci_pseudobulk, ci_convert).
#
# Rules followed by every function in this file:
# - only public accessors, called through explicit namespaces; no packages are
#   attached and no data, references or packages are downloaded;
# - assays are never realized as a whole: views realize an explicit bounded
#   slice and checks walk column blocks of at most `ci_block_values` values;
# - a successful coercion or equal dimensions never count as a lossless
#   conversion; every field is reported as preserved, transformed or lost;
# - cells are not biological replicates: aggregation is by donor and group.

ci_block_values <- 1e6

ci_stop <- function(...) stop(paste0(...), call. = FALSE)

ci_prefix <- function(prefix, x) if (length(x)) paste0(prefix, x) else character()

ci_need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    ci_stop("Package '", pkg, "' is required for this step but is not installed. Install it deliberately; nothing is installed automatically.")
  }
  invisible(TRUE)
}

# Load the namespace that defines an S4 object's class so that its documented
# methods dispatch (for example after readRDS()). Only installed packages named
# by the object's own class attribute are loaded.
ci_load_class_pkg <- function(x) {
  pkg <- attr(class(x), "package")
  if (isS4(x) && is.character(pkg) && length(pkg) == 1L && nzchar(pkg) && !isNamespaceLoaded(pkg)) ci_need(pkg)
  invisible(TRUE)
}

ci_class_label <- function(x) {
  pkg <- attr(class(x), "package")
  if (is.character(pkg) && length(pkg) == 1L && nzchar(pkg)) paste0(pkg, "::", class(x)[[1L]]) else class(x)[[1L]]
}

ci_storage <- function(m) {
  if (methods::is(m, "DelayedArray")) return("delayed")
  if (methods::is(m, "IterableMatrix")) return("disk_backed")
  if (methods::is(m, "sparseMatrix")) return("sparse")
  if (is.matrix(m) || methods::is(m, "denseMatrix")) return("dense")
  "other"
}

ci_col_blocks <- function(nr, nc, block_values = ci_block_values) {
  if (nc < 1L) return(list())
  per <- max(1L, as.integer(floor(block_values / max(1L, nr))))
  split(seq_len(nc), ceiling(seq_len(nc) / per))
}

ci_block <- function(m, j) {
  b <- as.matrix(m[, j, drop = FALSE])
  dimnames(b) <- NULL
  b
}

# Value facts computed block-wise; never realizes more than one column block.
ci_value_facts <- function(m) {
  lo <- Inf
  hi <- -Inf
  integer_valued <- TRUE
  missing <- 0
  for (j in ci_col_blocks(nrow(m), ncol(m))) {
    b <- ci_block(m, j)
    missing <- missing + sum(is.na(b))
    b <- b[!is.na(b)]
    if (length(b)) {
      lo <- min(lo, b)
      hi <- max(hi, b)
      if (integer_valued && any(!is.finite(b) | b != round(b))) integer_valued <- FALSE
    }
  }
  list(min = lo, max = hi, integer_valued = integer_valued, nonnegative = lo >= 0, n_missing = missing)
}

ci_values_equal <- function(a, b) {
  if (!identical(as.integer(dim(a)), as.integer(dim(b)))) return(FALSE)
  for (j in ci_col_blocks(nrow(a), ncol(a))) {
    if (!isTRUE(all.equal(ci_block(a, j), ci_block(b, j), tolerance = 1e-12, check.attributes = FALSE))) return(FALSE)
  }
  TRUE
}

# Values at (row, column) position pairs; realizes only the needed rows, one
# column block at a time.
ci_lookup <- function(m, fi, si) {
  uf <- unique(fi)
  us <- unique(si)
  out <- numeric(length(fi))
  for (b in ci_col_blocks(length(uf), length(us))) {
    cols <- us[b]
    slice <- as.matrix(m[uf, cols, drop = FALSE])
    hit <- which(si %in% cols)
    out[hit] <- as.numeric(slice[cbind(match(fi[hit], uf), match(si[hit], cols))])
  }
  out
}

# Positions of source keys in target keys plus a status describing the match.
ci_align_keys <- function(src, tgt, what) {
  if (is.null(src)) {
    return(list(index = NULL, status = if (is.null(tgt)) "preserved" else "transformed",
        detail = paste("source has no", what, "names")))
  }
  if (is.null(tgt)) return(list(index = NULL, status = "lost", detail = paste(what, "names are absent in the target")))
  if (identical(as.character(src), as.character(tgt))) {
    return(list(index = seq_along(src), status = "preserved", detail = paste(length(src), what, "keys identical in identical order")))
  }
  m <- match(src, tgt)
  if (!anyNA(m) && length(tgt) == length(src)) {
    return(list(index = m, status = "transformed", detail = paste("same", length(src), what, "keys in a different order")))
  }
  if (length(tgt) == length(src) && anyNA(m)) {
    m2 <- match(gsub("_", "-", src, fixed = TRUE), tgt)
    if (!anyNA(m2)) {
      renamed <- which(src != tgt[m2])
      return(list(index = m2, status = "transformed", detail = paste0(length(renamed), " of ", length(src), " ", what,
            " keys renamed (e.g. '", src[renamed[1L]], "' -> '", tgt[m2[renamed[1L]]], "')")))
    }
  }
  kept <- sum(!is.na(m))
  if (kept == 0L) return(list(index = m, status = "lost", detail = paste("no source", what, "keys found in the target")))
  extra <- sum(!tgt %in% src)
  list(index = m, status = "transformed", detail = paste0(kept, " of ", length(src), " source ", what, " keys retained",
      if (extra) paste0("; ", extra, " target keys without source counterpart") else ""))
}

# Atomic columns of a data.frame/DataFrame with their keys; nested columns are
# listed separately and never flattened.
ci_columns <- function(df) {
  if (is.null(df) || ncol(df) == 0L) {
    return(list(keys = if (is.null(df)) NULL else rownames(df), atomic = list(), nested = character(), nested_values = list()))
  }
  cols <- as.list(df)
  atomic <- vapply(cols, function(v) is.atomic(v) && is.null(dim(v)), logical(1))
  list(keys = rownames(df), atomic = cols[atomic], nested = names(cols)[!atomic], nested_values = cols[!atomic])
}

ci_meta <- function(md) {
  if (!length(md)) return(list())
  n <- names(md)
  if (is.null(n)) n <- rep("", length(md))
  n[!nzchar(n)] <- paste0("[[", which(!nzchar(n)), "]]")
  names(md) <- n
  md
}

ci_assay_names <- function(x) {
  n_assays <- length(SummarizedExperiment::assays(x, withDimnames = FALSE))
  nm <- SummarizedExperiment::assayNames(x)
  if (is.null(nm)) nm <- rep("", n_assays)
  nm[!nzchar(nm)] <- paste0("assay", which(!nzchar(nm)))
  nm
}

ci_experiment_se <- function(x) {
  nm <- ci_assay_names(x)
  assays <- lapply(seq_along(nm), function(i) SummarizedExperiment::assay(x, i, withDimnames = TRUE))
  names(assays) <- nm
  list(features = rownames(x), assays = assays, row_data = ci_columns(SummarizedExperiment::rowData(x)),
    assay_class = ci_class_label(x))
}

ci_experiment_seurat <- function(x, a) {
  ao <- x[[a]]
  layers <- SeuratObject::Layers(ao)
  mats <- lapply(layers, function(l) SeuratObject::LayerData(x, assay = a, layer = l))
  names(mats) <- layers
  list(features = rownames(ao), assays = mats, row_data = ci_columns(tryCatch(ao[[]], error = function(e) NULL)),
    assay_class = ci_class_label(ao))
}

ci_profile <- function(x) {
  ci_load_class_pkg(x)
  if (methods::is(x, "Seurat")) {
    ci_need("SeuratObject")
    def <- SeuratObject::DefaultAssay(x)
    exps <- lapply(SeuratObject::Assays(x), function(a) ci_experiment_seurat(x, a))
    names(exps) <- SeuratObject::Assays(x)
    reduced <- lapply(SeuratObject::Reductions(x), function(r) SeuratObject::Embeddings(x, reduction = r))
    names(reduced) <- SeuratObject::Reductions(x)
    return(list(kind = "Seurat", class = ci_class_label(x), dim = dim(x), samples = SeuratObject::Cells(x),
        main_name = def, main = exps[[def]], alts = exps[setdiff(names(exps), def)], reduced = reduced,
        col_data = ci_columns(x[[]]), metadata = ci_meta(SeuratObject::Misc(x)),
        vocabulary = list(experiment = "assay", assay = "layer", col_data = "meta.data", alt = "assay")))
  }
  if (methods::is(x, "SummarizedExperiment")) {
    sce <- methods::is(x, "SingleCellExperiment")
    alts <- list()
    reduced <- list()
    main_name <- NA_character_
    if (sce) {
      for (n in SingleCellExperiment::altExpNames(x)) alts[[n]] <- ci_experiment_se(SingleCellExperiment::altExp(x, n))
      for (n in SingleCellExperiment::reducedDimNames(x)) reduced[[n]] <- SingleCellExperiment::reducedDim(x, n)
      mn <- SingleCellExperiment::mainExpName(x)
      if (!is.null(mn)) main_name <- mn
    }
    return(list(kind = if (sce) "SingleCellExperiment" else "SummarizedExperiment", class = ci_class_label(x),
        dim = dim(x), samples = colnames(x), main_name = main_name, main = ci_experiment_se(x), alts = alts,
        reduced = reduced, col_data = ci_columns(SummarizedExperiment::colData(x)), metadata = ci_meta(S4Vectors::metadata(x)),
        vocabulary = list(experiment = "experiment", assay = "assay", col_data = "colData", alt = "altExp")))
  }
  ci_stop("Unsupported object of class '", class(x)[[1L]], "'. Supported: SummarizedExperiment, SingleCellExperiment, Seurat.")
}

ci_layer_aliases <- function(name) {
  groups <- list(c("counts"), c("logcounts", "data"), c("scaledata", "scale.data"))
  for (g in groups) if (name %in% g) return(g)
  name
}

ci_row <- function(field, status, detail) data.frame(field = field, status = status, detail = detail, stringsAsFactors = FALSE)

ci_compare_matrix <- function(src_m, tgt_m) {
  rk <- ci_align_keys(rownames(src_m), rownames(tgt_m), "row")
  ck <- ci_align_keys(colnames(src_m), colnames(tgt_m), "column")
  aligned <- !is.null(rk$index) && !anyNA(rk$index) && !is.null(ck$index) && !anyNA(ck$index)
  if (is.null(rownames(src_m)) && is.null(rownames(tgt_m)) && nrow(src_m) == nrow(tgt_m)) aligned <- !is.null(ck$index) && !anyNA(ck$index)
  if (!aligned) return(list(equal = NA, keys_ok = FALSE, detail = paste("values not compared:", rk$detail, "/", ck$detail)))
  ri <- if (is.null(rk$index)) seq_len(nrow(tgt_m)) else rk$index
  tgt_aligned <- if (identical(ri, seq_len(nrow(tgt_m))) && identical(ck$index, seq_len(ncol(tgt_m)))) tgt_m else tgt_m[ri, ck$index, drop = FALSE]
  equal <- ci_values_equal(src_m, tgt_aligned)
  keys_ok <- rk$status == "preserved" && ck$status == "preserved"
  list(equal = equal, keys_ok = keys_ok, detail = paste0(if (equal) "values identical" else "values differ",
      if (keys_ok) "" else paste0(" after key alignment (", rk$detail, "; ", ck$detail, ")")))
}

ci_compare_assays <- function(src_exp, tgt_exp, prefix, vocab) {
  rows <- list()
  used <- character()
  for (n in names(src_exp$assays)) {
    src_m <- src_exp$assays[[n]]
    cand <- intersect(ci_layer_aliases(n), names(tgt_exp$assays))
    cand <- c(intersect(n, cand), setdiff(cand, n))
    if (!length(cand)) {
      rows[[length(rows) + 1L]] <- ci_row(paste0(prefix, "assay:", n), "lost", paste0("no target ", vocab$assay, " named '", n, "' or a documented equivalent"))
      next
    }
    tn <- cand[[1L]]
    used <- c(used, tn)
    tgt_m <- tgt_exp$assays[[tn]]
    cmp <- ci_compare_matrix(src_m, tgt_m)
    same_class <- identical(class(src_m)[[1L]], class(tgt_m)[[1L]])
    status <- if (identical(tn, n) && same_class && isTRUE(cmp$equal) && cmp$keys_ok) "preserved" else "transformed"
    detail <- paste0(if (!identical(tn, n)) paste0("renamed '", n, "' -> ", vocab$assay, " '", tn, "'; ") else "",
      if (!same_class) paste0("class ", class(src_m)[[1L]], " -> ", class(tgt_m)[[1L]], "; ") else "", cmp$detail)
    rows[[length(rows) + 1L]] <- ci_row(paste0(prefix, "assay:", n), status, detail)
    s_src <- ci_storage(src_m)
    s_tgt <- ci_storage(tgt_m)
    st <- if (same_class) "preserved" else if (s_src %in% c("delayed", "disk_backed", "sparse") && s_tgt != s_src) "lost" else "transformed"
    rows[[length(rows) + 1L]] <- ci_row(paste0(prefix, "storage:", n), st,
      paste0(s_src, " ", class(src_m)[[1L]], " -> ", s_tgt, " ", class(tgt_m)[[1L]],
        if (st == "lost") paste0(" (", s_src, " representation not retained)") else ""))
  }
  for (tn in setdiff(names(tgt_exp$assays), used)) {
    rows[[length(rows) + 1L]] <- ci_row(paste0(prefix, "assay:", tn), "transformed", paste0("added in target as ", vocab$assay, " '", tn, "'; no source counterpart"))
  }
  rows
}

ci_compare_columns <- function(src_cols, tgt_cols, field, key_label) {
  rows <- list()
  for (n in names(src_cols$atomic)) {
    if (!n %in% c(names(tgt_cols$atomic), tgt_cols$nested)) {
      rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), "lost", "column absent in target")
      next
    }
    if (n %in% tgt_cols$nested) {
      rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), "transformed", "atomic column became a nested column")
      next
    }
    k <- ci_align_keys(src_cols$keys, tgt_cols$keys, key_label)
    sv <- src_cols$atomic[[n]]
    tv <- tgt_cols$atomic[[n]]
    idx <- if (is.null(k$index)) if (length(sv) == length(tv)) seq_along(sv) else NULL else k$index
    if (is.null(idx) || anyNA(idx)) {
      rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), "transformed", paste("present but not aligned:", k$detail))
      next
    }
    same_values <- identical(as.character(sv), as.character(tv[idx]))
    same_type <- identical(class(sv), class(tv))
    if (same_values && same_type && identical(idx, seq_along(sv))) {
      rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), "preserved", "values and type identical per key")
    } else {
      rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), "transformed", paste0(
        if (same_values) "values identical per key" else "values differ per key",
        if (!same_type) paste0("; type ", paste(class(sv), collapse = "/"), " -> ", paste(class(tv), collapse = "/")) else "",
        if (!identical(idx, seq_along(sv))) "; keys renamed or reordered" else ""))
    }
  }
  for (n in src_cols$nested) {
    present <- n %in% c(names(tgt_cols$atomic), tgt_cols$nested)
    same <- n %in% tgt_cols$nested && identical(src_cols$keys, tgt_cols$keys) &&
      identical(src_cols$nested_values[[n]], tgt_cols$nested_values[[n]])
    rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), if (same) "preserved" else if (present) "transformed" else "lost",
      if (same) "nested column identical with identical keys" else if (present) "nested column present but not identical" else "nested column absent in target")
  }
  for (n in setdiff(c(names(tgt_cols$atomic), tgt_cols$nested), c(names(src_cols$atomic), src_cols$nested))) {
    rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), "transformed", "added in target; no source counterpart")
  }
  rows
}

ci_match_experiment <- function(name, exp, targets, exclude) {
  pool <- targets[setdiff(names(targets), exclude)]
  if (!is.na(name) && name %in% names(pool)) return(name)
  if (!length(pool) || is.null(exp$features)) return(NA_character_)
  norm <- gsub("_", "-", exp$features, fixed = TRUE)
  score <- vapply(pool, function(t) sum(exp$features %in% t$features | norm %in% t$features), numeric(1))
  if (max(score) > 0) names(pool)[which.max(score)] else NA_character_
}

ci_report_objects <- function(src, tgt) {
  rows <- list(ci_row("class", if (identical(src$class, tgt$class)) "preserved" else "transformed", paste(src$class, "->", tgt$class)))
  same_dim <- identical(as.integer(src$dim), as.integer(tgt$dim))
  rows[[2L]] <- ci_row("dimensions", if (same_dim) "preserved" else "transformed", paste0(paste(src$dim, collapse = " x "), " -> ",
      paste(tgt$dim, collapse = " x "), "; equal dimensions are not evidence of a lossless conversion"))
  sk <- ci_align_keys(src$samples, tgt$samples, "sample")
  rows[[3L]] <- ci_row("sample_keys", sk$status, sk$detail)
  tgt_exps <- c(stats::setNames(list(tgt$main), if (is.na(tgt$main_name)) ".main" else tgt$main_name), tgt$alts)
  alt_names <- intersect(names(src$alts), names(tgt_exps))
  main_match <- ci_match_experiment(src$main_name, src$main, tgt_exps, alt_names)
  if (is.na(main_match)) {
    rows[[length(rows) + 1L]] <- ci_row("main_experiment", "lost", "no target experiment matches the source main experiment")
  } else {
    is_main <- identical(main_match, names(tgt_exps)[[1L]])
    rows[[length(rows) + 1L]] <- ci_row("main_experiment", if (is_main && identical(src$main$assay_class, tgt$main$assay_class)) "preserved" else "transformed",
      paste0("source main ", src$vocabulary$experiment, " -> target ", if (is_main) "main/default " else "non-default ",
        tgt$vocabulary$experiment, " '", main_match, "' (", tgt_exps[[main_match]]$assay_class, ")"))
    te <- tgt_exps[[main_match]]
    fk <- ci_align_keys(src$main$features, te$features, "feature")
    rows[[length(rows) + 1L]] <- ci_row("feature_keys", fk$status, fk$detail)
    rows <- c(rows, ci_compare_assays(src$main, te, "", tgt$vocabulary))
    rd_src <- src$main$row_data
    if (is.null(rd_src$keys)) rd_src$keys <- src$main$features
    rd_tgt <- te$row_data
    if (is.null(rd_tgt$keys)) rd_tgt$keys <- te$features
    rows <- c(rows, ci_compare_columns(rd_src, rd_tgt, "rowData", "feature"))
  }
  for (n in names(src$alts)) {
    if (!n %in% names(tgt_exps)) {
      rows[[length(rows) + 1L]] <- ci_row(paste0("altExp:", n), "lost", paste0("no target ", tgt$vocabulary$alt, " named '", n, "'"))
      next
    }
    sub <- do.call(rbind, ci_compare_assays(src$alts[[n]], tgt_exps[[n]], "", tgt$vocabulary))
    fk <- ci_align_keys(src$alts[[n]]$features, tgt_exps[[n]]$features, "feature")
    role <- if (identical(n, names(tgt_exps)[[1L]])) "became the target main/default " else "kept as target "
    ok <- fk$status == "preserved" && (is.null(sub) || all(sub$status == "preserved"))
    rows[[length(rows) + 1L]] <- ci_row(paste0("altExp:", n), if (ok && role == "kept as target ") "preserved" else "transformed",
      paste0(role, tgt$vocabulary$alt, " '", n, "' (", tgt_exps[[n]]$assay_class, "); features: ", fk$status,
        if (!is.null(sub)) paste0("; ", paste(paste0(sub$field, " ", sub$status), collapse = ", ")) else ""))
  }
  rows <- c(rows, ci_compare_columns(src$col_data, tgt$col_data, "colData", "sample"))
  for (n in names(src$reduced)) {
    hit <- names(tgt$reduced)[tolower(names(tgt$reduced)) == tolower(n)]
    if (!length(hit)) {
      rows[[length(rows) + 1L]] <- ci_row(paste0("reducedDim:", n), "lost", "no target reduction with this name")
      next
    }
    s_m <- as.matrix(src$reduced[[n]])
    t_m <- as.matrix(tgt$reduced[[hit[[1L]]]])
    rk <- ci_align_keys(rownames(s_m), rownames(t_m), "sample")
    idx <- if (is.null(rk$index)) seq_len(nrow(t_m)) else rk$index
    equal <- !anyNA(idx) && identical(dim(s_m), dim(t_m)) &&
      isTRUE(all.equal(unname(s_m), unname(t_m[idx, , drop = FALSE]), tolerance = 1e-12, check.attributes = FALSE))
    same_names <- identical(hit[[1L]], n) && identical(colnames(s_m), colnames(t_m))
    rows[[length(rows) + 1L]] <- ci_row(paste0("reducedDim:", n), if (equal && same_names && rk$status == "preserved") "preserved" else "transformed",
      paste0(if (equal) "embedding values identical" else "embedding values differ or could not be aligned",
        if (!identical(hit[[1L]], n)) paste0("; renamed -> '", hit[[1L]], "'") else "",
        if (!identical(colnames(s_m), colnames(t_m))) paste0("; components ", paste(utils::head(colnames(s_m), 2L), collapse = ","),
          " -> ", paste(utils::head(colnames(t_m), 2L), collapse = ",")) else ""))
  }
  for (n in names(tgt$reduced)[!tolower(names(tgt$reduced)) %in% tolower(names(src$reduced))]) {
    rows[[length(rows) + 1L]] <- ci_row(paste0("reducedDim:", n), "transformed", "added in target; no source counterpart")
  }
  rows <- c(rows, ci_compare_metadata(src$metadata, tgt$metadata))
  rows
}

ci_compare_metadata <- function(src_md, tgt_md) {
  if (!length(src_md)) return(list(ci_row("metadata", "preserved", "source has no metadata entries")))
  lapply(names(src_md), function(n) {
    if (!n %in% names(tgt_md)) return(ci_row(paste0("metadata:", n), "lost", "metadata entry absent in target"))
    if (identical(src_md[[n]], tgt_md[[n]])) ci_row(paste0("metadata:", n), "preserved", "identical") else
      ci_row(paste0("metadata:", n), "transformed", "present but not identical")
  })
}

ci_report_data_frame <- function(src, df) {
  view <- attr(df, "ci_view")
  long <- all(c("feature", "sample", "value") %in% names(df))
  feats <- src$main$features
  samples <- src$samples
  rn <- if (is.character(attr(df, "row.names"))) rownames(df) else NULL
  layout <- if (long) "long" else if (!is.null(rn) && !is.null(feats) && all(rn %in% feats) && length(samples) && all(names(df) %in% samples)) "wide_assay" else
    if (!is.null(rn) && !is.null(samples) && all(rn %in% samples)) "sample_table" else
      if (!is.null(rn) && !is.null(feats) && all(rn %in% feats)) "feature_table" else "unkeyed"
  rows <- list(ci_row("class", "transformed", paste(src$class, "-> data.frame", paste0("(", layout, " layout)"))),
    ci_row("dimensions", "transformed", paste0(paste(src$dim, collapse = " x "), " -> ", nrow(df), " rows x ", ncol(df), " columns")))
  key_row <- function(field, src_keys, present, what) {
    if (is.null(present)) return(ci_row(field, "lost", paste(what, "keys are not represented")))
    present <- unique(as.character(present))
    k <- ci_align_keys(src_keys, present, what)
    ci_row(field, k$status, paste0(k$detail, if (k$status == "preserved") " (as key column)" else ""))
  }
  rows[[3L]] <- key_row("sample_keys", samples, switch(layout, long = df$sample, wide_assay = names(df), sample_table = rn, NULL), "sample")
  rows[[4L]] <- key_row("feature_keys", feats, switch(layout, long = df$feature, wide_assay = rn, feature_table = rn, NULL), "feature")
  view_assay <- if (is.list(view)) view$assay else NA_character_
  for (n in names(src$main$assays)) {
    m <- src$main$assays[[n]]
    status <- "lost"
    detail <- "assay values are not represented"
    if (layout == "long" && nrow(df) && !is.null(rownames(m)) && !is.null(colnames(m))) {
      fi <- match(df$feature, rownames(m))
      si <- match(df$sample, colnames(m))
      if (!anyNA(fi) && !anyNA(si) && (is.na(view_assay) || identical(view_assay, n))) {
        vals <- ci_lookup(m, fi, si)
        if (isTRUE(all.equal(vals, as.numeric(df$value), tolerance = 1e-12, check.attributes = FALSE))) {
          status <- "transformed"
          detail <- paste0("flattened to long 'value' column for ", length(unique(fi)), " of ", nrow(m), " features x ",
            length(unique(si)), " of ", ncol(m), " samples; values identical per key")
        }
      }
    } else if (layout == "wide_assay") {
      fi <- match(rn, rownames(m))
      si <- match(names(df), colnames(m))
      if (!anyNA(fi) && !anyNA(si) && ci_values_equal(m[fi, si, drop = FALSE], as.matrix(df))) {
        status <- "transformed"
        detail <- "dense data.frame columns; values identical per key"
      }
    }
    rows[[length(rows) + 1L]] <- ci_row(paste0("assay:", n), status, detail)
    if (status != "lost") {
      s <- ci_storage(m)
      rows[[length(rows) + 1L]] <- ci_row(paste0("storage:", n), if (s == "dense") "transformed" else "lost",
        paste0(s, " ", class(m)[[1L]], " -> plain data.frame values", if (s != "dense") paste0(" (", s, " representation not retained)") else ""))
    }
  }
  df_cols <- function(keys_vec, prefix) {
    renamed <- if (is.list(view) && length(view$renamed)) view$renamed else character()
    out <- list(keys = keys_vec, atomic = list(), nested = character())
    for (cn in names(df)) {
      orig <- if (cn %in% names(renamed) && startsWith(renamed[[cn]], prefix)) sub(paste0("^", prefix, ":"), "", renamed[[cn]]) else cn
      if (cn %in% names(renamed) && !startsWith(renamed[[cn]], prefix)) next
      out$atomic[[orig]] <- df[[cn]]
    }
    out
  }
  feat_src <- src$main$row_data
  if (is.null(feat_src$keys)) feat_src$keys <- feats
  if (layout == "long") {
    rows <- c(rows, ci_long_columns(feat_src, df, df$feature, "rowData", df_cols(NULL, "rowData")),
      ci_long_columns(src$col_data, df, df$sample, "colData", df_cols(NULL, "colData")))
  } else if (layout == "sample_table") {
    tgt <- df_cols(rn, "colData")
    rows <- c(rows, ci_compare_columns(src$col_data, tgt, "colData", "sample")[seq_along(c(names(src$col_data$atomic), src$col_data$nested))],
      lapply(c(names(feat_src$atomic), feat_src$nested), function(n) ci_row(paste0("rowData:", n), "lost", "feature annotations not represented")))
  } else if (layout == "feature_table") {
    tgt <- df_cols(rn, "rowData")
    rows <- c(rows, ci_compare_columns(feat_src, tgt, "rowData", "feature")[seq_along(c(names(feat_src$atomic), feat_src$nested))],
      lapply(c(names(src$col_data$atomic), src$col_data$nested), function(n) ci_row(paste0("colData:", n), "lost", "sample annotations not represented")))
  } else {
    rows <- c(rows, lapply(c(names(feat_src$atomic), feat_src$nested), function(n) ci_row(paste0("rowData:", n), "lost", "feature annotations not represented")),
      lapply(c(names(src$col_data$atomic), src$col_data$nested), function(n) ci_row(paste0("colData:", n), "lost", "sample annotations not represented")))
  }
  rows <- c(rows, lapply(names(src$alts), function(n) ci_row(paste0("altExp:", n), "lost", "alternative experiments are not represented in a data.frame")),
    lapply(names(src$reduced), function(n) ci_row(paste0("reducedDim:", n), "lost", "reduced dimensions are not represented in a data.frame")))
  md <- ci_compare_metadata(src$metadata, list())
  if (length(src$metadata)) md <- lapply(md, function(r) {
    r$detail <- "object metadata is not represented in a data.frame"
    r
  })
  c(rows, md)
}

ci_long_columns <- function(src_cols, df, keys, field, tgt) {
  rows <- list()
  for (n in names(src_cols$atomic)) {
    if (!n %in% names(tgt$atomic)) {
      rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), "lost", "column absent in target")
      next
    }
    idx <- match(keys, src_cols$keys)
    same <- !anyNA(idx) && identical(as.character(src_cols$atomic[[n]][idx]), as.character(tgt$atomic[[n]])) &&
      identical(class(src_cols$atomic[[n]]), class(tgt$atomic[[n]]))
    rows[[length(rows) + 1L]] <- ci_row(paste0(field, ":", n), if (same) "preserved" else "transformed",
      if (same) "values and type identical per key (repeated per long row)" else "present but values or type differ per key")
  }
  c(rows, lapply(src_cols$nested, function(n) ci_row(paste0(field, ":", n), "lost", "nested column not flattened into the data.frame")))
}

# Bounded long view of one SummarizedExperiment assay slice.
#
# Returns a data.frame with columns feature, sample, feature_index,
# sample_index, value and the atomic rowData/colData columns of the selected
# slice. Selections are sets; rows follow the object's original order
# (samples outer, features inner). Refuses before realizing more than
# `max_cells` values; the stored assay object is never coerced as a whole.
ci_se_tidy_view <- function(se, assay = 1L, features = NULL, samples = NULL, max_cells = 1e5) {
  ci_load_class_pkg(se)
  if (!methods::is(se, "SummarizedExperiment")) ci_stop("se must be a SummarizedExperiment (or a subclass such as SingleCellExperiment).")
  if (!is.numeric(max_cells) || length(max_cells) != 1L || is.na(max_cells) || max_cells < 1) ci_stop("max_cells must be one positive number.")
  nm <- ci_assay_names(se)
  if (!length(nm)) ci_stop("The object has no assays.")
  i <- if (is.character(assay) && length(assay) == 1L && !is.na(assay)) match(assay, SummarizedExperiment::assayNames(se)) else
    if (is.numeric(assay) && length(assay) == 1L && !is.na(assay) && assay == round(assay) && assay >= 1 && assay <= length(nm)) as.integer(assay) else NA_integer_
  if (is.na(i)) ci_stop("assay must name an existing assay or give its position (available: ", paste(nm, collapse = ", "), ").")
  fi <- ci_select(features, rownames(se), nrow(se), "features")
  si <- ci_select(samples, colnames(se), ncol(se), "samples")
  n_values <- as.numeric(length(fi)) * length(si)
  if (n_values > max_cells) {
    ci_stop("The requested slice has ", format(n_values, scientific = FALSE), " values and exceeds max_cells = ",
      format(max_cells, scientific = FALSE), ". Select fewer features or samples; the full assay is not realized.")
  }
  a <- SummarizedExperiment::assay(se, i, withDimnames = FALSE)
  block <- as.matrix(a[fi, si, drop = FALSE])
  if (!identical(as.integer(dim(block)), c(length(fi), length(si)))) ci_stop("The assay slice did not have the requested shape.")
  fkeys <- if (is.null(rownames(se))) as.character(fi) else rownames(se)[fi]
  skeys <- if (is.null(colnames(se))) as.character(si) else colnames(se)[si]
  out <- data.frame(feature = rep(fkeys, times = length(si)), sample = rep(skeys, each = length(fi)),
    feature_index = rep(fi, times = length(si)), sample_index = rep(si, each = length(fi)),
    value = as.vector(block), stringsAsFactors = FALSE)
  rd <- ci_columns(SummarizedExperiment::rowData(se))
  cd <- ci_columns(SummarizedExperiment::colData(se))
  rd_names <- names(rd$atomic)
  cd_names <- names(cd$atomic)
  rd_cols <- ifelse(rd_names %in% c(names(out), cd_names), paste0("rowData_", rd_names), rd_names)
  cd_cols <- ifelse(cd_names %in% c(names(out), rd_names), paste0("colData_", cd_names), cd_names)
  if (anyDuplicated(c(names(out), rd_cols, cd_cols))) ci_stop("rowData/colData column names collide with view columns even after prefixing.")
  for (k in seq_along(rd_names)) out[[rd_cols[[k]]]] <- rep(rd$atomic[[k]][fi], times = length(si))
  for (k in seq_along(cd_names)) out[[cd_cols[[k]]]] <- rep(cd$atomic[[k]][si], each = length(fi))
  renamed <- c(stats::setNames(ci_prefix("rowData:", rd_names), rd_cols)[rd_cols != rd_names],
    stats::setNames(ci_prefix("colData:", cd_names), cd_cols)[cd_cols != cd_names])
  rownames(out) <- NULL
  sce <- methods::is(se, "SingleCellExperiment")
  omitted <- c(ci_prefix("assay:", setdiff(nm, nm[[i]])), ci_prefix("rowData:", rd$nested), ci_prefix("colData:", cd$nested),
    ci_prefix("metadata:", names(ci_meta(S4Vectors::metadata(se)))),
    if (sce) ci_prefix("reducedDim:", SingleCellExperiment::reducedDimNames(se)),
    if (sce) ci_prefix("altExp:", SingleCellExperiment::altExpNames(se)),
    if (length(fi) < nrow(se)) paste0("features outside slice: ", nrow(se) - length(fi)),
    if (length(si) < ncol(se)) paste0("samples outside slice: ", ncol(se) - length(si)),
    paste0("storage: ", ci_storage(a), " ", class(a)[[1L]], " representation (values copied for the slice only)"))
  attr(out, "ci_view") <- list(adapter = "interop.se_tidy_view", adapter_version = "1.0.0",
    source_class = ci_class_label(se), assay = nm[[i]], assay_class = class(a)[[1L]], storage = ci_storage(a),
    source_dim = as.integer(dim(se)), slice_dim = c(length(fi), length(si)), realized_values = n_values,
    max_cells = max_cells, order = "original object order; samples outer, features inner",
    preserved = c("feature keys", "sample keys", paste0("assay:", nm[[i]], " values for the slice"),
      ci_prefix("rowData:", names(rd$atomic)), ci_prefix("colData:", names(cd$atomic))),
    omitted = omitted, renamed = renamed)
  out
}

ci_select <- function(sel, keys, n, what) {
  if (is.null(sel)) return(seq_len(n))
  if (is.character(sel)) {
    if (is.null(keys)) ci_stop("The object has no ", what, " names; select ", what, " by position.")
    if (anyNA(sel) || anyDuplicated(sel)) ci_stop(what, " must be unique and not missing.")
    idx <- match(sel, keys)
    if (anyNA(idx)) ci_stop("Unknown ", what, " keys (key mismatch): ", paste(utils::head(sel[is.na(idx)], 5L), collapse = ", "), ".")
  } else if (is.logical(sel)) {
    if (length(sel) != n || anyNA(sel)) ci_stop("A logical ", what, " selection must have one non-missing value per entry.")
    idx <- which(sel)
  } else if (is.numeric(sel)) {
    if (anyNA(sel) || any(sel != round(sel)) || any(sel < 1 | sel > n) || anyDuplicated(sel)) ci_stop(what, " positions must be unique integers within range.")
    idx <- as.integer(sel)
  } else {
    ci_stop(what, " must be NULL, names, positions or a logical vector.")
  }
  if (!length(idx)) ci_stop("The ", what, " selection is empty.")
  sort(idx)
}

# Field-level conversion report: data.frame(field, status, detail).
#
# `from` is a SummarizedExperiment, SingleCellExperiment or Seurat object;
# `to` is one of those or a data.frame. Status is preserved, transformed or
# lost. Values are compared block-wise after key alignment. The attribute
# "lossless" is TRUE only when every field is preserved; equal dimensions
# alone never make a conversion lossless.
ci_conversion_report <- function(from, to) {
  if (is.data.frame(from)) ci_stop("from must be a SummarizedExperiment, SingleCellExperiment or Seurat object.")
  src <- ci_profile(from)
  rows <- if (is.data.frame(to)) ci_report_data_frame(src, to) else ci_report_objects(src, ci_profile(to))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "lossless") <- all(out$status == "preserved")
  attr(out, "adapter") <- c(id = "interop.bioc_s4", version = "1.0.0")
  out
}

# Explicit SingleCellExperiment <-> Seurat conversion with a loss report.
#
# Calls Seurat::as.Seurat() or Seurat::as.SingleCellExperiment() and returns
# list(object, report). The conversion is never described as lossless unless
# every reported field is preserved.
ci_convert <- function(x, to = c("Seurat", "SingleCellExperiment"), counts = "counts", data = "logcounts") {
  to <- match.arg(to)
  ci_load_class_pkg(x)
  ci_need("Seurat")
  if (identical(to, "Seurat")) {
    if (!methods::is(x, "SingleCellExperiment")) ci_stop("Conversion to Seurat requires a SingleCellExperiment.")
    obj <- Seurat::as.Seurat(x, counts = counts, data = data)
  } else {
    if (!methods::is(x, "Seurat")) ci_stop("Conversion to SingleCellExperiment requires a Seurat object.")
    obj <- Seurat::as.SingleCellExperiment(x)
  }
  list(object = obj, report = ci_conversion_report(x, obj))
}

# Assay/layer inventory of a Seurat object with counts/data semantics checks.
#
# One row per assay layer. Value checks walk column blocks and never realize
# a whole layer at once. `issue` is empty when no problem was detected.
ci_seurat_layers <- function(obj) {
  ci_load_class_pkg(obj)
  ci_need("SeuratObject")
  if (!methods::is(obj, "Seurat")) ci_stop("obj must be a Seurat object.")
  def <- SeuratObject::DefaultAssay(obj)
  rows <- list()
  for (a in SeuratObject::Assays(obj)) {
    ao <- obj[[a]]
    layers <- SeuratObject::Layers(ao)
    for (l in layers) {
      m <- SeuratObject::LayerData(obj, assay = a, layer = l)
      base <- if (startsWith(l, "scale.data")) "scale.data" else sub("\\..*$", "", l)
      suffix <- substring(l, nchar(base) + 1L)
      facts <- ci_value_facts(m)
      issue <- character()
      semantics <- "unrecognized"
      if (base == "counts") {
        semantics <- if (facts$integer_valued && facts$nonnegative && facts$n_missing == 0) "raw_counts" else "non_count_values"
        if (semantics != "raw_counts") issue <- c(issue, "counts layer holds negative, non-integer or missing values; it is not a UMI count matrix (sctransform and count-based pseudobulk models do not apply)")
      } else if (base == "data") {
        semantics <- "normalized"
        counts_layer <- paste0("counts", suffix)
        if (counts_layer %in% layers) {
          cm <- SeuratObject::LayerData(obj, assay = a, layer = counts_layer)
          if (ci_values_equal(cm, m)) {
            semantics <- "unnormalized_copy"
            issue <- c(issue, "data layer equals counts; normalization has not been applied")
          }
        }
      } else if (base == "scale.data") {
        semantics <- "scaled"
      }
      if (nzchar(suffix)) issue <- c(issue, "split layer; join layers explicitly before whole-assay aggregation")
      if (!methods::is(ao, "Assay5")) issue <- c(issue, "assay is not Assay5; Seurat v5 layer semantics are not guaranteed")
      rows[[length(rows) + 1L]] <- data.frame(assay = a, assay_class = class(ao)[[1L]], default_assay = identical(a, def),
        layer = l, split = nzchar(suffix), layer_class = class(m)[[1L]], storage = ci_storage(m),
        n_features = nrow(m), n_cells = ncol(m), semantics = semantics, integer_valued = facts$integer_valued,
        nonnegative = facts$nonnegative, min = facts$min, max = facts$max, issue = paste(issue, collapse = "; "),
        stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "object_version") <- as.character(SeuratObject::Version(obj))
  out
}

# Donor-level pseudobulk: sums raw counts by donor x group.
#
# Accepts a SingleCellExperiment/SummarizedExperiment with a "counts" assay or
# a Seurat object whose default assay has a single "counts" layer. `donor` and
# `group` name sample-level columns. Refuses missing labels, non-count values
# and any group with fewer than two donors. Returns list(counts, samples,
# design); pseudobulk samples, not cells, are the replicates downstream.
ci_pseudobulk <- function(obj_or_sce, donor, group) {
  x <- obj_or_sce
  for (arg in list(donor, group)) {
    if (!is.character(arg) || length(arg) != 1L || is.na(arg) || !nzchar(arg)) ci_stop("donor and group must each name one sample-level column.")
  }
  if (identical(donor, group)) ci_stop("donor and group must be different columns.")
  ci_load_class_pkg(x)
  if (methods::is(x, "Seurat")) {
    ci_need("SeuratObject")
    a <- SeuratObject::DefaultAssay(x)
    layers <- SeuratObject::Layers(x[[a]])
    if (!"counts" %in% layers) {
      if (any(startsWith(layers, "counts."))) ci_stop("Counts are split across layers; join them explicitly (SeuratObject::JoinLayers()) before aggregation.")
      ci_stop("The default assay has no raw counts layer; normalized data are not aggregated.")
    }
    counts <- SeuratObject::LayerData(x, assay = a, layer = "counts")
    meta <- x[[]]
    cells <- SeuratObject::Cells(x)
    source <- paste0("Seurat assay '", a, "' layer 'counts'")
  } else if (methods::is(x, "SummarizedExperiment")) {
    if (!"counts" %in% SummarizedExperiment::assayNames(x)) ci_stop("A 'counts' assay with raw counts is required; normalized assays are not aggregated.")
    counts <- SummarizedExperiment::assay(x, "counts", withDimnames = TRUE)
    meta <- SummarizedExperiment::colData(x)
    cells <- colnames(x)
    source <- paste0(class(x)[[1L]], " assay 'counts'")
  } else {
    ci_stop("obj_or_sce must be a Seurat object or a SingleCellExperiment/SummarizedExperiment.")
  }
  for (col in c(donor, group)) if (!col %in% names(meta)) ci_stop("Column '", col, "' is not present in the cell metadata.")
  d <- meta[[donor]]
  g <- meta[[group]]
  if (length(d) != ncol(counts) || length(g) != ncol(counts)) ci_stop("Cell metadata and counts do not have the same cells.")
  if (anyNA(d) || anyNA(g)) ci_stop("Donor and group labels must not be missing; resolve or exclude those cells explicitly first.")
  g_levels <- if (is.factor(g)) levels(droplevels(g)) else sort(unique(as.character(g)))
  d_levels <- if (is.factor(d)) levels(droplevels(d)) else sort(unique(as.character(d)))
  d <- as.character(d)
  g <- as.character(g)
  pairs <- unique(data.frame(donor = d, group = g, stringsAsFactors = FALSE))
  per_group <- vapply(g_levels, function(lv) length(unique(pairs$donor[pairs$group == lv])), integer(1))
  if (any(per_group < 2L)) {
    ci_stop("Donor-level pseudobulk needs at least 2 donors per group; too few in: ",
      paste0(names(per_group)[per_group < 2L], " (", per_group[per_group < 2L], ")", collapse = ", "),
      ". Cells are not biological replicates.")
  }
  facts <- ci_value_facts(counts)
  if (!facts$integer_valued || !facts$nonnegative || facts$n_missing > 0) ci_stop("counts must be nonnegative integers without missing values (raw counts).")
  pairs <- pairs[order(match(pairs$group, g_levels), match(pairs$donor, d_levels)), , drop = FALSE]
  ids <- paste(pairs$group, pairs$donor, sep = "|")
  j <- match(paste(g, d, sep = "|"), ids)
  ind <- Matrix::sparseMatrix(i = seq_along(j), j = j, x = 1, dims = c(length(j), length(ids)))
  if (methods::is(counts, "sparseMatrix") || is.matrix(counts)) {
    res <- as.matrix(counts %*% ind)
  } else {
    res <- matrix(0, nrow(counts), length(ids))
    for (b in ci_col_blocks(nrow(counts), ncol(counts))) res <- res + ci_block(counts, b) %*% as.matrix(ind[b, , drop = FALSE])
  }
  dimnames(res) <- list(rownames(counts), ids)
  n_cells <- as.integer(tabulate(j, nbins = length(ids)))
  samples <- data.frame(pseudobulk_id = ids, donor = pairs$donor, group = pairs$group, n_cells = n_cells, stringsAsFactors = FALSE)
  list(counts = res, samples = samples, design = list(adapter = "interop.seurat_v5", adapter_version = "1.0.0",
      source = source, donor_column = donor, group_column = group, n_cells = length(cells),
      donors_per_group = per_group, unit = "donor x group pseudobulk sample",
      note = "Cells were summed within donor and group; use pseudobulk samples, not cells, as replicates."))
}

# S4 validity and class facts.
#
# Returns list(class, package, package_version, is_s4, class_defined, valid,
# message, extends). Non-S4 inputs report valid = NA (not applicable).
ci_validate_s4 <- function(x) {
  ci_load_class_pkg(x)
  cls <- class(x)
  pkg <- attr(cls, "package")
  pkg <- if (is.character(pkg) && length(pkg) == 1L) pkg else NA_character_
  version <- if (!is.na(pkg) && nzchar(system.file(package = pkg))) as.character(utils::packageVersion(pkg)) else NA_character_
  if (!isS4(x)) {
    return(list(class = cls[[1L]], package = pkg, package_version = version, is_s4 = FALSE, class_defined = FALSE,
        valid = NA, message = "not an S4 object; S4 validity does not apply", extends = as.character(cls)))
  }
  res <- tryCatch(methods::validObject(x, test = TRUE), error = function(e) conditionMessage(e))
  valid <- isTRUE(res)
  defined <- methods::isClass(cls[[1L]])
  list(class = cls[[1L]], package = pkg, package_version = version, is_s4 = TRUE, class_defined = defined,
    valid = valid, message = if (valid) "" else paste(as.character(res), collapse = "; "),
    extends = if (defined) methods::extends(cls[[1L]]) else cls[[1L]])
}
