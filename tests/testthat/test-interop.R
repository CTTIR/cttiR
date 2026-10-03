# Synthetic, nonclinical fixtures only. No datasets are downloaded and
# SeuratData is never used.

interop_template_path <- function() {
  system.file("templates", "standard-0.3.0", "code", "R", "cttir_interop.R", package = "cttiR")
}

interop_template <- function() {
  path <- interop_template_path()
  expect_true(nzchar(path))
  env <- new.env(parent = baseenv())
  sys.source(path, envir = env, keep.source = FALSE)
  env
}

# A DelayedArray seed that records how many values were realized, so tests can
# prove that only the requested slice is ever extracted.
counting_seed_class <- local({
  generator <- NULL
  function() {
    if (is.null(generator)) {
      where <- new.env()
      generator <<- methods::setClass("CttirCountingSeed", methods::representation(data = "matrix", log = "environment"), where = where)
      methods::setMethod("dim", "CttirCountingSeed", function(x) dim(x@data), where = where)
      methods::setMethod("dimnames", "CttirCountingSeed", function(x) dimnames(x@data), where = where)
      methods::setMethod(DelayedArray::extract_array, "CttirCountingSeed", function(x, index) {
        out <- x@data
        if (!is.null(index[[1L]])) out <- out[index[[1L]], , drop = FALSE]
        if (!is.null(index[[2L]])) out <- out[, index[[2L]], drop = FALSE]
        x@log$max <- max(x@log$max, length(out))
        x@log$total <- x@log$total + length(out)
        out
      }, where = where)
    }
    generator
  }
})

counting_delayed <- function(m) {
  log <- new.env()
  log$max <- 0
  log$total <- 0
  list(array = DelayedArray::DelayedArray(counting_seed_class()(data = m, log = log)), log = log)
}

reset_log <- function(log) {
  log$max <- 0
  log$total <- 0
}

se_fixture <- function() {
  set.seed(20261002)
  samples <- c("s05", "s02", "s08", "s01", "s07", "s03", "s06", "s04")
  counts <- matrix(rpois(48, 2), 6, 8, dimnames = list(paste0("g", 1:6), samples))
  abundance <- matrix(round(rnorm(48, 5), 3), 6, 8, dimnames = dimnames(counts))
  delayed <- counting_delayed(abundance)
  rd <- S4Vectors::DataFrame(symbol = paste0("SYM", 1:6), biotype = factor(c("coding", "lnc", "coding", "coding", "lnc", "coding")))
  rd$ranges <- S4Vectors::DataFrame(start = 1:6 * 100L, end = 1:6 * 100L + 50L)
  cd <- S4Vectors::DataFrame(donor = paste0("donor", c(1, 2, 3, 1, 2, 3, 4, 4)),
    condition = factor(rep(c("ctrl", "treated"), 4)), batch = c(1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L), row.names = samples)
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = Matrix::Matrix(counts, sparse = TRUE), abundance = delayed$array),
    rowData = rd, colData = cd, metadata = list(protocol = "synthetic-v1", label = "nonclinical synthetic fixture"))
  list(se = se, counts = counts, abundance = abundance, log = delayed$log)
}

sce_fixture <- function() {
  set.seed(11)
  cells <- sprintf("cell%02d", 30:1)
  m <- matrix(rpois(180, 2), 6, 30, dimnames = list(paste0("gene_", 1:6), cells))
  adt <- matrix(rpois(60, 5), 2, 30, dimnames = list(c("ADT1", "ADT2"), cells))
  sp <- Matrix::Matrix(m, sparse = TRUE)
  ap <- Matrix::Matrix(adt, sparse = TRUE)
  sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = sp, logcounts = log1p(sp)),
    colData = S4Vectors::DataFrame(donor = rep(c("d1", "d2", "d3"), 10), group = rep(c("A", "B"), each = 15), row.names = cells),
    rowData = S4Vectors::DataFrame(symbol = toupper(rownames(m)), chr = "1"),
    metadata = list(note = "nonclinical synthetic fixture"))
  SingleCellExperiment::mainExpName(sce) <- "RNA"
  SingleCellExperiment::reducedDim(sce, "PCA") <- matrix(rnorm(60), 30, 2, dimnames = list(cells, c("PC1", "PC2")))
  SingleCellExperiment::altExp(sce, "ADT") <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = ap, logcounts = log1p(ap)))
  sce
}

field_status <- function(report, field) report$status[report$field == field]

test_that("the static interop source uses only reviewed namespaces and no package references", {
  path <- interop_template_path()
  text <- readLines(path, warn = FALSE)
  expect_false(any(grepl("cttir", text, ignore.case = TRUE)))
  pd <- utils::getParseData(parse(path, keep.source = TRUE))
  allowed <- c("SummarizedExperiment", "SingleCellExperiment", "S4Vectors", "Matrix", "SeuratObject", "Seurat", "methods", "stats", "utils")
  expect_true(all(unique(pd$text[pd$token == "SYMBOL_PACKAGE"]) %in% allowed))
  expect_false(any(pd$token == "NS_GET_INT"))
  expect_false(any(pd$token == "'@'"))
  calls <- pd$text[pd$token == "SYMBOL_FUNCTION_CALL"]
  expect_false(any(calls %in% c("library", "require", "source", "sys.source", "eval", "evalq", "parse", "install.packages",
        "download.file", "system", "system2", "attach", "setwd", "options")))
  exprs <- parse(path, keep.source = FALSE)
  top <- vapply(exprs, function(e) if (is.call(e) && identical(e[[1L]], as.name("<-"))) as.character(e[[2L]]) else "", character(1))
  expect_true(all(startsWith(top, "ci_")))
  env <- interop_template()
  for (f in c("ci_se_tidy_view", "ci_conversion_report", "ci_seurat_layers", "ci_pseudobulk", "ci_validate_s4", "ci_convert")) {
    expect_true(is.function(env[[f]]))
  }
  expect_equal(names(formals(env$ci_se_tidy_view)), c("se", "assay", "features", "samples", "max_cells"))
  expect_equal(formals(env$ci_se_tidy_view)$max_cells, 1e5)
  expect_equal(names(formals(env$ci_conversion_report)), c("from", "to", "pins"))
  expect_equal(names(formals(env$ci_pseudobulk)), c("obj_or_sce", "donor", "group", "allow_single_cell_samples", "pins"))
  expect_false(formals(env$ci_pseudobulk)$allow_single_cell_samples)
})

test_that("package requirements can enforce exact pinned versions", {
  env <- interop_template()
  installed <- as.character(utils::packageVersion("utils"))
  expect_true(env$ci_need("utils"))
  expect_true(env$ci_need("utils", installed))
  # package_version() semantics: "-" and "." separators compare equal.
  expect_true(env$ci_need("utils", gsub(".", "-", installed, fixed = TRUE)))
  expect_true(env$ci_need("utils", package_version(installed)))
  expect_error(env$ci_need("utils", "0.0.1"), paste0("'utils' ", installed, " is installed but the project pins 0.0.1"))
  expect_error(env$ci_need("utils", "not a version"), "must be one version")
  expect_error(env$ci_need("utils", c("1.0", "2.0")), "must be one version")
  expect_error(env$ci_need("cttirNoSuchPackage", "1.0.0"), "not installed")
  deps <- list(list(package = "Seurat", version = "5.5.1", required = TRUE, stages = list("analysis")),
    list(package = "Matrix", version = NULL, required = TRUE))
  expect_identical(env$ci_pinned_version(deps, "Seurat"), "5.5.1")
  expect_null(env$ci_pinned_version(deps, "Matrix"))
  expect_null(env$ci_pinned_version(deps, "ggplot2"))
  expect_null(env$ci_pinned_version(NULL, "Seurat"))
  expect_identical(env$ci_pinned_version(c(Seurat = "5.5.1"), "Seurat"), "5.5.1")
  expect_identical(env$ci_pinned_version(list(Seurat = "5.5.1"), "Seurat"), "5.5.1")
  table <- data.frame(package = c("Seurat", "Matrix"), version = c("5.5.1", NA))
  expect_identical(env$ci_pinned_version(table, "Seurat"), "5.5.1")
  expect_null(env$ci_pinned_version(table, "Matrix"))
  expect_error(env$ci_pinned_version(data.frame(name = "Seurat"), "Seurat"), "'package' and 'version'")
  expect_error(env$ci_pinned_version("5.5.1", "Seurat"), "dependency list")

  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("DelayedArray")
  se <- se_fixture()$se
  wrong <- list(list(package = "SummarizedExperiment", version = "0.0.1"))
  expect_error(env$ci_conversion_report(se, se, pins = wrong), "'SummarizedExperiment' .* pins 0.0.1")
  right <- list(list(package = "SummarizedExperiment", version = as.character(utils::packageVersion("SummarizedExperiment"))))
  expect_true(attr(env$ci_conversion_report(se, se, pins = right), "lossless"))
})

test_that("a bounded tidy view keeps keys and order without realizing the full assay", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("DelayedArray")
  env <- interop_template()
  fx <- se_fixture()
  se <- fx$se
  before <- env$ci_validate_s4(se)
  expect_true(before$valid)
  expect_true(before$is_s4)
  expect_contains(before$extends, "SummarizedExperiment")
  expect_s4_class(SummarizedExperiment::assay(se, "abundance"), "DelayedMatrix")
  reset_log(fx$log)
  view <- env$ci_se_tidy_view(se, assay = "abundance", features = c("g5", "g2"), samples = c("s01", "s05", "s08"), max_cells = 6)
  expect_equal(fx$log$total, 6)
  expect_lte(fx$log$max, 6)
  expect_equal(view$feature, rep(c("g2", "g5"), 3))
  expect_equal(view$sample, rep(c("s05", "s08", "s01"), each = 2))
  expect_equal(view$feature_index, rep(c(2L, 5L), 3))
  expect_equal(view$sample_index, rep(c(1L, 3L, 4L), each = 2))
  expect_equal(view$value, fx$abundance[cbind(view$feature, view$sample)])
  expect_equal(view$symbol, rep(c("SYM2", "SYM5"), 3))
  expect_s3_class(view$biotype, "factor")
  expect_equal(as.character(view$condition), as.character(SummarizedExperiment::colData(se)[view$sample, "condition"]))
  info <- attr(view, "ci_view")
  expect_equal(info$assay, "abundance")
  expect_equal(info$storage, "delayed")
  expect_equal(info$realized_values, 6)
  expect_contains(info$omitted, c("assay:counts", "rowData:ranges", "metadata:protocol", "metadata:label"))
  expect_contains(info$preserved, c("rowData:symbol", "colData:donor"))
  expect_false("ranges" %in% names(view))
  sparse_view <- env$ci_se_tidy_view(se, assay = 1L, features = 1:2, samples = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(sparse_view$value, as.vector(fx$counts[1:2, c(1, 3)]))
  expect_equal(attr(sparse_view, "ci_view")$storage, "sparse")
  reset_log(fx$log)
  expect_error(env$ci_se_tidy_view(se, assay = "abundance", max_cells = 10), "exceeds max_cells")
  expect_equal(fx$log$total, 0)
  expect_error(env$ci_se_tidy_view(se, assay = "abundance", features = c("g1", "missing")), "key mismatch")
  expect_error(env$ci_se_tidy_view(se, assay = "absent"), "assay must name")
  expect_error(env$ci_se_tidy_view(se, features = c("g1", "g1")), "unique")
  expect_error(env$ci_se_tidy_view(data.frame(x = 1)), "SummarizedExperiment")
  expect_s4_class(SummarizedExperiment::assay(se, "abundance"), "DelayedMatrix")
  expect_s4_class(SummarizedExperiment::assay(se, "counts"), "dgCMatrix")
  after <- env$ci_validate_s4(se)
  expect_true(after$valid)
})

test_that("deliberately lossy SummarizedExperiment conversions are flagged field by field", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("DelayedArray")
  env <- interop_template()
  fx <- se_fixture()
  se <- fx$se
  sample_table <- as.data.frame(SummarizedExperiment::colData(se))
  r <- env$ci_conversion_report(se, sample_table)
  expect_named(r, c("field", "status", "detail"))
  expect_true(all(r$status %in% c("preserved", "transformed", "lost")))
  expect_false(attr(r, "lossless"))
  for (f in c("assay:counts", "assay:abundance", "rowData:symbol", "rowData:ranges", "metadata:protocol", "metadata:label", "feature_keys")) {
    expect_equal(field_status(r, f), "lost")
  }
  expect_equal(field_status(r, "colData:donor"), "preserved")
  expect_equal(field_status(r, "sample_keys"), "preserved")
  view <- env$ci_se_tidy_view(se, assay = "abundance", features = c("g1", "g3"))
  reset_log(fx$log)
  rv <- env$ci_conversion_report(se, view)
  expect_equal(fx$log$total, 16)
  expect_equal(field_status(rv, "assay:abundance"), "transformed")
  expect_equal(field_status(rv, "storage:abundance"), "lost")
  expect_equal(field_status(rv, "assay:counts"), "lost")
  expect_equal(field_status(rv, "rowData:symbol"), "preserved")
  expect_equal(field_status(rv, "rowData:ranges"), "lost")
  expect_equal(field_status(rv, "metadata:protocol"), "lost")
  expect_equal(field_status(rv, "feature_keys"), "transformed")
  expect_equal(field_status(rv, "sample_keys"), "preserved")
  expect_false(attr(rv, "lossless"))
  wide <- as.data.frame(as.matrix(SummarizedExperiment::assay(se, "counts")))
  rw <- env$ci_conversion_report(se, wide)
  expect_equal(field_status(rw, "assay:counts"), "transformed")
  expect_equal(field_status(rw, "storage:counts"), "lost")
  expect_equal(field_status(rw, "assay:abundance"), "lost")
  expect_equal(field_status(rw, "colData:donor"), "lost")
  expect_error(env$ci_conversion_report(wide, se), "from must be")
})

test_that("equal dimensions never make a conversion lossless", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("DelayedArray")
  env <- interop_template()
  se <- se_fixture()$se
  same <- env$ci_conversion_report(se, se)
  expect_true(all(same$status == "preserved"))
  expect_true(attr(same, "lossless"))
  changed <- se
  S4Vectors::metadata(changed) <- list(protocol = "synthetic-v1")
  SummarizedExperiment::rowData(changed)$symbol <- NULL
  counts <- SummarizedExperiment::assay(changed, "counts")
  counts[1, 1] <- counts[1, 1] + 1
  SummarizedExperiment::assay(changed, "counts") <- counts
  r <- env$ci_conversion_report(se, changed)
  expect_equal(field_status(r, "dimensions"), "preserved")
  expect_match(r$detail[r$field == "dimensions"], "not evidence of a lossless conversion")
  expect_false(attr(r, "lossless"))
  expect_equal(field_status(r, "metadata:label"), "lost")
  expect_equal(field_status(r, "metadata:protocol"), "preserved")
  expect_equal(field_status(r, "rowData:symbol"), "lost")
  expect_equal(field_status(r, "assay:counts"), "transformed")
  expect_match(r$detail[r$field == "assay:counts"], "values differ")
  expect_equal(field_status(r, "assay:abundance"), "preserved")
})

test_that("SingleCellExperiment to Seurat conversion reports altExps, reductions and losses", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  env <- interop_template()
  sce <- sce_fixture()
  v <- env$ci_validate_s4(sce)
  expect_true(v$valid)
  expect_contains(v$extends, c("SingleCellExperiment", "SummarizedExperiment"))
  table <- env$ci_conversion_report(sce, as.data.frame(SummarizedExperiment::colData(sce)))
  for (f in c("altExp:ADT", "reducedDim:PCA", "metadata:note", "rowData:symbol", "assay:counts")) expect_equal(field_status(table, f), "lost")
  conv <- suppressWarnings(env$ci_convert(sce, "Seurat"))
  expect_s4_class(conv$object, "Seurat")
  r <- conv$report
  expect_false(attr(r, "lossless"))
  expect_equal(field_status(r, "class"), "transformed")
  expect_equal(field_status(r, "metadata:note"), "lost")
  expect_equal(field_status(r, "sample_keys"), "preserved")
  expect_equal(field_status(r, "colData:donor"), "preserved")
  expect_equal(field_status(r, "altExp:ADT"), "transformed")
  expect_equal(field_status(r, "reducedDim:PCA"), "transformed")
  expect_match(r$detail[r$field == "reducedDim:PCA"], "embedding values identical")
  expect_equal(field_status(r, "assay:logcounts"), "transformed")
  expect_match(r$detail[r$field == "assay:logcounts"], "renamed 'logcounts' -> layer 'data'")
  expect_match(r$detail[r$field == "assay:counts"], "values identical")
  s <- conv$object
  SeuratObject::DefaultAssay(s) <- "RNA"
  back <- suppressWarnings(env$ci_convert(s, "SingleCellExperiment"))
  expect_s4_class(back$object, "SingleCellExperiment")
  round_trip <- env$ci_conversion_report(sce, back$object)
  expect_false(attr(round_trip, "lossless"))
  expect_equal(field_status(round_trip, "metadata:note"), "lost")
  expect_equal(field_status(round_trip, "colData:donor"), "preserved")
  expect_true(env$ci_validate_s4(back$object)$valid)
})

test_that("reductions filed under an alternative experiment are reported where they went", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  env <- interop_template()
  set.seed(3)
  cells <- sprintf("cell%02d", 1:20)
  rna <- Matrix::Matrix(matrix(rpois(120, 2), 6, 20, dimnames = list(paste0("gene", 1:6), cells)), sparse = TRUE)
  adt <- Matrix::Matrix(matrix(rpois(40, 5), 2, 20, dimnames = list(c("ADT1", "ADT2"), cells)), sparse = TRUE)
  s <- SeuratObject::CreateSeuratObject(counts = rna)
  s[["ADT"]] <- SeuratObject::CreateAssay5Object(counts = adt)
  embeddings <- matrix(rnorm(40), 20, 2, dimnames = list(cells, c("PC_1", "PC_2")))
  s[["pca"]] <- SeuratObject::CreateDimReducObject(embeddings = embeddings, key = "PC_", assay = "RNA")
  SeuratObject::DefaultAssay(s) <- "ADT"
  conv <- suppressWarnings(env$ci_convert(s, "SingleCellExperiment"))
  sce <- conv$object
  expect_length(SingleCellExperiment::reducedDimNames(sce), 0L)
  expect_true("PCA" %in% SingleCellExperiment::reducedDimNames(SingleCellExperiment::altExp(sce, "RNA")))
  r <- conv$report
  expect_equal(field_status(r, "reducedDim:pca"), "transformed")
  expect_match(r$detail[r$field == "reducedDim:pca"],
    "embedding values identical \\(computed on assay 'RNA'\\); moved to target altExp 'RNA', not the main experiment")
  expect_false(any(r$status[startsWith(r$field, "reducedDim:")] == "lost"))
  # The SingleCellExperiment side profiles altExp reductions under their location.
  same <- env$ci_conversion_report(sce, sce)
  expect_equal(field_status(same, "reducedDim:RNA/PCA"), "preserved")
  dropped <- sce
  alt <- SingleCellExperiment::altExp(dropped, "RNA")
  SingleCellExperiment::reducedDims(alt) <- list()
  SingleCellExperiment::altExp(dropped, "RNA") <- alt
  lost <- env$ci_conversion_report(sce, dropped)
  expect_equal(field_status(lost, "reducedDim:RNA/PCA"), "lost")
  table <- env$ci_conversion_report(sce, as.data.frame(SummarizedExperiment::colData(sce)))
  expect_equal(field_status(table, "reducedDim:RNA/PCA"), "lost")
})

test_that("altExps without the requested data assay are reported, not fatal", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  env <- interop_template()
  sce <- sce_fixture()
  adt <- SummarizedExperiment::assay(SingleCellExperiment::altExp(sce, "ADT"), "counts")
  SingleCellExperiment::altExp(sce, "ADT") <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = adt))
  conv <- suppressWarnings(env$ci_convert(sce, "Seurat"))
  expect_s4_class(conv$object, "Seurat")
  expect_equal(SeuratObject::Assays(conv$object), "RNA")
  expect_length(conv$limitations, 1L)
  expect_match(conv$limitations, "altExp 'ADT' was not converted: it has no assay 'logcounts'")
  expect_identical(attr(conv$report, "limitations"), conv$limitations)
  expect_equal(field_status(conv$report, "altExp:ADT"), "lost")
  expect_match(conv$report$detail[conv$report$field == "altExp:ADT"], "no assay 'logcounts'")
  expect_false(attr(conv$report, "lossless"))
  expect_equal(field_status(conv$report, "assay:logcounts"), "transformed")
  counts_only <- suppressWarnings(env$ci_convert(sce, "Seurat", data = NULL))
  expect_equal(sort(SeuratObject::Assays(counts_only$object)), c("ADT", "RNA"))
  expect_length(counts_only$limitations, 0L)
  no_log <- sce
  SummarizedExperiment::assay(no_log, "logcounts") <- NULL
  expect_error(env$ci_convert(no_log, "Seurat"), "main experiment has no assay 'logcounts'")
  expect_error(env$ci_convert(sce, "Seurat", counts = NULL, data = NULL), "cannot both be NULL")
  expect_error(env$ci_convert(sce, "Seurat", pins = list(Seurat = "0.0.1")), "'Seurat' .* pins 0.0.1")
})

test_that("observed Seurat 5.5.1 conversion behavior stays recorded in the registry", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  skip_if_not(utils::packageVersion("Seurat") == "5.5.1" && utils::packageVersion("SeuratObject") == "5.4.0")
  env <- interop_template()
  sce <- sce_fixture()
  conv <- suppressWarnings(env$ci_convert(sce, "Seurat"))
  r <- conv$report
  expect_equal(field_status(r, "feature_keys"), "transformed")
  expect_match(r$detail[r$field == "feature_keys"], "'gene_1' -> 'gene-1'")
  expect_equal(field_status(r, "main_experiment"), "transformed")
  expect_equal(SeuratObject::DefaultAssay(conv$object), "ADT")
  expect_equal(field_status(r, "rowData:symbol"), "preserved")
  layers <- env$ci_seurat_layers(conv$object)
  expect_true(all(layers$assay_class == "Assay"))
  expect_true(all(grepl("not Assay5", layers$issue)))
  s <- conv$object
  SeuratObject::DefaultAssay(s) <- "RNA"
  back <- suppressWarnings(env$ci_convert(s, "SingleCellExperiment"))$report
  expect_equal(field_status(back, "rowData:symbol"), "lost")
  expect_equal(field_status(back, "reducedDim:PCA"), "preserved")
  expect_equal(field_status(back, "altExp:ADT"), "transformed")
  expect_match(back$detail[back$field == "altExp:ADT"], "kept as target altExp 'ADT'.*features: preserved.*assay:counts preserved.*assay:data transformed")
  # With the default assay left at ADT, Seurat files PCA under altExp 'RNA'.
  moved <- suppressWarnings(env$ci_convert(conv$object, "SingleCellExperiment"))
  expect_equal(SingleCellExperiment::reducedDimNames(SingleCellExperiment::altExp(moved$object, "RNA")), "PCA")
  expect_equal(field_status(moved$report, "reducedDim:PCA"), "transformed")
  expect_match(moved$report$detail[moved$report$field == "reducedDim:PCA"], "values identical.*moved to target altExp 'RNA'")
  caps <- interop_capabilities()$capabilities
  conversion <- Filter(function(x) x$id == "seurat.interop.sce_conversion", caps)[[1L]]
  expect_true(any(grepl("Seurat 5.5.1", unlist(conversion$requirements), fixed = TRUE)))
})

test_that("Seurat v5 Assay5 layers are inventoried with counts and data semantics", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Matrix")
  env <- interop_template()
  set.seed(5)
  cnt <- Matrix::Matrix(matrix(rpois(30 * 60, 1.5), 30, 60, dimnames = list(paste0("gene", 1:30), paste0("c", 1:60))), sparse = TRUE)
  s <- SeuratObject::CreateSeuratObject(counts = cnt)
  l0 <- env$ci_seurat_layers(s)
  expect_equal(l0$layer, "counts")
  expect_equal(l0$assay_class, "Assay5")
  expect_equal(l0$semantics, "raw_counts")
  expect_equal(l0$storage, "sparse")
  expect_equal(l0$issue, "")
  s <- Seurat::NormalizeData(s, verbose = FALSE)
  l1 <- env$ci_seurat_layers(s)
  expect_equal(l1$layer, c("counts", "data"))
  expect_equal(l1$semantics, c("raw_counts", "normalized"))
  expect_equal(l1$layer_class, c("dgCMatrix", "dgCMatrix"))
  expect_equal(l1$integer_valued, c(TRUE, FALSE))
  expect_equal(l1$n_cells, c(60L, 60L))
  expect_true(all(l1$default_assay))
  expect_identical(SeuratObject::LayerData(s, layer = "counts"), cnt)
  copy <- SeuratObject::CreateSeuratObject(counts = SeuratObject::CreateAssay5Object(counts = cnt, data = cnt))
  lc <- env$ci_seurat_layers(copy)
  expect_equal(lc$semantics, c("raw_counts", "unnormalized_copy"))
  expect_match(lc$issue[[2L]], "data layer equals counts")
  scaled <- SeuratObject::CreateSeuratObject(counts = cnt * 0.5)
  ls <- env$ci_seurat_layers(scaled)
  expect_equal(ls$semantics, "non_count_values")
  expect_match(ls$issue, "sctransform")
  split_obj <- s
  split_obj$donor <- rep(c("d1", "d2"), 30)
  split_obj[["RNA"]] <- split(split_obj[["RNA"]], f = split_obj$donor)
  lsp <- env$ci_seurat_layers(split_obj)
  expect_true(all(lsp$split))
  expect_true(all(grepl("split layer", lsp$issue)))
  expect_error(env$ci_pseudobulk(split_obj, "donor", "orig.ident"), "JoinLayers")
  conv <- suppressWarnings(env$ci_convert(s, "SingleCellExperiment"))
  r <- conv$report
  expect_equal(field_status(r, "assay:counts"), "preserved")
  expect_equal(field_status(r, "assay:data"), "transformed")
  expect_match(r$detail[r$field == "assay:data"], "'data' -> assay 'logcounts'")
  expect_equal(field_status(r, "sample_keys"), "preserved")
  expect_false(attr(r, "lossless"))
  expect_true(env$ci_validate_s4(s)$valid)
  expect_error(env$ci_seurat_layers(cnt), "Seurat object")
})

test_that("donor-level pseudobulk sums raw counts and refuses too few donors", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("SeuratObject")
  env <- interop_template()
  set.seed(42)
  donor <- rep(paste0("d", 1:6), each = 6)
  group <- ifelse(donor %in% c("d1", "d2", "d3"), "ctrl", "treated")
  shuffle <- sample(36)
  donor <- donor[shuffle]
  group <- group[shuffle]
  cells <- paste0("cell", 1:36)
  m <- matrix(rpois(4 * 36, 3), 4, 36, dimnames = list(paste0("gene", 1:4), cells))
  expected <- t(rowsum(t(m), paste(group, donor, sep = "|")))
  storage.mode(expected) <- "double"
  sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = Matrix::Matrix(m, sparse = TRUE)),
    colData = S4Vectors::DataFrame(donor = donor, group = group, row.names = cells))
  pb <- env$ci_pseudobulk(sce, "donor", "group")
  expect_equal(colnames(pb$counts), c(paste0("ctrl|d", 1:3), paste0("treated|d", 4:6)))
  expect_identical(pb$counts, expected[, colnames(pb$counts)])
  expect_equal(pb$samples$n_cells, rep(6L, 6))
  expect_equal(pb$samples$pseudobulk_id, colnames(pb$counts))
  expect_equal(unname(pb$design$donors_per_group), c(3L, 3L))
  expect_equal(sum(pb$counts), sum(m))
  delayed <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = DelayedArray::DelayedArray(m)),
    colData = S4Vectors::DataFrame(donor = donor, group = group, row.names = cells))
  expect_identical(env$ci_pseudobulk(delayed, "donor", "group")$counts, pb$counts)
  seu <- SeuratObject::CreateSeuratObject(counts = Matrix::Matrix(m, sparse = TRUE),
    meta.data = data.frame(donor = donor, group = group, row.names = cells))
  expect_identical(env$ci_pseudobulk(seu, "donor", "group")$counts, pb$counts)
  one_donor <- sce
  SummarizedExperiment::colData(one_donor)$donor[group == "treated"] <- "d4"
  expect_error(env$ci_pseudobulk(one_donor, "donor", "group"), "at least 2 donors per group.*treated \\(1\\)")
  unlabeled <- sce
  SummarizedExperiment::colData(unlabeled)$donor[1] <- NA
  expect_error(env$ci_pseudobulk(unlabeled, "donor", "group"), "must not be missing")
  expect_error(env$ci_pseudobulk(sce, "patient", "group"), "not present")
  expect_error(env$ci_pseudobulk(sce, "donor", "donor"), "different columns")
  normalized <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = Matrix::Matrix(log1p(m), sparse = TRUE)),
    colData = S4Vectors::DataFrame(donor = donor, group = group, row.names = cells))
  expect_error(env$ci_pseudobulk(normalized, "donor", "group"), "nonnegative integers")
  no_counts <- SingleCellExperiment::SingleCellExperiment(assays = list(logcounts = Matrix::Matrix(log1p(m), sparse = TRUE)),
    colData = S4Vectors::DataFrame(donor = donor, group = group, row.names = cells))
  expect_error(env$ci_pseudobulk(no_counts, "donor", "group"), "'counts' assay")
  expect_equal(pb$design$single_cell_samples, 0L)
  expect_false(pb$design$allow_single_cell_samples)

  # Cells are not replicates: a per-cell identifier is never a donor column.
  SummarizedExperiment::colData(sce)$cell_id <- cells
  expect_error(env$ci_pseudobulk(sce, "cell_id", "group"), "'cell_id' has a different value for each of the 36 cells")
  expect_error(env$ci_pseudobulk(sce, "cell_id", "group", allow_single_cell_samples = TRUE), "identifies cells")
  seu$cell_id <- cells
  expect_error(env$ci_pseudobulk(seu, "cell_id", "group"), "identifies cells")
  # One donor contributing a single cell is refused unless explicitly allowed.
  lone <- sce
  SummarizedExperiment::colData(lone)$donor[1] <- "d_lone"
  lone_id <- paste(group[1], "d_lone", sep = "|")
  expect_error(env$ci_pseudobulk(lone, "donor", "group"), paste0("1 pseudobulk sample\\(s\\) contain a single cell \\(", lone_id, "\\)"))
  allowed <- env$ci_pseudobulk(lone, "donor", "group", allow_single_cell_samples = TRUE)
  expect_equal(allowed$samples$n_cells[allowed$samples$pseudobulk_id == lone_id], 1L)
  expect_equal(allowed$design$single_cell_samples, 1L)
  expect_true(allowed$design$allow_single_cell_samples)
  expect_equal(sum(allowed$counts), sum(m))
  expect_error(env$ci_pseudobulk(lone, "donor", "group", allow_single_cell_samples = NA), "TRUE or FALSE")
})

test_that("a mixed-model S4 fit maps to broom output matching its fixed effects", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("broom.mixed")
  env <- interop_template()
  set.seed(20261002)
  subject <- factor(rep(sprintf("subject%02d", 1:12), each = 6))
  x <- rep(seq(0, 1, length.out = 6), 12)
  y <- 1.5 + 2 * x + rnorm(12, sd = 0.8)[as.integer(subject)] + rnorm(72, sd = 0.3)
  fit <- lme4::lmer(y ~ x + (1 | subject), data = data.frame(y, x, subject), REML = TRUE)
  facts <- env$ci_validate_s4(fit)
  expect_true(isS4(fit))
  expect_equal(facts$class, "lmerMod")
  expect_equal(facts$package, "lme4")
  expect_true(facts$valid)
  expect_contains(facts$extends, "merMod")
  tidy <- broom.mixed::tidy(fit, effects = "fixed")
  expect_s3_class(tidy, "data.frame")
  expect_equal(tidy$term, names(lme4::fixef(fit)))
  expect_equal(tidy$estimate, unname(lme4::fixef(fit)), tolerance = 1e-10)
  expect_equal(tidy$std.error, unname(sqrt(diag(as.matrix(stats::vcov(fit))))), tolerance = 1e-10)
  expect_true(all(tidy$effect == "fixed"))
  ran <- broom.mixed::tidy(fit, effects = "ran_pars")
  expect_equal(ran$estimate, as.data.frame(lme4::VarCorr(fit))$sdcor, tolerance = 1e-10)
  plain <- env$ci_validate_s4(data.frame(a = 1))
  expect_false(plain$is_s4)
  expect_true(is.na(plain$valid))
})

test_that("the bioc/seurat capability registry is valid and truthful about testing", {
  reg <- interop_capabilities()
  caps <- reg$capabilities
  ids <- vapply(caps, function(x) x$id, character(1))
  expect_equal(reg$schema_version, 1L)
  expect_equal(reg$family, "bioc_seurat")
  expect_false(anyDuplicated(ids) > 0)
  expect_contains(ids, c("bioc.container.summarized_experiment", "bioc.container.single_cell_experiment",
      "bioc.container.spatial_experiment", "bioc.container.multi_assay_experiment", "bioc.container.qfeatures",
      "bioc.se.tidy_view", "seurat.single_cell.exploration", "seurat.normalization.sctransform", "seurat.chromatin.signac",
      "seurat.annotation.azimuth", "seurat.wrappers.external_methods", "seurat.interchange.seuratdisk",
      "seurat.acceleration.bpcells", "seurat.markers.presto", "seurat.count_models.glmgampoi"))
  status <- stats::setNames(vapply(caps, function(x) x$status, character(1)), ids)
  expect_equal(status[["bioc.se.tidy_view"]], "adapter_tested")
  expect_equal(status[["seurat.single_cell.exploration"]], "candidate")
  tested_packages <- c("SummarizedExperiment", "S4Vectors", "SingleCellExperiment", "Matrix", "Seurat", "SeuratObject", "methods")
  adapters <- interop_adapters()
  env <- interop_template()
  for (cap in caps) {
    modality <- unlist(cap$applies$modality)
    expect_false("tabular" %in% modality)
    if (cap$status == "adapter_tested") {
      expect_true(all(unlist(cap$packages) %in% tested_packages), label = cap$id)
      row <- adapters[adapters$id == cap$adapter$id, ]
      expect_equal(nrow(row), 1L)
      for (f in strsplit(row$functions, ",", fixed = TRUE)[[1L]]) expect_true(is.function(env[[f]]), label = f)
    } else {
      expect_null(cap$adapter)
    }
    if (isTRUE(cap$infrastructure)) expect_false(isTRUE(cap$specialist))
  }
  untested <- c("tidySummarizedExperiment", "tidySingleCellExperiment", "BPCells", "presto", "glmGamPoi", "Signac", "SeuratDisk",
    "SeuratData", "Azimuth", "SeuratWrappers", "MultiAssayExperiment", "QFeatures", "TreeSummarizedExperiment", "SpatialExperiment", "sctransform")
  for (cap in caps) if (any(unlist(cap$packages) %in% untested)) expect_equal(cap$status, "candidate", label = cap$id)
  expect_length(interop_capabilities_for("tabular"), 0L)
  expect_length(interop_capabilities_for("unknown"), 0L)
  single <- vapply(interop_capabilities_for("single_cell"), function(x) x$id, character(1))
  expect_contains(single, c("seurat.single_cell.exploration", "bioc.single_cell.donor_pseudobulk"))
  bulk <- vapply(interop_capabilities_for("bulk_rna"), function(x) x$id, character(1))
  expect_false(any(c("seurat.single_cell.exploration", "seurat.normalization.sctransform") %in% bulk))
  expect_error(interop_capabilities_for("clinical"), class = "cttir_input_error")
  rules <- modality_routing_rules()
  expect_equal(rules$effect[rules$rule == "tabular_never_seurat"], "exclude")
  expect_true(all(c("donor_level_inference", "no_dataset_download", "infrastructure_not_specialist") %in% rules$rule))
})

test_that("invalid capability registries are rejected", {
  base <- jsonlite::fromJSON(system.file("extdata", "capabilities", "bioc.json", package = "cttiR"), simplifyVector = FALSE)
  write_registry <- function(x) {
    path <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
    writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", pretty = TRUE), path)
    path
  }
  expect_silent(interop_capabilities(write_registry(base)))
  seurat <- which(vapply(base$capabilities, function(x) x$id, character(1)) == "seurat.single_cell.exploration")
  candidate <- which(vapply(base$capabilities, function(x) x$status, character(1)) == "candidate")[[1L]]
  mutations <- list(
    duplicate = function(x) {
      x$capabilities[[2L]]$id <- x$capabilities[[1L]]$id
      x
    },
    bad_status = function(x) {
      x$capabilities[[1L]]$status <- "approved"
      x
    },
    unknown_key = function(x) {
      x$capabilities[[1L]]$extra <- TRUE
      x
    },
    unknown_top_key = function(x) {
      x$extra <- 1
      x
    },
    missing_key = function(x) {
      x$capabilities[[1L]]$requirements <- NULL
      x
    },
    empty_packages = function(x) {
      x$capabilities[[1L]]$packages <- list()
      x
    },
    seurat_tabular = function(x) {
      x$capabilities[[seurat]]$applies$modality <- list("single_cell", "tabular")
      x
    },
    clinical_only = function(x) {
      x$capabilities[[candidate]]$applies$modality <- list("tabular")
      x
    },
    unknown_modality = function(x) {
      x$capabilities[[1L]]$applies$modality <- list("clinical")
      x
    },
    candidate_with_adapter = function(x) {
      x$capabilities[[candidate]]$adapter <- list(id = "interop.bioc_s4", version = "1.0.0")
      x
    },
    tested_without_adapter = function(x) {
      x$capabilities[[seurat]]$status <- "adapter_tested"
      x$capabilities[[seurat]]$adapter <- NULL
      x
    },
    unreviewed_adapter = function(x) {
      x$capabilities[[seurat]]$status <- "adapter_tested"
      x$capabilities[[seurat]]$adapter <- list(id = "interop.seurat_v5", version = "1.0.0")
      x$capabilities[[seurat]]$adapter$version <- "9.9.9"
      x
    },
    wrong_family_prefix = function(x) {
      x$capabilities[[seurat]]$family <- "bioconductor"
      x
    },
    infrastructure_specialist = function(x) {
      x$capabilities[[1L]]$specialist <- TRUE
      x
    },
    missing_german_keywords = function(x) {
      x$capabilities[[1L]]$keywords$de <- list()
      x
    }
  )
  for (name in names(mutations)) {
    x <- mutations[[name]](base)
    if (name == "tested_without_adapter") x$capabilities[[seurat]]["adapter"] <- list(NULL)
    expect_error(interop_capabilities(write_registry(x)), class = "cttir_schema_error", label = name)
  }
})
