test_that("literal author roles preserve names, contacts and declared roles", {
  literal <- paste0('c(person("Ada", "Example", email="ada@example.org", role=c("aut", "cre"), ',
    'comment=c(ORCID="0000-0000-0000-0000")), utils::person(given="An Institute", role="cph"))')
  roles <- description_roles(literal)
  expect_identical(roles$status, "literal_roles_parsed")
  expect_true(description_has_maintainer(roles))
  expect_identical(roles$people[[1]]$family, "Example")
  expect_identical(roles$people[[1]]$roles, c("aut", "cre"))
  expect_identical(roles$people[[2]]$roles, "cph")
  expect_identical(roles$ownership, "not_inferred")
  expect_false(description_has_maintainer(description_roles('person("A", role="aut")')))
  expect_false(description_has_maintainer(description_roles('person("A", role="cre", email="invalid")')))
})

test_that("unsupported or executable author declarations stay unresolved", {
  for (literal in c('stop("executed")', 'person(Sys.getenv("USER"), role="cre")',
      'person("A", role="cre", email=paste0("a", "@b.org"))',
      'person("A", role="cre", comment=system("false"))',
      'person("A", rol="cre")', 'person("A", role="aut", role="cre")',
      'person("A"); person("B")', 'c()', 'person("A", role=NA)', 'person(', NA_character_)) {
    roles <- description_roles(literal)
    expect_identical(roles$status, "unresolved", info = literal)
    expect_false(description_has_maintainer(roles))
  }
  sentinel <- tempfile()
  roles <- description_roles(paste0('person("A", comment=file.create("', sentinel, '"))'))
  expect_false(file.exists(sentinel))
  expect_identical(roles$status, "unresolved")
})

test_that("literal cre evidence remains bound to the source revision", {
  root <- fixture_source(file.path(new_parent(), "source"), "fit <- function(x) x", "fit")
  file <- file.path(root, "DESCRIPTION")
  lines <- readLines(file)
  lines <- lines[!grepl("^(Author|Maintainer|Authors@R):", lines)]
  writeLines(c(lines, 'Authors@R: person("Ada", "Example", role=c("aut", "cre"), email="ada@example.org")'), file)
  record <- extract_source(root, "https://example.org/source", "fixture")
  expect_identical(record$maintainer_evidence$maintainer, "")
  expect_true(companion_provenance_valid(record))
  expect_true(description_has_maintainer(record$maintainer_evidence$authors_r_roles))
  record$maintainer_evidence$authors_r_literal <- 'person("A", role="aut")'
  # Do not trust a cached roles object after literal text changes.
  expect_false(companion_provenance_valid(record))
  record$maintainer_evidence$authors_r_literal <- 'person("A", role="cre", email="a@example.org")'
  record$maintainer_evidence$description_sha256 <- "stale"
  expect_false(companion_provenance_valid(record))
})

test_that("bundled companion roles are available without inventing explicit maintainers", {
  for (name in c("BPCells", "SeuratDisk", "presto")) {
    rows <- resources(name, limit = 1L)
    expect_identical(rows$name, name)
    expect_true(is.na(rows$maintainer))
    expect_true(description_has_maintainer(rows$authors_r_roles[[1]]))
  }
})
