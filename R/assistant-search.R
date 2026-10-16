# Bounded lexical candidates, optionally across the pinned catalog. Selection is
# retrieval evidence, never approval or a claim that the API answers the task.
assistant_reference_candidates <- function(question, catalog, limit = 4L, allow_unnamed = FALSE) {
  words <- function(text) {
    if (!length(text)) return(character())
    tokens <- unique(regmatches(tolower(text), gregexpr("[\\p{L}\\p{N}]+", tolower(text), perl = TRUE))[[1]])
    tokens[nchar(tokens) >= 3L]
  }
  terms <- words(question)
  stopwords <- c("the", "and", "for", "with", "which", "what", "how", "does", "can", "you", "please",
    "explain", "describe", "package", "function", "functions", "use", "using", "from", "that", "this",
    "der", "die", "das", "und", "f\u00fcr", "mit", "wie", "welche", "welcher", "was", "bitte",
    "erkl\u00e4re", "beschreibe", "paket", "funktion", "funktionen", "eine", "einen", "ist", "sind")
  terms <- setdiff(terms, stopwords)
  # Small literal terminology bridge; never translate source documentation or
  # manufacture API names. Compound terms expand into independent search words.
  bridge <- list(schwellenwert = "threshold", schwellenwerte = "threshold",
    schwellenwertmethoden = c("threshold", "methods"), methoden = "methods",
    argumente = "arguments", argumenten = "arguments", zustandsmarker = c("state", "markers"),
    zustandsmarkern = c("state", "markers"), spalten = "columns", zeilen = "rows",
    mittelwert = "mean", summen = "sums", normalisierung = "normalization")
  terms <- unique(c(terms, unlist(bridge[intersect(terms, names(bridge))], use.names = FALSE)))
  candidates <- list()
  for (package in catalog$packages) {
    explicit <- tolower(package$name) %in% terms
    if (!explicit && !allow_unnamed) next
    query <- setdiff(terms, tolower(package$name))
    if (!length(query)) next
    if (!length(package$documentation_corpus$documents)) next
    docs <- stats::setNames(package$documentation_corpus$documents,
      vapply(package$documentation_corpus$documents, function(d) d$path, ""))
    for (entry in package$exports) {
      if (!identical(entry$verification, "static_api_verified")) next
      doc <- docs[[entry$documentation$path %||% ""]]
      if (is.null(doc) || !identical(doc$storage, "source_text")) next
      title <- regmatches(doc$content, regexpr("\\\\title\\{[^{}]*\\}", doc$content, perl = TRUE))
      if (!explicit && sum(query %in% unique(c(words(entry$name), words(title)))) < 2L) next
      score <- 4L * sum(query %in% words(entry$name)) + 2L * sum(query %in% words(title))
      if (score <= 0L) next
      candidates[[length(candidates) + 1L]] <- list(symbol = paste0(package$name, "::", entry$name), score = score)
    }
  }
  if (!length(candidates)) return(character())
  symbols <- vapply(candidates, function(x) x$symbol, "")
  scores <- vapply(candidates, function(x) x$score, integer(1))
  utils::head(symbols[order(-scores, symbols, method = "radix")], limit)
}
