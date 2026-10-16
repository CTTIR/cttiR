# Translate only known presentation messages. API results, code, identifiers and
# literal source excerpts remain unchanged.
app_answer_text <- function(text, lang) {
  if (!identical(app_lang(lang), "de")) return(text)
  messages <- app_messages()
  keys <- grep("^ask.content[.]", names(messages$en), value = TRUE)
  english <- unlist(messages$en[keys], use.names = FALSE)
  vapply(text, function(value) {
    index <- match(value, english)
    if (!is.na(index)) return(messages$de[[keys[[index]]]])
    prefix <- "Supported with approved adapters: "
    if (startsWith(value, prefix)) return(paste0("Unterst\u00fctzt durch freigegebene Adapter: ", substring(value, nchar(prefix) + 1L)))
    if (identical(value, "The requested model or design has no compatible approved adapter. No code is returned and no model is substituted; see gaps.")) {
      return("F\u00fcr das angefragte Modell oder Studiendesign gibt es keinen passenden freigegebenen Adapter. Es wird kein Code ausgegeben und kein Ersatzmodell vorgeschlagen; siehe L\u00fccken.")
    }
    partial <- regmatches(value, regexec(paste0("^Partially supported with approved adapters: ([a-z0-9_. ,]+)\\. ",
      "No approved adapter covers: ([a-z0-9_. ,]+); see gaps\\. The supported steps do not complete the whole request\\.$"), value, perl = TRUE))[[1L]]
    if (length(partial)) return(paste0("Teilweise unterst\u00fctzt durch freigegebene Adapter: ", partial[[2L]],
      ". Kein freigegebener Adapter deckt Folgendes ab: ", partial[[3L]],
      "; siehe L\u00fccken. Die unterst\u00fctzten Schritte decken die Gesamtanfrage nicht ab."))
    mismatch <- regmatches(value, regexec("^(model_configuration_mismatch:[a-z0-9_.]+ \\([^\\r\\n()]+\\)): (.+)$", value, perl = TRUE))[[1L]]
    if (length(mismatch)) {
      descriptions <- c(
        "the requested design is outside this adapter's reviewed applicability; no model is substituted" =
          "das angefragte Studiendesign liegt au\u00dferhalb des gepr\u00fcften Anwendungsbereichs dieses Adapters; es wird kein Ersatzmodell vorgeschlagen",
        "no reviewed model adapter covers this combination; no model is substituted" =
          "kein gepr\u00fcfter Modelladapter deckt diese Kombination ab; es wird kein Ersatzmodell vorgeschlagen")
      translated <- unname(descriptions[mismatch[[3L]]])
      if (!is.na(translated)) return(paste0(mismatch[[2L]], ": ", translated))
    }
    value
  }, "", USE.NAMES = FALSE)
}
