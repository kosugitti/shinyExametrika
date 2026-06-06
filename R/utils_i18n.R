# =============================================================================
# utils_i18n.R -- i18n helpers
# =============================================================================
#
# The app enables shiny.i18n's automatic client-side translation by calling
# `i18n$use_js()` before building the UI (see app_ui.R). In that mode
# `i18n$t("X")` no longer returns a plain string -- it returns a
# `<span class="i18n" data-key="X">` tag that the bundled JavaScript swaps live
# when the language toggle changes.
#
# That is exactly what we want for visible text, but it breaks HTML *attribute*
# contexts (e.g. an input `placeholder=`), where a tag object cannot be used.
# `t_plain()` returns the plain translated string for those cases.
# =============================================================================

#' Plain (unwrapped) translation, safe for HTML attribute contexts
#'
#' Unlike `i18n$t()` in automatic (use_js) mode, this always returns a bare
#' character string, so it can be used for `placeholder`, `title` attributes,
#' etc. The trade-off is that such text is rendered once at build time and does
#' not live-swap on language change (attributes cannot be auto-translated by
#' shiny.i18n); it is still correct for the language active at render time.
#'
#' @param i18n shiny.i18n Translator object.
#' @param key English source string (the translation key).
#'
#' @return A character scalar: the translation for the active language, or the
#'   key itself when no translation is found / the active language is the
#'   source language.
#' @noRd
t_plain <- function(i18n, key) {
  lang <- i18n$get_translation_language()
  if (identical(lang, i18n$get_key_translation())) {
    return(key)
  }

  tr <- i18n$get_translations()
  if (key %in% rownames(tr) && lang %in% colnames(tr)) {
    val <- tr[key, lang]
    if (!is.na(val) && nzchar(val)) {
      return(val)
    }
  }
  key
}
