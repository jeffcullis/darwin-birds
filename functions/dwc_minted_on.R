# dwc_minted_on.R
#
# Add the date each Darwin Core term was first minted to the Quick Reference
# terms.
#
# term_versions.csv (https://github.com/tdwg/dwc/blob/master/vocabulary/term_versions.csv)
# has one row per version of each term, keyed to the term by `term_iri`, with
# the version's publication date in `issued`. A term's minted date is the
# earliest `issued` date among all its versions. Terms are matched on
# tdwg_terms$identifier == term_versions_df$term_iri.
#
# Usage:
#   source(rprojroot::find_root_file("functions", "dwc_minted_on.R",
#     criterion = rprojroot::is_rstudio_project))
#   tdwg_terms <- add_dwc_minted_on(tdwg_terms, term_versions_df)

# Add a `minted_on` Date column to `tdwg_terms` (needs an `identifier`
# column), NA for terms with no versions in `term_versions_df` (needs
# `term_iri` and `issued` columns).
add_dwc_minted_on <- function(tdwg_terms, term_versions_df) {
  issued <- as.Date(term_versions_df$issued)
  minted <- tapply(issued, term_versions_df$term_iri, min, na.rm = TRUE)

  tdwg_terms$minted_on <- as.Date(unname(minted[tdwg_terms$identifier]))

  n_missing <- sum(is.na(tdwg_terms$minted_on))
  if (n_missing > 0) {
    message(n_missing, " term(s) not found in term_versions_df: ",
            paste(tdwg_terms$qualified_name[is.na(tdwg_terms$minted_on)],
                  collapse = ", "))
  }

  tdwg_terms
}
