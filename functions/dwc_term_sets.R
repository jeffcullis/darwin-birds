# dwc_term_sets.R
#
# Find out which Darwin Core distribution term lists ("sets") each term in the
# Quick Reference terms belongs to.
#
# The set files (*_vertical.csv, one term per line, no header) must already be
# downloaded; run scripts/data_processing/download_dwc_term_sets.R first. Each
# set is named by its file stem, e.g. "simple_dwc" for simple_dwc_vertical.csv.
#
# Matching:
#   - Namespace sets (ac, chrono, chronoiri, dc, dcterms, dwc, dwciri, eco,
#     ecoiri) list local names only, so a term matches when its
#     qualified_name is "<set>:<term_name>". Without this, a name like
#     "behavior" would match both dwc:behavior and dwciri:behavior.
#   - simple_dwc mixes namespaces and has no IRI terms, so it is matched on
#     term_name among dc:, dcterms: and dwc: terms. A name that exists in
#     more than one of those (e.g. "language" as both dc:language and
#     dcterms:language) is counted as in simple_dwc for each.
#
# Usage:
#   source(rprojroot::find_root_file("functions", "dwc_term_sets.R",
#     criterion = rprojroot::is_rstudio_project))
#   terms <- add_dwc_term_sets(tdwg_terms)

# Read the *_vertical.csv files in `sets_dir` into a named list of character
# vectors, one per set.
read_dwc_term_sets <- function(sets_dir) {
  paths <- list.files(sets_dir, pattern = "_vertical\\.csv$", full.names = TRUE)
  if (length(paths) == 0) {
    stop("No *_vertical.csv files in ", sets_dir, "; run ",
         "scripts/data_processing/download_dwc_term_sets.R first.")
  }
  sets <- lapply(paths, function(p) {
    x <- trimws(readLines(p, warn = FALSE, encoding = "UTF-8"))
    x[x != ""]
  })
  names(sets) <- sub("_vertical\\.csv$", "", basename(paths))
  sets
}

# Add a `sets` column to the Quick Reference terms data frame (needs
# `term_name` and `qualified_name` columns): a comma-separated list of the
# sets each term is in ("" if none).
add_dwc_term_sets <- function(
    terms,
    sets_dir = rprojroot::find_root_file(
      "data", "external", "tdwg", criterion = rprojroot::is_rstudio_project)) {
  sets <- read_dwc_term_sets(sets_dir)
  namespace <- sub(":.*$", "", terms$qualified_name)

  # Logical matrix: one row per term, one column per set.
  in_set <- vapply(names(sets), function(set) {
    if (set == "simple_dwc") {
      namespace %in% c("dc", "dcterms", "dwc") & terms$term_name %in% sets[[set]]
    } else {
      terms$qualified_name %in% paste0(set, ":", sets[[set]])
    }
  }, logical(nrow(terms)))
  in_set <- matrix(in_set, nrow = nrow(terms), dimnames = list(NULL, names(sets)))

  terms$sets <- apply(in_set, 1, function(row) {
    paste(names(sets)[row], collapse = ", ")
  })

  # Report set entries that matched no Quick Reference term.
  for (set in names(sets)) {
    missing <- setdiff(sets[[set]], terms$term_name[in_set[, set]])
    if (length(missing) > 0) {
      message(set, ": ", length(missing), " term(s) not in Quick Reference: ",
              paste(missing, collapse = ", "))
    }
  }

  terms
}
