# dwc_term_sets.R
#
# Find out which Darwin Core distribution term lists ("sets") each term in the
# Quick Reference CSV belongs to.
#
# The tdwg/dwc repo publishes one-term-per-line, headerless lists in dist/,
# e.g. dist/dwc_vertical.csv, dist/chrono_vertical.csv,
# dist/simple_dwc_vertical.csv. These functions download them to
# data/external/tdwg/ (skipping any that are already there), read them in, and add
# a logical column per set to the Quick Reference terms.
#
# Matching:
#   - Namespace sets (ac, chrono, chronoiri, dc, dcterms, dwc, dwciri, eco,
#     ecoiri) list local names only, so a term matches when its
#     qualified_name is "<set>:<term_name>". Without this, a name like
#     "behavior" would match both dwc:behavior and dwciri:behavior.
#   - simple_dwc mixes namespaces and has no IRI terms, so it is matched on
#     term_name among dc:, dcterms: and dwc: terms. A name that exists in
#     more than one of those (e.g. "language" as both dc:language and
#     dcterms:language) is flagged TRUE for each.
#
# Usage:
#   source(here::here("functions", "dwc_term_sets.R"))
#   terms <- add_dwc_term_sets()

dwc_dist_api <- "https://api.github.com/repos/tdwg/dwc/contents/dist"
dwc_dist_raw <- "https://raw.githubusercontent.com/tdwg/dwc/master/dist"

# Names of the *_vertical.csv files directly in dist/ (not in subdirectories).
list_dwc_vertical_files <- function(api_url = dwc_dist_api) {
  listing <- jsonlite::fromJSON(api_url)
  files <- listing$name[listing$type == "file"]
  sort(grep("_vertical\\.csv$", files, value = TRUE))
}

# Download any *_vertical.csv files not already in `dest_dir`. Returns the
# local paths of all of them.
download_dwc_vertical_files <- function(
    dest_dir = here::here("data", "external", "tdwg"),
    files = list_dwc_vertical_files()) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dest_dir, files)
  for (i in seq_along(files)) {
    if (!file.exists(paths[i])) {
      curl::curl_download(file.path(dwc_dist_raw, files[i]), paths[i])
      message("Downloaded ", files[i])
    }
  }
  paths
}

# Read the vertical files into a named list of character vectors, one per
# set, named by the file stem (e.g. "simple_dwc").
read_dwc_vertical_files <- function(paths) {
  sets <- lapply(paths, function(p) {
    x <- trimws(readLines(p, warn = FALSE, encoding = "UTF-8"))
    x[x != ""]
  })
  names(sets) <- sub("_vertical\\.csv$", "", basename(paths))
  sets
}

# Load the Quick Reference terms and add one logical column per set
# (set_ac, set_chrono, ..., set_simple_dwc), plus `sets`, a comma-separated
# summary of the sets each term is in.
add_dwc_term_sets <- function(
    quick_ref_file = here::here("data", "external", "tdwg",
                                "dwc_quick_reference_terms.csv"),
    dest_dir = here::here("data", "external", "tdwg")) {
  terms <- readr::read_csv(quick_ref_file, show_col_types = FALSE)
  sets <- read_dwc_vertical_files(download_dwc_vertical_files(dest_dir))

  namespace <- sub(":.*$", "", terms$qualified_name)

  for (set in names(sets)) {
    in_set <- if (set == "simple_dwc") {
      namespace %in% c("dc", "dcterms", "dwc") & terms$term_name %in% sets[[set]]
    } else {
      terms$qualified_name %in% paste0(set, ":", sets[[set]])
    }
    terms[[paste0("set_", set)]] <- in_set
  }

  set_cols <- paste0("set_", names(sets))
  terms$sets <- apply(as.matrix(terms[set_cols]), 1, function(row) {
    paste(names(sets)[row], collapse = ", ")
  })

  # Report set entries that matched no Quick Reference term.
  for (set in names(sets)) {
    matched <- terms$term_name[terms[[paste0("set_", set)]]]
    missing <- setdiff(sets[[set]], matched)
    if (length(missing) > 0) {
      message(set, ": ", length(missing), " term(s) not in Quick Reference: ",
              paste(missing, collapse = ", "))
    }
  }

  terms
}
