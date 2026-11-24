# parse_tdwg_dwc_terms.R
#
# Build a CSV of Darwin Core terms from the Darwin Core Quick Reference Guide
# at https://dwc.tdwg.org/terms/.
#
# One row per term (class definitions are not given their own rows). Columns:
#   term_name       local term name, e.g. "occurrenceID"
#   qualified_name  namespaced name, e.g. "dwc:occurrenceID" (needed because
#                   the same local name can appear in several namespaces,
#                   e.g. dwc:behavior and dwciri:behavior)
#   identifier      term IRI, e.g. http://rs.tdwg.org/dwc/terms/occurrenceID
#   definition
#   notes
#   link            anchor URL for the term on the terms page
#   class           name of the page section/class the term is listed under
#   class_link      anchor URL for that class/section on the terms page
#
# Examples are deliberately omitted.
#
# Sections on the page that are not classes (e.g. "Record-level",
# "UseWithIRI") are recorded in `class` using the section heading.
#
# Default output is the project's
# data/external/tdwg_dwc_quick_reference_terms.csv, regardless of the working
# directory. The project root is the nearest folder above this script
# containing an .Rproj file (falling back to searching upward from the working
# directory if the script's own location can't be determined). An explicit
# output path given on the command line is used as-is (relative paths resolve
# against the working directory).
#
# Usage (paths shown relative to the project root):
#   Rscript scripts/data_processing/parse_tdwg_dwc_terms.R [output.csv]
#
# Or from R:
#   source("scripts/data_processing/parse_tdwg_dwc_terms.R")
#   terms <- parse_tdwg_dwc_terms(out_file = NULL)  # returns data frame only

dwc_terms_url <- "https://dwc.tdwg.org/terms/"

# Walk up from `start` to the first directory containing an .Rproj file.
find_project_root <- function(start) {
  dir <- normalizePath(start, mustWork = FALSE)
  repeat {
    if (length(list.files(dir, pattern = "\\.Rproj$")) > 0) return(dir)
    parent <- dirname(dir)
    if (parent == dir) return(NA_character_)
    dir <- parent
  }
}

# Path of this script, captured when it is loaded: via `--file=` under
# Rscript, or via source()'s `ofile` when sourced. NA if neither applies.
dwc_script_path <- if (sys.nframe() == 0L) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
} else {
  ofiles <- Filter(Negate(is.null), lapply(sys.frames(), function(f) f$ofile))
  if (length(ofiles) > 0) ofiles[[length(ofiles)]] else NA_character_
}

dwc_project_root <- function() {
  root <- if (!is.na(dwc_script_path)) {
    find_project_root(dirname(dwc_script_path))
  } else NA_character_
  if (is.na(root)) root <- find_project_root(getwd())
  if (is.na(root)) {
    stop("Could not locate the project root (a folder containing an .Rproj ",
         "file); pass `out_file` explicitly.")
  }
  root
}

# Convert an HTML fragment to plain text: drop tags, decode entities, tidy
# whitespace.
html_to_text <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  # numeric entities (decimal and hex)
  decode_numeric <- function(s, pattern, base) {
    m <- gregexpr(pattern, s, perl = TRUE)
    regmatches(s, m) <- lapply(regmatches(s, m), function(ents) {
      vapply(ents, function(e) {
        code <- sub(pattern, "\\1", e, perl = TRUE)
        intToUtf8(strtoi(code, base = base))
      }, "")
    })
    s
  }
  x <- decode_numeric(x, "&#([0-9]+);", 10L)
  x <- decode_numeric(x, "&#[xX]([0-9a-fA-F]+);", 16L)
  named <- list(
    c("&quot;", "\""), c("&apos;", "'"), c("&lt;", "<"), c("&gt;", ">"),
    c("&nbsp;", " "), c("&amp;", "&") # &amp; must be last
  )
  for (e in named) x <- gsub(e[1], e[2], x, fixed = TRUE)
  trimws(gsub("\\s+", " ", x))
}

# Extract the HTML content of the <td> following a given row label.
row_value <- function(table_html, label) {
  pattern <- sprintf("<tr><td>%s</td><td>(.*?)</td></tr>", label)
  m <- regmatches(table_html, regexec(pattern, table_html, perl = TRUE))[[1]]
  if (length(m) < 2) NA_character_ else m[2]
}

parse_tdwg_dwc_terms <- function(
    out_file = file.path(dwc_project_root(), "data", "external",
                         "tdwg_dwc_quick_reference_terms.csv"),
    url = dwc_terms_url) {
  html <- paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # Split the page into <h2> sections; each section is a class (or a
  # non-class grouping like "Record-level"). Text before the first <h2> is
  # discarded.
  h2_pattern <- '<h2 id="([^"]+)">(.*?)</h2>'
  h2_pos <- gregexpr(h2_pattern, html, perl = TRUE)[[1]]
  if (h2_pos[1] == -1) stop("No <h2> sections found at ", url)
  h2_tags <- regmatches(html, list(h2_pos))[[1]]
  starts <- as.integer(h2_pos)
  ends <- c(starts[-1] - 1L, nchar(html))

  rows <- list()
  for (i in seq_along(starts)) {
    section <- substr(html, starts[i], ends[i])
    section_id <- sub(h2_pattern, "\\1", h2_tags[i], perl = TRUE)
    section_name <- html_to_text(sub(h2_pattern, "\\2", h2_tags[i], perl = TRUE))

    # Term blocks: <p class="invisible"> anchor spans followed by a
    # table-secondary table. Class tables (table-primary) have no such
    # anchor block and are skipped.
    term_pattern <- paste0(
      '(?s)<p class="invisible">\\s*<span id="([^"]+)"></span>.*?</p>\\s*',
      '<div class="table-responsive"><table class="table">\\s*<tbody>\\s*',
      '<tr class="table-secondary"><th colspan="2">(.*?)</th></tr>(.*?)</tbody>'
    )
    blocks <- regmatches(section, gregexpr(term_pattern, section, perl = TRUE))[[1]]
    if (length(blocks) == 0) next

    for (b in blocks) {
      parts <- regmatches(b, regexec(term_pattern, b, perl = TRUE))[[1]]
      anchor <- parts[2]
      body <- parts[4]
      rows[[length(rows) + 1]] <- data.frame(
        term_name = html_to_text(parts[3]),
        qualified_name = anchor,
        identifier = html_to_text(row_value(body, "Identifier")),
        definition = html_to_text(row_value(body, "Definition")),
        notes = html_to_text(row_value(body, "Notes")),
        link = paste0(dwc_terms_url, "#", anchor),
        class = section_name,
        class_link = paste0(dwc_terms_url, "#", section_id),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) stop("No terms parsed; the page structure may have changed.")
  terms <- do.call(rbind, rows)

  if (!is.null(out_file)) {
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(terms, out_file, na = "")
    message("Wrote ", nrow(terms), " terms in ", length(unique(terms$class)),
            " classes -> ", out_file)
  }
  invisible(terms)
}

# ---- Command-line entry point ----------------------------------------------
if (sys.nframe() == 0L) {
  # renv only auto-activates when R starts in the project root, so load the
  # project library explicitly in case Rscript was run from elsewhere.
  root <- dwc_project_root()
  if (Sys.getenv("RENV_PROJECT") != root &&
      file.exists(file.path(root, "renv", "activate.R")) &&
      requireNamespace("renv", quietly = TRUE)) {
    suppressMessages(renv::load(root))
  }

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0) parse_tdwg_dwc_terms(out_file = args[1]) else
    parse_tdwg_dwc_terms()
}
