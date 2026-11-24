# parse_bmde_terms.R
#
# Download BMDE field names and descriptions from NatureCounts
# (https://naturecounts.ca/nc/default/nc_bmde_fields.jsp) and write one CSV
# per requested BMDE version to data/external/<version>.csv, with columns
# `field` and `description`.
#
# Versions may be given either as the codes used by the page's drop-down
# selector (e.g. "BMDE2.00", "BMDE2.00-ext", "BMDE-BAND-2.00") or as the
# labels shown in it (e.g. "BMDE 2.0 (extended list)").
#
# The page embeds every version's field list as JSON in the `data-props`
# attribute of the BMDEFieldsNCPage component, so a single request is enough.
#
# Output always goes to the project's data/external/ folder, regardless of
# the working directory. The project root is the nearest folder above this
# script containing an .Rproj file (falling back to searching upward from the
# working directory if the script's own location can't be determined).
#
# Usage (paths shown relative to the project root):
#   Rscript scripts/data_processing/parse_bmde_terms.R --list
#   Rscript scripts/data_processing/parse_bmde_terms.R BMDE1.38 BMDE-BAND-2.00
#   Rscript scripts/data_processing/parse_bmde_terms.R --overwrite BMDE2.00
#   Rscript scripts/data_processing/parse_bmde_terms.R --all
#
# Or from R:
#   source("scripts/data_processing/parse_bmde_terms.R")
#   parse_bmde_terms(c("BMDE1.38", "BMDE-NEST-2.00"))

bmde_url <- "https://naturecounts.ca/nc/default/nc_bmde_fields.jsp"

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
bmde_script_path <- if (sys.nframe() == 0L) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
} else {
  ofiles <- Filter(Negate(is.null), lapply(sys.frames(), function(f) f$ofile))
  if (length(ofiles) > 0) ofiles[[length(ofiles)]] else NA_character_
}

bmde_project_root <- function() {
  root <- if (!is.na(bmde_script_path)) {
    find_project_root(dirname(bmde_script_path))
  } else NA_character_
  if (is.na(root)) root <- find_project_root(getwd())
  if (is.na(root)) {
    stop("Could not locate the project root (a folder containing an .Rproj ",
         "file); pass `out_dir` explicitly.")
  }
  root
}

unescape_html <- function(x) {
  entities <- list(
    c("&quot;", "\""), c("&#34;", "\""), c("&#39;", "'"), c("&apos;", "'"),
    c("&lt;", "<"), c("&gt;", ">"), c("&nbsp;", " "),
    c("&amp;", "&") # must be last
  )
  for (e in entities) x <- gsub(e[1], e[2], x, fixed = TRUE)
  x
}

# Fetch the page and return the parsed props: `lookup` (named character
# vector, code -> label) and `fields` (named list, label -> data frame).
fetch_bmde_props <- function(url = bmde_url) {
  html <- paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tag <- regmatches(
    html,
    regexpr('data-componentname="BMDEFieldsNCPage"[^>]*data-props="[^"]*"', html)
  )
  if (length(tag) == 0) {
    stop("Could not find BMDEFieldsNCPage data-props on ", url,
         "; the page structure may have changed.")
  }
  props_json <- unescape_html(sub('.*data-props="([^"]*)"$', "\\1", tag))
  props <- jsonlite::fromJSON(props_json, simplifyVector = FALSE)

  lookup <- unlist(props$versionLookup)
  fields <- lapply(props$versionFields, function(v) {
    data.frame(
      field = trimws(vapply(v$fields, function(f) f$field %||% NA_character_, "")),
      # descriptions are HTML-escaped a second time inside the JSON
      description = trimws(unescape_html(
        vapply(v$fields, function(f) f$description %||% NA_character_, ""))),
      stringsAsFactors = FALSE
    )
  })
  names(fields) <- vapply(props$versionFields, function(v) v$version, "")
  list(lookup = lookup, fields = fields)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Map user-supplied codes or labels to version codes.
resolve_versions <- function(versions, lookup) {
  codes <- vapply(versions, function(v) {
    if (v %in% names(lookup)) return(v)
    hit <- names(lookup)[lookup == v]
    if (length(hit) == 1) return(hit)
    NA_character_
  }, "")
  bad <- versions[is.na(codes)]
  if (length(bad) > 0) {
    stop(
      "Unknown BMDE version(s): ", paste(bad, collapse = ", "), "\n",
      "Available versions:\n",
      paste0("  ", names(lookup), "  (", lookup, ")", collapse = "\n")
    )
  }
  unname(codes)
}

parse_bmde_terms <- function(versions = NULL,
                             out_dir = file.path(bmde_project_root(), "data", "external"),
                             overwrite = FALSE,
                             url = bmde_url) {
  props <- fetch_bmde_props(url)
  codes <- if (is.null(versions)) names(props$lookup) else
    resolve_versions(versions, props$lookup)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_files <- character(0)
  for (code in codes) {
    label <- props$lookup[[code]]
    df <- props$fields[[label]]
    if (is.null(df)) {
      warning("No field list found on page for ", code, " (", label, "); skipping.")
      next
    }
    out_file <- file.path(out_dir, paste0(code, ".csv"))
    if (file.exists(out_file) && !overwrite) {
      message("Skipping ", out_file, " (exists; use overwrite = TRUE / --overwrite)")
      next
    }
    readr::write_csv(df, out_file, na = "")
    message("Wrote ", nrow(df), " fields for ", label, " -> ", out_file)
    out_files <- c(out_files, out_file)
  }
  invisible(out_files)
}

# ---- Command-line entry point ----------------------------------------------
if (sys.nframe() == 0L) {
  # renv only auto-activates when R starts in the project root, so load the
  # project library explicitly in case Rscript was run from elsewhere.
  root <- bmde_project_root()
  if (Sys.getenv("RENV_PROJECT") != root &&
      file.exists(file.path(root, "renv", "activate.R")) &&
      requireNamespace("renv", quietly = TRUE)) {
    suppressMessages(renv::load(root))
  }

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0 || any(args %in% c("-h", "--help"))) {
    cat("Usage: Rscript scripts/data_processing/parse_bmde_terms.R [--overwrite] [--all | --list | VERSION ...]\n")
    quit(status = if (length(args) == 0) 1 else 0)
  }
  if ("--list" %in% args) {
    lookup <- fetch_bmde_props()$lookup
    cat(paste0(names(lookup), "\t", lookup), sep = "\n")
    quit(status = 0)
  }
  overwrite <- "--overwrite" %in% args
  versions <- setdiff(args, c("--overwrite", "--all"))
  if ("--all" %in% args) versions <- NULL
  parse_bmde_terms(versions, overwrite = overwrite)
}
