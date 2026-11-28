# download_dwc_term_sets.R
#
# Download the Darwin Core distribution term lists ("sets") from the tdwg/dwc
# GitHub repo to data/external/tdwg/.
#
# The repo publishes one-term-per-line, headerless lists in dist/, e.g.
# dist/dwc_vertical.csv, dist/chrono_vertical.csv,
# dist/simple_dwc_vertical.csv. This script finds every *_vertical.csv
# directly in dist/ and downloads any that aren't already in the destination
# folder (delete a file to force it to be re-downloaded).
#
# Use functions/dwc_term_sets.R to add set membership to the Quick Reference
# terms once these files are downloaded.
#
# Usage (paths shown relative to the project root):
#   Rscript scripts/data_processing/download_dwc_term_sets.R
#
# Or from R:
#   source(rprojroot::find_root_file("scripts", "data_processing",
#     "download_dwc_term_sets.R", criterion = rprojroot::is_rstudio_project))

dwc_dist_api <- "https://api.github.com/repos/tdwg/dwc/contents/dist"
dwc_dist_raw <- "https://raw.githubusercontent.com/tdwg/dwc/master/dist"
dwc_sets_dir <- rprojroot::find_root_file(
  "data", "external", "tdwg", criterion = rprojroot::is_rstudio_project)

# Names of the *_vertical.csv files directly in dist/ (not in subdirectories).
listing <- jsonlite::fromJSON(dwc_dist_api)
dwc_set_files <- sort(grep("_vertical\\.csv$",
                           listing$name[listing$type == "file"], value = TRUE))

dir.create(dwc_sets_dir, recursive = TRUE, showWarnings = FALSE)
for (f in dwc_set_files) {
  path <- file.path(dwc_sets_dir, f)
  if (!file.exists(path)) {
    curl::curl_download(file.path(dwc_dist_raw, f), path)
    message("Downloaded ", f)
  }
}
message(length(dwc_set_files), " term set files in ", dwc_sets_dir)
