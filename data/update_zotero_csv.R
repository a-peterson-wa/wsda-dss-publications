# update_zotero_csv.R
#
# Standalone script to fetch publication data from Zotero API based on
# pubs_resources.csv and generate publications.csv. Report IDs/Numbers are used to grab specific items from Zotero library. A dedicated Zotero library is used and kept public for this purpose. This keeps the number of items the API has to return low.
#
# Workflow:
#   1. Reads pubs_resources.csv to get list of needed report IDs/numbers
#   2. Fetches ALL items from Zotero API.
#   3. Filters to only the report numbers in pubs_resources.csv
#   4. Generates publications.csv
#
# Usage:
#   1. Ensure httr and jsonlite packages are installed
#
#   **publications.csv** <-- Used for links in DSS App
#   **pubs_resources.csv** <-- Used by this script to query Zotero API and return item information, including URLs
#
#   The dedicated Zotero library that is queried (dss-literature) is checked weekly with another script to detect broken links which are fixed manually after detection.


# Author: Adam Peterson (adam.peterson@agr.wa.gov)
# Date: 2026-01-11

# Load required packages
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")

library(httr)
library(jsonlite)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Zotero group ID for WSDA publications
ZOTERO_GROUP_ID <- "6220639"

INPUT_FILE <- "C:/Users/adam.peterson/Documents/R/projects/wsda-dss-publications/data/pubs_resources.csv"

OUTPUT_FILE <- "C:/Users/adam.peterson/Documents/R/projects/wsda-dss-publications/data/publications.csv"

# Maximum number of items to fetch from API <- If this ever gets over 100, modify the script for multiple requests. Zotero limits returned items to 100.
FETCH_LIMIT <- 100

# Optional: Collection ID to filter results (NULL = all items) <- TODO: Separate out into separate collections for thematic purposes (e.g. "publications" for publications, "links" for links)
COLLECTION_ID <- NULL

# ==============================================================================
# READ INPUT FILE
#  =============================================================================

cat("================================================================================\n")
cat("ZOTERO PUBLICATIONS UPDATER\n")
cat("================================================================================\n\n")

cat("Reading pubs_resources.csv...\n")
cat("  Input file:", INPUT_FILE, "\n")

if (!file.exists(INPUT_FILE)) {
  stop("Input file not found: ", INPUT_FILE)
}

pubs_resources <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)

# Get unique report numbers (excluding EMPTY entries)
needed_reports <- unique(pubs_resources$reportNumber)
needed_reports <- needed_reports[needed_reports != "EMPTY" & needed_reports != ""]

cat("✓ Found", length(needed_reports), "unique report numbers needed\n")
cat("  Sample:", paste(head(needed_reports, 5), collapse = ", "), "\n\n")

# ==============================================================================
# FETCH FROM ZOTERO API
# ==============================================================================

cat("Fetching publications from Zotero API...\n")
cat("  Group ID:", ZOTERO_GROUP_ID, "\n")
cat("  Limit:", FETCH_LIMIT, "items\n\n")

# Build API URL
if (!is.null(COLLECTION_ID)) {
  api_url <- paste0(
    "https://api.zotero.org/groups/", ZOTERO_GROUP_ID,
    "/collections/", COLLECTION_ID, "/items/top?limit=", FETCH_LIMIT
  )
} else {
  api_url <- paste0(
    "https://api.zotero.org/groups/", ZOTERO_GROUP_ID,
    "/items/top?limit=", FETCH_LIMIT
  )
}

# Make API request
response <- tryCatch({
  GET(api_url, timeout(30))
}, error = function(e) {
  stop("Failed to connect to Zotero API: ", e$message)
})

# Check response status
if (status_code(response) != 200) {
  stop("Zotero API returned error status: ", status_code(response), "\n",
       "Response: ", content(response, "text"))
}

# Parse JSON response
content_text <- content(response, "text", encoding = "UTF-8")
data <- tryCatch({
  fromJSON(content_text)
}, error = function(e) {
  stop("Failed to parse JSON response: ", e$message)
})

cat("✓ Successfully fetched", length(data$key), "items from Zotero\n\n")

# ==============================================================================
# BUILD DATAFRAME
# ==============================================================================

cat("Processing publication data...\n")

publications <- data.frame(
  key = data$key,
  title = data$data$title,
  reportNumber = data$data$reportNumber,
  url = data$data$url,
  itemType = data$data$itemType,
  date = data$data$date,
  stringsAsFactors = FALSE
)

# Generate thumbnail filenames from report numbers
cat("  Generating thumbnail filenames...\n")
publications$thumbnail <- sapply(publications$reportNumber, function(report) {
  if (is.na(report) || report == "") {
    return("")
  }
  # Convert "PNW 615" to "pnw_615.png" <- Important to deal with variable report numbers. Zotero library maintains them as they are in the publication (if it's a publication). Alternative IDs are given for non-publication items (e.g. "AGNET" for "AgWeatherNet").
  thumbnail_name <- gsub(" ", "_", tolower(report))
  paste0(thumbnail_name, ".png")
})

# Filter to only publications with URLs
before_url_filter <- nrow(publications)
publications <- publications[!is.na(publications$url) & publications$url != "", ]
after_url_filter <- nrow(publications)

if (before_url_filter > after_url_filter) {
  cat("  Filtered out", before_url_filter - after_url_filter, "items without URLs\n")
}

# ==============================================================================
# FILTER TO NEEDED REPORT NUMBERS
# ==============================================================================

cat("  Filtering to report numbers in pubs_resources.csv...\n")

# Normalize report numbers for matching
normalize_report <- function(x) {
  gsub("[^A-Za-z0-9]", "", toupper(as.character(x)))
}

publications$reportNumber_norm <- normalize_report(publications$reportNumber)
needed_reports_norm <- normalize_report(needed_reports)

# Filter to only needed reports
before_filter <- nrow(publications)
publications_filtered <- publications[publications$reportNumber_norm %in% needed_reports_norm, ]
after_filter <- nrow(publications_filtered)

# Check for missing report numbers
missing_reports <- needed_reports_norm[!needed_reports_norm %in% publications_filtered$reportNumber_norm]

if (length(missing_reports) > 0) {
  cat("\n⚠ WARNING: The following report numbers from pubs_resources.csv were NOT found in Zotero:\n")
  for (report in missing_reports) {
    original_report <- needed_reports[normalize_report(needed_reports) == report]
    cat("    -", original_report, "\n")
  }
  cat("\n  You may need to add these items to the Zotero library.\n")
}

# Remove the normalized column before writing
publications_filtered$reportNumber_norm <- NULL

cat("  Kept", after_filter, "of", before_filter, "publications (",
    before_filter - after_filter, "filtered out)\n")
cat("✓ Processed", nrow(publications_filtered), "publications\n\n")

# ==============================================================================
# WRITE CSV FILE
# ==============================================================================

cat("Writing CSV file...\n")
cat("  Output:", normalizePath(OUTPUT_FILE, mustWork = FALSE), "\n")

tryCatch({
  write.csv(publications_filtered, OUTPUT_FILE, row.names = FALSE)
  cat("✓ Successfully wrote CSV file\n\n")
}, error = function(e) {
  stop("Failed to write CSV file: ", e$message)
})

