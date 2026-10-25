
# 01_pull_311_partitioned.R
# ---------------------------------------------
# Requires: SOCRATA_APP_TOKEN in ~/.Renviron

# ---- packages ----
rm(list=ls())

complaints <- c("Noise - Residential","Illegal Parking","HEAT/HOT WATER","Dirty Conditions")
complaints_filter <- paste0("complaint_type in ('", paste(complaints, collapse="','"), "')")

where_month <- function(m) {
  start <- soql_time(floor_date(m, "month"))
  stop  <- soql_time(ceiling_date(m, "month") - milliseconds(1))
  paste(
    sprintf("created_date between '%s' and '%s'", start, stop),
    "AND created_date is not null",
    "AND incident_zip is not null",
    "AND", complaints_filter,
    sep = " "
  )
}



if (!requireNamespace("RSocrata", quietly = TRUE)) {
  install.packages("remotes"); remotes::install_github("Chicago/RSocrata")
}
pkgs <- c("RSocrata","dplyr","purrr","lubridate","arrow","readr","tibble","stringr")
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)))
lapply(pkgs, library, character.only = TRUE)

# ---- config ----
endpoint  <- "https://data.cityofnewyork.us/resource/erm2-nwe9.csv"
app_token <- Sys.getenv("SOCRATA_APP_TOKEN")

# Columns needed (your 9)
cols <- c("created_date","closed_date","complaint_type","incident_zip",
          "borough","latitude","longitude","community_board","bbl")

# Window
months_seq <- seq(ymd("2024-01-01"), ymd("2024-09-01"), by = "1 month")

# Boroughs (exact strings used by 311 dataset)
boroughs <- c("MANHATTAN","BROOKLYN","QUEENS","BRONX","STATEN ISLAND")

# OPTIONAL filter to shrink size (uncomment to use)
complaints <- c(
  # "Noise - Residential","Illegal Parking","HEAT/HOT WATER","Dirty Conditions"
)
complaints_filter <- if (length(complaints)) paste0("complaint_type in ('", paste(complaints, collapse="','"), "')") else NULL

# Output dir
dir.create("data/311_parts", recursive = TRUE, showWarnings = FALSE)

# ---- helpers ----
soql_time <- function(x) format(x, "%Y-%m-%dT%H:%M:%OS3", tz = "America/New_York")

month_where <- function(m, boro = NULL) {
  m_start <- soql_time(m)
  m_end   <- soql_time(ceiling_date(m, "month") - milliseconds(1))
  parts <- c(
    sprintf("created_date between '%s' and '%s'", m_start, m_end),
    "created_date is not null",
    "incident_zip is not null"      # drop records without ZIP for our analysis
  )
  if (!is.null(complaints_filter)) parts <- c(parts, complaints_filter)
  if (!is.null(boro)) parts <- c(parts, sprintf("borough='%s'", boro))
  paste(parts, collapse = " AND ")
}

# Count rows BEFORE downloading (fast)
count_month_borough <- function(m, boro = NULL) {
  w <- month_where(m, boro)
  url <- paste0(endpoint, "?$select=count(1)&$where=", w)
  out <- RSocrata::read.socrata(url, app_token = app_token)
  as.integer(out[[1]])
}

# Page and fetch a given month × borough, return a tibble
fetch_month_borough <- function(m, boro, limit = 50000, max_retries = 3, verbose = TRUE) {
  sel <- paste(cols, collapse = ",")
  base <- paste0(endpoint, "?$select=", sel, "&$where=", month_where(m, boro))
  
  out <- list(); offset <- 0L
  repeat {
    url <- paste0(base, "&$order=created_date", "&$limit=", limit, "&$offset=", offset)
    if (verbose) message(sprintf("[Fetch %s | %s] offset=%d", format(m, "%Y-%m"), boro, offset))
    
    attempt <- 1L
    repeat {
      res <- try(RSocrata::read.socrata(url, app_token = app_token), silent = TRUE)
      if (!inherits(res, "try-error")) break
      if (attempt >= max_retries) stop("API error after retries.")
      Sys.sleep(2 * attempt); attempt <- attempt + 1L
    }
    
    n <- nrow(res); if (verbose) message("  -> rows: ", n)
    if (n == 0) break
    out[[length(out) + 1L]] <- res
    if (n < limit) break
    offset <- offset + limit
  }
  
  if (!length(out)) return(tibble())
  
  dplyr::bind_rows(out) |>
    dplyr::mutate(
      created_date = lubridate::ymd_hms(created_date, quiet = TRUE),
      closed_date  = lubridate::ymd_hms(closed_date,  quiet = TRUE),
      response_time_days = as.numeric(difftime(closed_date, created_date, units = "days"))
    ) |>
    dplyr::filter(!is.na(created_date)) |>
    # keep only resolved for response-time analysis
    dplyr::filter(!is.na(closed_date),
                  response_time_days >= 0,
                  response_time_days < 90) |>
    dplyr::select(dplyr::all_of(cols), response_time_days)
}

# Write one parquet per month × borough; return row count written
write_part <- function(df, m, boro) {
  if (!nrow(df)) return(0L)
  fn <- sprintf("data/311_parts/311_%s_%s.parquet", format(m, "%Y-%m"), str_replace_all(boro, " ", "_"))
  arrow::write_parquet(df, fn)
  nrow(df)
}

# ---- RUN: counts → pulls → write parquet ----
log_counts <- tibble()
log_written <- tibble()

for (m in months_seq) {
  for (b in boroughs) {
    cnt <- count_month_borough(m, b)
    log_counts <- bind_rows(log_counts,
                            tibble(month = format(m, "%Y-%m"), borough = b, api_count = cnt))
    message(sprintf("[COUNT %s | %s] %s rows", format(m, "%Y-%m"), b, format(cnt, big.mark=",")))
    
    # Skip empty months
    if (cnt == 0) next
    
    df <- fetch_month_borough(m, b)
    wrote <- write_part(df, m, b)
    log_written <- bind_rows(log_written,
                             tibble(month = format(m, "%Y-%m"), borough = b, written = wrote))
    message(sprintf("[WROTE %s | %s] %s rows", format(m, "%Y-%m"), b, format(wrote, big.mark=",")))
  }
}

# Save logs (great for Methods section)
readr::write_csv(log_counts,  "data/311_parts/_counts_by_month_borough.csv")
readr::write_csv(log_written, "data/311_parts/_written_by_month_borough.csv")

message("Done. Parts are in data/311_parts")
