rm(list = ls())  # optional but helps avoid ghosts

library(RSocrata); library(dplyr); library(lubridate); library(arrow); library(tibble)

endpoint  <- "https://data.cityofnewyork.us/resource/erm2-nwe9.csv"
cols <- c("created_date","closed_date","complaint_type","incident_zip",
          "borough","latitude","longitude","community_board","bbl")
dir.create("data/311_months", recursive = TRUE, showWarnings = FALSE)

# NYC-time formatter
soql_time <- function(x) format(x, "%Y-%m-%dT%H:%M:%OS3", tz = "America/New_York")

where_month <- function(m) {
  # harden against numeric input
  if (!inherits(m, "POSIXt")) m <- as.POSIXct(m, origin = "1970-01-01", tz = "America/New_York")
  start <- soql_time(lubridate::floor_date(m, "month"))
  stop  <- soql_time(lubridate::ceiling_date(m, "month") - lubridate::milliseconds(1))
  paste(
    sprintf("created_date between '%s' and '%s'", start, stop),
    "AND created_date is not null",
    "AND incident_zip is not null",
    sep = " "
  )
}

fetch_one_month <- function(m, limit = 50000) {
  sel  <- paste(cols, collapse = ",")
  base <- paste0(endpoint, "?$select=", sel, "&$where=", where_month(m))
  out <- list(); offset <- 0L
  repeat {
    url <- paste0(base, "&$order=created_date", "&$limit=", limit, "&$offset=", offset)
    res <- RSocrata::read.socrata(url, app_token = Sys.getenv("SOCRATA_APP_TOKEN"))
    n <- nrow(res); if (n == 0) break
    out[[length(out)+1L]] <- res
    if (n < limit) break
    offset <- offset + limit
  }
  if (!length(out)) return(tibble())
  dplyr::bind_rows(out) |>
    mutate(
      created_date = ymd_hms(created_date, quiet = TRUE),
      closed_date  = ymd_hms(closed_date,  quiet = TRUE),
      response_time_days = as.numeric(difftime(closed_date, created_date, units = "days"))
    ) |>
    filter(!is.na(created_date),
           !is.na(closed_date),
           response_time_days >= 0,
           response_time_days < 90) |>
    select(all_of(cols), response_time_days)
}

# ✅ Build a POSIXct month vector (name it something other than 'months')
months_seq6 <- seq(
  as.POSIXct("2024-04-01", tz = "America/New_York"),
  as.POSIXct("2024-09-01", tz = "America/New_York"),
  by = "1 month"
)



library(arrow); library(dplyr); library(readr); library(lubridate)

# What files did we produce?
list.files("data/311_months", pattern = "\\.parquet$", full.names = TRUE)

# Peek one file
one <- arrow::read_parquet("data/311_months/311_2024-09.parquet")
nrow(one); dplyr::glimpse(one)

# Sanity on dates & response times
range(one$created_date, na.rm = TRUE)
summary(one$response_time_days)
table(one$borough, useNA = "ifany")[1:10]

getwd()
