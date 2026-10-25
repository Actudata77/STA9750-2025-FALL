
# ---- packages ----
if (!requireNamespace("RSocrata", quietly = TRUE)) install.packages("RSocrata")
library(RSocrata)
library(dplyr)
library(lubridate)

# ---- API token (must be in your .Renviron) ----
# e.g. in ~/.Renviron add this line:
# SOCRATA_APP_TOKEN = "your_token_here"

start_date <- "2024-04-01T00:00:00"
end_date   <- "2024-09-30T23:59:59"


get_311_month <- function(year, month) {
  # Construct start/end for that month
  start <- sprintf("%04d-%02d-01T00:00:00", year, month)
  end <- sprintf("%04d-%02d-%02dT23:59:59",
                 year, month, days_in_month(ymd(sprintf("%04d-%02d-01", year, month))))
  
  # SoQL query selecting just the 9 columns + date filter
  query <- glue::glue("
    select created_date, closed_date, complaint_type, incident_zip, borough,
           latitude, longitude, community_board, bbl
    where created_date between '{start}' and '{end}'
  ")
  
  message('Pulling data for ', month.name[month], ' ', year)
  
  data <- RSocrata::read.socrata(
    paste0("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?$query=", URLencode(query)),
    app_token = Sys.getenv("SOCRATA_APP_TOKEN")
  )
  
  # Light cleaning: only residential/neighborhood complaints
  data_clean <- data |>
    filter(!is.na(complaint_type)) |>
    filter(!grepl("business|restaurant|store|construction|inspection|permit|building|boiler",
                  complaint_type, ignore.case = TRUE))
  
  # Save monthly file
  fname <- sprintf("nyc311_%d_%02d.csv", year, month)
  write.csv(data_clean, fname, row.names = FALSE)
  
  return(data_clean)
}

# Step 5



Sys.getenv("SOCRATA_APP_TOKEN")



# ---- packages ----
if (!requireNamespace("RSocrata", quietly = TRUE)) install.packages("RSocrata")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
library(RSocrata)
library(dplyr)
library(lubridate)
library(purrr)

# sanity check: token
stopifnot(nzchar(Sys.getenv("SOCRATA_APP_TOKEN")))

# columns you want
sel_cols <- paste(
  c("created_date","closed_date","complaint_type","incident_zip","borough",
    "latitude","longitude","community_board","bbl"),
  collapse = ","
)

get_311_month <- function(year, month) {
  year  <- as.integer(year)
  month <- as.integer(month)
  
  # Month start/end
  start <- lubridate::ymd(sprintf("%04d-%02d-01", year, month), quiet = TRUE)
  end   <- (start %m+% months(1)) - lubridate::milliseconds(1)
  
  # Define Socrata-friendly timestamp formatter (inside function)
  socrata_time <- function(x) format(x, "%Y-%m-%dT%H:%M:%OS3")
  
  # SoQL WHERE: monthly range + residential filter
  where_clause <- sprintf(
    'created_date between "%s" and "%s"
     AND complaint_type IS NOT NULL
     AND NOT (
       upper(complaint_type) like "%%BUSINESS%%" OR
       upper(complaint_type) like "%%RESTAURANT%%" OR
       upper(complaint_type) like "%%STORE%%" OR
       upper(complaint_type) like "%%CONSTRUCTION%%" OR
       upper(complaint_type) like "%%INSPECTION%%" OR
       upper(complaint_type) like "%%PERMIT%%" OR
       upper(complaint_type) like "%%BUILDING%%" OR
       upper(complaint_type) like "%%BOILER%%"
     )',
    socrata_time(start), socrata_time(end)
  )
  
  # Columns to pull
  sel_cols <- paste(
    c("created_date","closed_date","complaint_type","incident_zip","borough",
      "latitude","longitude","community_board","bbl"),
    collapse = ","
  )
  
  # Construct URL
  base_url <- "https://data.cityofnewyork.us/resource/erm2-nwe9.csv"
  q <- paste0(
    "$select=", utils::URLencode(sel_cols,     reserved = TRUE),
    "&$where=", utils::URLencode(where_clause, reserved = TRUE),
    "&$order=", utils::URLencode(":id",        reserved = TRUE)
  )
  full_url <- paste0(base_url, "?", q)
  
  message("Pulling data for ", month.name[month], " ", year, " ...")
  
  df <- RSocrata::read.socrata(
    full_url,
    app_token = Sys.getenv("SOCRATA_APP_TOKEN")
  )
  
  # Parse dates safely
  df <- df |>
    dplyr::mutate(
      created_date = lubridate::ymd_hms(created_date, quiet = TRUE),
      closed_date  = lubridate::ymd_hms(closed_date,  quiet = TRUE)
    )
  
  # Save monthly file
  out <- sprintf("nyc311_%d_%02d.csv", year, month)
  write.csv(df, out, row.names = FALSE)
  
  df
}

apr_res <- get_311_month(2024, 4)
nrow(apr_res)
head(apr_res$complaint_type)

months_to_pull <- 4:9
data_list <- purrr::map(months_to_pull, ~ get_311_month(2024, .x))
nyc311_2024 <- dplyr::bind_rows(data_list)




# counts per month (from saved CSVs)
files <- sprintf("nyc311_%d_%02d.csv", 2024, 4:9)
stopifnot(all(file.exists(files)))

counts <- sapply(files, \(f) nrow(read.csv(f, nrows = 1e6)))
data.frame(month = month.abb[4:9], rows = counts)

# combine and save once
nyc311_2024 <- dplyr::bind_rows(lapply(files, read.csv))
write.csv(nyc311_2024, "nyc311_2024_apr_sep_residential.csv", row.names = FALSE)



library(dplyr); library(lubridate)

nyc311_2024 <- nyc311_2024 %>%
  mutate(
    created_date = ymd_hms(created_date, quiet = TRUE),
    closed_date  = ymd_hms(closed_date,  quiet = TRUE),
    response_hours = as.numeric(difftime(closed_date, created_date, units = "hours")),
    incident_zip = as.character(incident_zip),
    latitude  = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude))
  )

# light QC
summary(select(nyc311_2024, borough, complaint_type, response_hours))


  
  



# Temporarily override the WHERE inside the function to this for testing a single day:
  where_clause <- sprintf(
    'created_date between "%s" and "%s"
   AND complaint_type IS NOT NULL
   AND NOT (
     upper(complaint_type) like "%%BUSINESS%%" OR
     upper(complaint_type) like "%%RESTAURANT%%" OR
     upper(complaint_type) like "%%STORE%%" OR
     upper(complaint_type) like "%%CONSTRUCTION%%" OR
     upper(complaint_type) like "%%INSPECTION%%" OR
     upper(complaint_type) like "%%PERMIT%%" OR
     upper(complaint_type) like "%%BUILDING%%" OR
     upper(complaint_type) like "%%BOILER%%"
   )',
    socrata_time(start), socrata_time(end)
  )
  
  
  apr <- get_311_month(2024, 4)
  nrow(apr)
  
  
  sel_cols <- paste(
    c("created_date","closed_date","complaint_type","incident_zip","borough",
      "latitude","longitude","community_board","bbl"),
    collapse = ","
  )
  
  base_url <- "https://data.cityofnewyork.us/resource/erm2-nwe9.csv"
  q <- paste0(
    "$select=", utils::URLencode(sel_cols,     reserved = TRUE),
    "&$where=", utils::URLencode(where_clause, reserved = TRUE),
    "&$order=", utils::URLencode(":id",        reserved = TRUE)
  )
  full_url <- paste0(base_url, "?", q)
  
  message("Pulling data for ", month.name[month], " ", year, " ...")
  message("URL: ", full_url)  # helps debug 400s
  
  df <- RSocrata::read.socrata(
    full_url,
    app_token = Sys.getenv("SOCRATA_APP_TOKEN")
  )
  
  # Parse dates safely
  df <- df |>
    dplyr::mutate(
      created_date = lubridate::ymd_hms(created_date, quiet = TRUE),
      closed_date  = lubridate::ymd_hms(closed_date,  quiet = TRUE)
    )
  
  # save monthly file
  out <- sprintf("nyc311_%d_%02d.csv", year, month)
  write.csv(df, out, row.names = FALSE)
  
  df
}


# ---- run for April–September 2024 (adjust to 3–6 months as you like) ----
months_to_pull <- 4:9
data_list <- map(months_to_pull, ~ get_311_month(2024, .x))
nyc311_2024 <- bind_rows(data_list)

head(nyc311_2024)
nrow(nyc311_2024)
tail(nyc311_2024)
# Save combined file
name_combined <- "nyc311_2024_apr_sep_residential.csv"
write.csv(nyc311_2024, name_combined, row.names = FALSE)