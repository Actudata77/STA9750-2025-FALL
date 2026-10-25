# Format datetimes the way Socrata expects (NYC time)
soql_time <- function(x) format(x, "%Y-%m-%dT%H:%M:%OS3", tz = "America/New_York")

# Build WHERE for a single month (try Sept 2024 first)
w <- paste0(
  "created_date between '", soql_time(as.POSIXct("2024-09-01 00:00:00")),
  "' and '",                soql_time(as.POSIXct("2024-09-30 23:59:59")), "'",
  " AND created_date is not null"
)

# Full URL (no URLencode)
test_url <- paste0(
  "https://data.cityofnewyork.us/resource/erm2-nwe9.csv?",
  "$select=created_date,closed_date,complaint_type,borough,incident_zip&",
  "$where=", w
)

dat <- RSocrata::read.socrata(test_url, app_token = Sys.getenv("SOCRATA_APP_TOKEN"))
nrow(dat); head(dat)

tail(dat)
