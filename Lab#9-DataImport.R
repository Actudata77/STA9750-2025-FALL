install.packages("httr2")
library(httr2)

curl https://cranlogs.r-pkg.org/downloads/total/last-week/ggplot2

library(jsonlite)
fromJSON("https://cranlogs.r-pkg.org/downloads/total/last-week/ggplot2")

library(jsonlite)
fromJSON("https://cranlogs.r-pkg.org/downloads/total/last-week/dplyr")

library(jsonlite)
fromJSON("https://cranlogs.r-pkg.org/downloads/total/last-week/readr")

my_req <- request("https://cranlogs.r-pkg.org")
print(my_req)

# We see here that this is a GET request by default, indicating we would like 
# a response from the server, but we are not

#We then modify the path of the request to point to the specific resource or endpoint we want to query. 
#For example, if we want to get the “top” R packages for the last day, we can run the following:
my_req <- my_req |> 
  req_url_path_append("top") |>
  req_url_path_append("last-day")
print(my_req)

# execute a request to get a response

my_resp <- req_perform(my_req)
print(my_resp)





#1 Do people prefer more sugary candies? (Think OLS)

library(tidyverse)
library(ggpmisc)
read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/refs/heads/master/candy-power-ranking/candy-data.csv") |>
  mutate(winpercent = winpercent / 100) |>
  ggplot(aes(x=sugarpercent, y=winpercent)) + 
  xlab("Sugariness (Percentile)") + 
  ylab("Popularity (Percentile)") + 
  geom_point() + 
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  stat_smooth(method="lm") + 
  stat_poly_eq(use_label("R2", "R2.CI", "P"), 
               color="red4", 
               size=5) + 
  theme_bw()


# Do people prefer more expensive candies? (Think OLS)

library(tidyverse)
library(ggpmisc)
read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/refs/heads/master/candy-power-ranking/candy-data.csv") |>
  mutate(winpercent = winpercent / 100) |>
  ggplot(aes(x=pricepercent, y=winpercent)) + 
  xlab("Price (Percentile)") + 
  ylab("Popularity (Percentile)") + 
  geom_point() + 
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  stat_smooth(method="lm") + 
  stat_poly_eq(use_label("R2", "R2.CI", "P"), 
               color="red4", 
               size=5) + 
  theme_bw()


# Do people prefer chocolate candies? (Think ANOVA)
library(tidyverse)

candies <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/refs/heads/master/candy-power-ranking/candy-data.csv")
head(candies)

library(tidyverse)
candies |>
  mutate(winpercent = winpercent / 100, 
         chocolate = as.logical(chocolate)) |>
  group_by(chocolate) |>
  mutate(win_mean = mean(winpercent), 
         win_sd   = sd(winpercent), 
         n        = n(), 
         win_se   = win_sd / sqrt(n)) |>
  ungroup() |>
  ggplot(aes(x=chocolate, y=winpercent)) + 
  geom_jitter(height=0, width=0.2, alpha=0.3) + 
  geom_errorbar(aes(ymin = win_mean - win_se, 
                    ymax = win_mean + win_se),
                width=0.2, 
                color="red4") + 
  stat_summary(fun="mean", 
               geom="col", 
               fill="lightblue", 
               color="blue4", 
               alpha=0.4) + 
  geom_point(aes(y=win_mean), size=2, color="red4") + 
  theme_bw() + 
  xlab("Chocolate") + 
  ylab("Popularity (Percentile)") + 
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, 1))

# USING THE FILE SYSTEM

getwd()  # get the current working directory

library(fs)
path("index.qmd") |> path_abs()

dir_ls()  # list files in the current directory
dir_ls(glob = "*.qmd")  # list only .qmd files
# Find largest file in project

library(fs)
library(dplyr)
dir_info() |> slice_max(size) |> pull(path) |> path_abs()

# Often, we want to perform a search over all files and folders in a directory recursively - 
# that is, looking inside subfolders, not just at files in the top level.

library(fs)
library(dplyr)
dir_info("data", recurse=TRUE) |> slice_max(size) |> pull(path) |> path_abs()

# 5 Determine how much storage is used by the 5 largest files in any part of your project directory.

library(fs)
library(dplyr)
dir_info(recurse=TRUE) |> slice_max(size, n=5) |> summarize(total_size = sum(size))

# 6 Use the file_exists function to determine whether you have started Mini-Project #03.

library(fs)
file_exists("mp03.qmd")




# HTTP AND WEB ACCESS

library(httr2)
request("https://raw.githubusercontent.com") |>
  req_url_path("fivethirtyeight", 
               "data",
               "refs",
               "heads",
               "master",
               "candy-power-ranking",
               "candy-data.csv") |>
  req_perform() |>
  resp_body_string() |> 
  read_csv()

library(httr2)
request("https://official-joke-api.appspot.com") |>
  req_url_path("random_joke") |>
  req_perform() |> 
  resp_body_json()

# API usage


library(jsonlite)
fromJSON("https://cranlogs.r-pkg.org/downloads/total/last-week/ggplot2")

library(jsonlite)
fromJSON("https://cranlogs.r-pkg.org/downloads/total/last-week/dplyr")

# Build a request step-by-step
library(httr2)
request

my_req <- request("https://cranlogs.r-pkg.org")
print(my_req)


my_req <- my_req |> 
  req_url_path_append("top") |>
  req_url_path_append("last-day")
print(my_req)

# Execute a request to get a response

my_resp <- req_perform(my_req)
print(my_resp)

# Process the request for use in R

downloads_raw <- resp_body_json(my_resp)
print(downloads_raw)


# Note that httr2 is designed for “piped” work, so we can write the entire process as:

request("https://cranlogs.r-pkg.org") |>
  req_url_path_append("top") |>
  req_url_path_append("last-day") |>
  req_perform() |>
  resp_body_json()

# This data is not super helpful for us, since it’s in a “list of lists” format.

library(jsonlite)

my_resp |>
  resp_body_string() |>
  prettify() # same as before but easier to read



# This is the same data as before, but much easier to read. At this point, we should pause 
# and make an ‘attack plan’ for our analysis. I see several things here:

# I really only want the "downloads" part of the response.
# Each element inside downloads has the same flat structure, so they can be easily built into a one-row data frame.
# The column names are the same for each downloads element, so we will be able to put them into one big easy-to-use data frame.
# To do these steps, we will need to use functionality from the purrr package, which we will discuss in more detail next week. 
# For now, it suffices to run:



library(purrr)
library(tibble)
downloads_df <- downloads_raw |>
  pluck("downloads") |>
  map(as_tibble) |>
  list_rbind()


downloads_df


# Exercises #03
Now it’s your turn! In your breakout rooms, try the following:
  
  Make sure you can run all of the code above.

Modify the above code to get the top 100 R packages.

This is a minor change to the request only, but you will need to read the documentation to see where and how the request needs to be changed.


library(httr2)
library(tidyverse)
request("https://cranlogs.r-pkg.org") |>
  req_url_path_append("top") |>
  req_url_path_append("last-day") |>
  req_url_path_append(100) |>
  req_perform() |>
  resp_body_json() |>
  pluck("downloads") |>
  map(as_tibble) |>
  list_rbind()


# Modify your query to get the daily downloads for the ggplot2 package over the last month. This will 
# require changes to how you process the response, so be sure to look at the raw JSON first.

library(httr2)
library(tidyverse)
request("https://cranlogs.r-pkg.org") |>
  req_url_path_append("downloads") |>
  req_url_path_append("daily") |>
  req_url_path_append("last-month") |>
  req_url_path_append("ggplot2") |>
  req_perform() |>
  resp_body_json() |>
  pluck(1)
pluck("downloads") |>
  map(as_tibble) |>
  list_rbind()


# ‘Functionize’ your daily downloads query as a function which 
# takes an arbitrary package name and gets its daily downloads.


library(httr2)
library(tidyverse)
get_downloads <- function(pkg){
  request("https://cranlogs.r-pkg.org") |>
    req_url_path_append("downloads") |>
    req_url_path_append("daily") |>
    req_url_path_append("last-month") |>
    req_url_path_append(pkg) |>
    req_perform() |>
    resp_body_json() |>
    pluck(1) |> 
    pluck("downloads") |>
    map(as_tibble) |>
    list_rbind() |>
    mutate(package=pkg, 
           day=as.Date(day))
}

gg_downloads <- get_downloads("ggplot2")
print(gg_downloads)

# Use your function to get daily downloads for the following R packages:
ggplot2
dplyr
httr2
tibble
purrr
tidyr and combine your results into a single data frame. Then plot the download trends for each package using ggplot2.
You can do this by calling your function in a loop, but you should also try to apply the map() |> list_rbind() idiom here as well.


library(ggplot2)

PACKAGES <- c("ggplot2", "dplyr", "httr2", "tibble", "purrr", "tidyr")

map(PACKAGES, get_downloads) |>
  list_rbind() |>
  ggplot(aes(x=day, 
             y=downloads, 
             color=package, 
             group=package)) +
  geom_point() +
  geom_line() +
  scale_x_date() +
  xlab("Date of Download") +
  ylab("Number of Package Downloads") +
  theme_bw() +
  labs(caption="Data from cranlogs.r-pkg.org") +
  theme(legend.position="bottom") +
  scale_color_brewer(type="qual", 
                     palette=2, 
                     name="Package Name")
