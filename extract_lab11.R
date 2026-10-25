
library(httr2)


web_response <- request("https://opentdb.com/api.php?amount=3&category=19&difficulty=hard&type=multiple") |> 
  req_perform()

web_response |> 
  resp_body_json() |> 
  pluck("results") |> 
  map(as_tibble) |> 
  bind_rows() |> 
  unnest(incorrect_answers) |> 
  
  
  read_html("https://michael-weylandt.com/STA9750/miniprojects.html") |> 
  html_elements("#mini-projects .anchored") |> 
  html_text()

read_html("https://michael-weylandt.com/STA9750/miniprojects.html") |> 
  html_element("#mini-projects .anchored") |> 
  html_text()


# extract table from lab11 page and put in a data frame
read_html("https://michael-weylandt.com/STA9750/labs/lab11.html") |> 
  html_element("table") |>
  html_table() |> 
  as_tibble()


# ALL TABLES

read_html("https://michael-weylandt.com/STA9750/labs/lab11.html") |> 
  html_elements("table") |>
  html_table()


read_html("https://en.wikipedia.org/wiki/City_University_of_New_York") |> 
  html_element("TABLE") |> 
  html_table()

# Exercise 2 - CUNY MAP



library(tidyverse)
library(rvest)


library(leaflet)

baruch_info <- read_html("https://en.wikipedia.org/wiki/Baruch_College") |> 
  html_element(".geo") |> 
  html_text() |> 
  str_split_1(";") |> 
  as.numeric() |> 
  set_names(c("latitude", "longitude")) |> 
  bind_rows()

leaflet() |>
  addTiles() |>
  setView(lat = baruch_info$latitude, 
          lng = baruch_info$longitude, 
          zoom=17) |>
  addPopups(lat = baruch_info$latitude, 
            lng = baruch_info$longitude, 
            "Look! It's <b>Baruch College</b>!") |>
  print()

# 1- Building on the code above, write a function that takes the Wikipedia URL 
# for a CUNY as an argument and returns a data frame with two columns:

# Latitude
# longitude
# Confirm that your function works by applying it to Baruch’s Wikipedia page


url <- "https://en.wikipedia.org/wiki/City_University_of_New_York"
get_cuny_coordinates <- function(url){
  read_html(url) |> 
    html_element(".geo") |> 
    html_text() |> 
    str_split_1(";") |> 
    as.numeric() |> 
    set_names(c("latitude", "longitude")) |> 
    bind_rows()
}

get_cuny_coordinates("https://en.wikipedia.org/wiki/Baruch_College")

# 2-Investigate the Wikipedia List of CUNYs and identify which table can be used to get links to each CUNY’s page.

# 3- Parse the table to pull links to each CUNY’s individual Wikipedia page.

CUNYs <- read_html("https://en.wikipedia.org/wiki/List_of_City_University_of_New_York_institutions") |> 
  html_element("tbody") |>
  html_elements("tr td:nth-child(2)") |>
  html_elements("a") 

CUNYs <- data_frame(name = CUNYs |> html_text(), 
                    link = CUNYs |> html_attr("href"))

CUNYs <- read_html("https://en.wikipedia.org/wiki/List_of_City_University_of_New_York_institutions") |> 
  html_element("tbody") |>
  html_elements("tr td:nth-child(2)") |>
  html_elements("a") |> 
  map(\(tr) data_frame(name = tr |> html_text(), 
                       link = tr |> html_attr("href"))) |> 
  bind_rows()

#4- Modify the links extracted from the Wikipedia table to form full URLs.

CUNYs <- CUNYs |> mutate(link = paste0("https://en.wikipedia.org", link))

# 4- Apply your HTML parsing function from Step 1 to each CUNY page using the map function or the rowwise() grouping structure:

CUNYs <- CUNYs |> rowwise() |> mutate(info = get_cuny_coordinates(link))
## Or with `map`
CUNYs <- CUNYs |> mutate(info = map(link, get_cuny_coordinates))

## Then unnest the info column

CUNYs <- CUNYs |> unnest(info)

CUNYs

# 5- Use your results to build a CUNY-wide map


MAP <- leaflet() |> 
  addTiles() |> 
  addMarkers(lng = CUNYs$longitude, 
             lat=CUNYs$latitude, 
             popup=CUNYs$name)

MAP

