
# Map from Lab 01

if (!require("leaflet")) install.packages("leaflet")
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
library(tidyverse)
library(rvest)
library(leaflet)

pAGE <- read_html("https://en.wikipedia.org/wiki/Baruch_College")
pAGE |>
  html_element(".latitude") |>
  html_text2() -> BaruchLatitude
baruch_longitude <- pAGE |>
  html_element(".longitude") |>
  html_text2()

BaruchLatitude <- sum(as.numeric(strsplit(
  BaruchLatitude,
  "[^0123456789]"
)[[1]]) * (1 / 60)^(0:2), na.rm = TRUE)
baruch_longitude <- sum(as.numeric(strsplit(baruch_longitude, "[^0123456789]")[[1]]) *
                          (1 / 60)^(0:2), na.rm = TRUE)

leaflet() |>
  addTiles() |> 
  setView(-baruch_longitude, 
          BaruchLatitude, 
          zoom = 17) |>
  addPopups(-baruch_longitude, 
            BaruchLatitude, 
            "Look! It's <b>Baruch College</b>!") |>
  print()


# MAP of CUNY campuses WEEK 11 LAB 

library(rvest)
library(tidyverse)
library(leaflet)

CUNYs <- read_html("https://en.wikipedia.org/wiki/List_of_City_University_of_New_York_institutions") |> 
  html_element("tbody") |>
  html_elements("tr td:nth-child(2)") |>
  html_elements("a")

CUNYs <- data.frame(name = CUNYs |> html_text(),
                    link = CUNYs |> html_attr("href")
)

get_cuny_gps <- function(url){
  COORDS <- read_html(url) |> html_element(".geo") |> html_text() |> str_split_1(";")
  LAT <- as.numeric(COORDS[1])
  LON <- as.numeric(COORDS[2])
  list(LAT=LAT, LON=LON)
}

CUNYs <- CUNYs |> 
  mutate(link = paste0("https://en.wikipedia.org/", link)) |>
  rowwise() |>
  mutate(gps = list(get_cuny_gps(link))) |>
  unnest_wider(gps)

MAP <- leaflet() |> 
  addTiles() |>
  addMarkers(CUNYs$LON, 
             CUNYs$LAT, 
             popup=CUNYs$name, 
             options = popupOptions(closeOnClick=FALSE))

MAP