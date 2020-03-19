# digest data = načtení + denormalizace dat

library(tidyverse)

curl::curl_download(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                    destfile = "./data/time_series_19-covid-Confirmed.csv")

raw_data <- readr::read_csv("./data/time_series_19-covid-Confirmed.csv") %>% 
  select(-"Province/State", -Lat, -Long, zeme = "Country/Region") %>% 
  pivot_longer(-zeme, names_to = "datum", values_to = "pocet") %>% 
  mutate(datum = lubridate::mdy(datum))

write_csv2(raw_data, "./data/raw_data.csv")
