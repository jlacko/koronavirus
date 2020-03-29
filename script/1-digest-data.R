# digest data = načtení + denormalizace dat

library(tidyverse)

# data z Johns Hopkins
curl::curl_download(
  url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
  destfile = "./data/time_series_19-covid-Confirmed.csv"
)

# data z Ministerstva (a já tam budu!)
curl::curl_download(
  url = "https://onemocneni-aktualne.mzcr.cz/api/v1/covid-19/nakaza.csv",
  destfile = "./data/nakaza.csv"
)


jh_data <- readr::read_csv("./data/time_series_19-covid-Confirmed.csv") %>%
  select(-"Province/State", -Lat, -Long, zeme = "Country/Region") %>%
  pivot_longer(-zeme, names_to = "datum", values_to = "pocet") %>%
  mutate(datum = lubridate::mdy(datum)) %>%
  filter(zeme != "Czechia") # zahodit - aby nebylo 2x

cz_data <- readr::read_csv("./data/nakaza.csv") %>%
  mutate(zeme = "Czechia") %>%
  select(datum, zeme, pocet = pocet_celkem)

write_csv2(bind_rows(jh_data, cz_data), "./data/raw_data.csv")
