library(dplyr)
library(tidyr)
library(rentrez)
library(xml2)

source("populate-database/populate-helpers.R")

source_dat <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1DODfY94eCqR2Xv1rQHWj7cN3_o7_cIvrQvBeQrjPUfE/edit?usp=sharing", 
                                        sheet = "Liczba odpowiedzi: 1")

countries_locations <- data.frame(Country = c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus",
                                              "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany",
                                              "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                                              "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal",
                                              "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Iceland",
                                              "Liechtenstein", "Norway")) |>
  tidygeocoder::geocode(country = Country, method = "osm",
                        lat = clat, long = clong)

cities_locations <- select(source_dat, City, Country) |>
  unique() |>
  tidygeocoder::geocode(city = City, country = Country, method = "osm",
                        lat = clat, long = clong, full_results = TRUE) |>
  clean_geocode_df()

# group_by(cities_locations, clat, clong) |> 
#   summarise(n = length(City), 
#             names = paste0(City, collapse = ", ")) |> 
#   arrange(desc(n))

cities_locations[["LID"]] <- paste0("CIT", sprintf("%06d", 1:nrow(cities_locations)))

new_publications <- download_pubmed_by_id(unique(source_dat[["DOI"]]))

id_dat <- source_dat |>
  mutate(ID = paste0("TIC", sprintf("%06d", 1:n())))

timestamps <- id_dat |>
  select(ID, `Sygnatura czasowa`) |>
  mutate(`Sygnatura czasowa` = format(
    `Sygnatura czasowa`,
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ))

final_dat <- id_dat |> 
  select(-`Sygnatura czasowa`, -`Adres e-mail`, -Comment) |> 
  mutate(`Collection site` = ifelse(is.na(`Collection site`), "Unknown", `Collection site`)) |> 
  rename(lat = Latitude, long = Longitude) |> 
  left_join(cities_locations, by = join_by(Country, City)) |> 
  mutate(approxcloc = is.na(clat),
         clat = as.numeric(ifelse(approxcloc, lat, clat)),
         clong = as.numeric(ifelse(approxcloc, long, clong))) |> 
  left_join(new_publications, by = join_by(DOI)) |> 
  select(-AbstractText, -PMID)

write.csv(final_dat, "intermediates/final_dat.csv", row.names = FALSE)
df2json(final_dat, final_dat[["ID"]], "intermediates/final_dat.json")
df2json(timestamps, timestamps[["ID"]], "intermediates/timestamps.json")
df2json(cities_locations, cities_locations[["LID"]], "intermediates/cities.json")
df2json(new_publications, new_publications[["DOI"]], "intermediates/publications.json")

