library(dplyr)

dat <- read.csv("data/cities - PubMed-res.csv")

final_dat <- mutate(dat, ID = paste0("TIC", sprintf("%06d", 1:nrow(dat)))) |> 
  rename(notes = X)

write.csv(final_dat, file = "intermediates/final_dat.csv", row.names = FALSE)

# commented out because it requires external API
# cities_locations <- select(dat, city) |>
#   unique() |>
#   tidygeocoder::geocode(city = city, method = 'osm', lat = lat, long = lon,
#                         custom_query = list(
#                           viewbox = "-25.0,72.0,40.0,34.0",
#                           bounded = 1
#                         ))
# 
# saveRDS(cities_locations, file = "intermediates/cities_locations.RDS")

cities_locations <- readRDS("intermediates/cities_locations.RDS")



