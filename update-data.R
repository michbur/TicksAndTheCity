library(dplyr)
library(tidyr)

clean_geocode_df <- function(geocode_df) {
  geocode_df |>
    unnest_wider(
      boundingbox,
      names_sep = "",
      names_repair = ~dplyr::recode(
        .x,
        boundingbox1 = "min_clat",
        boundingbox2 = "max_clat",
        boundingbox3 = "min_clong",
        boundingbox4 = "max_clong"
      )
    ) |>
    select(
      City, Country, clat, clong, min_clat, max_clat, min_clong, max_clong
    )
}

introduce_NA_unnested <- function(x) {
  lapply(x, function(i) {
    lapply(i, function(j) ifelse(length(j) == 0 || j == "NA", NA, j))
  })
}

source_dat <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1DODfY94eCqR2Xv1rQHWj7cN3_o7_cIvrQvBeQrjPUfE/edit?usp=sharing",
  sheet = "Liczba odpowiedzi: 1"
)

source_dat <- source_dat |>
  add_row(
    `Sygnatura czasowa` = as.POSIXct("2026-10-07 20:04:05"),
    `Adres e-mail` = "p",
    DOI = "123",
    `Publication year` = 2026,
    Country = "Poland",
    City = "Lubin",
    `Collection year start` = 2025,
    `Collection year end` = 2026,
    Source = "Flagging",
    `Tick species` = "Big",
    `Exact location` = "Approximated",
    `Collection site` = NA,
    Latitude = "51.395",
    Longitude = "16.202",
    `Referenced by (later paper DOI)` = NA,
    `Referencing (source paper DOI)` = NA,
    Comment = NA) |>
  add_row(
    `Sygnatura czasowa` = as.POSIXct("2026-10-05 11:12:05"),
    `Adres e-mail` = "p",
    DOI = "123",
    `Publication year` = 2026,
    Country = "Poland",
    City = "Wrocław",
    `Collection year start` = 2025,
    `Collection year end` = 2026,
    Source = "Flagging",
    `Tick species` = "Big",
    `Exact location` = "Approximated",
    `Collection site` = NA,
    Latitude = "51.395",
    Longitude = "16.202",
    `Referenced by (later paper DOI)` = NA,
    `Referencing (source paper DOI)` = NA,
    Comment = NA)

processed_dat_timestamps <- json2df("intermediates/timestamps.json")

updated_dat <- source_dat |>
  mutate(
    `Sygnatura czasowa` = format(
      `Sygnatura czasowa`,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  ) |>
  left_join(
    processed_dat_timestamps,
    by = join_by(`Sygnatura czasowa`),
    relationship = "one-to-one"
  ) |>
  mutate(new_dat = is.na(ID)) |>
  arrange(`Sygnatura czasowa`)

known_cities <- jsonlite::read_json("intermediates/cities.json",
                                    simplifyVector = FALSE) |>
  introduce_NA_unnested() |>
  bind_rows()

if(any(updated_dat[["new_dat"]])) {
  new_cities <- updated_dat |>
    select(City, Country) |>
    unique() |> 
    anti_join(known_cities, by = join_by(City, Country))
  
  updated_cities <- if(nrow(new_cities) > 0) {
    tidygeocoder::geocode(
      new_cities, city = City, country = Country, method = "osm", 
      full_results = TRUE, lat = clat, long = clong
    ) |>
      clean_geocode_df() |>
      bind_rows(known_cities) |>
      mutate(
        LID = {
          last_lid_num <- as.numeric(substr(max(LID, na.rm = TRUE), 4, 9))
          LID[is.na(LID)] <- paste0(
            "CIT",
            sprintf("%06d", (last_lid_num + 1):(last_lid_num + sum(is.na(LID))))
          )
          LID
        }
      )
  } else {
    known_cities
  }
  
  cities_without_loc  <- updated_cities |>
    rowwise() |>
    filter(any(is.na(c(clat, clong, min_clat, max_clat, min_clong, max_clong))))
  
  if(nrow(cities_without_loc) > 0) {
    warning(
      paste0(
        "geocode failed for the following cities:\n",
        paste(
          apply(
            cities_without_loc[c("City", "Country", "LID")],
            1, paste, collapse = ", "
          ),
          collapse = "\n"
        )
      )
    )
  }
  
  updated_dat <- updated_dat |>
    mutate(
      ID = {
        last_id_num <- as.numeric(substr(max(ID, na.rm = TRUE), 4, 9))
        ID[new_dat] <- paste0(
          "TIC",
          sprintf("%06d", (last_id_num + 1):(last_id_num + sum(new_dat)))
        )
        ID
      }
    )
  
  # testing if collection localizations are withing city box
  wrong_locs <- updated_dat |>
    filter(new_dat) |>
    left_join(updated_cities, by = join_by(Country, City)) |>
    anti_join(cities_without_loc) |>
    mutate(
      across(
        c(Latitude, Longitude, min_clat, max_clat, min_clong, max_clong),
        as.numeric
      )
    ) |>
    rowwise() |>
    mutate(
      inside_box = {
        Latitude >= min_clat &&
          Latitude <= max_clat &&
          Longitude >= min_clong &&
          Longitude <= max_clong
      }
    ) |>
    filter(!inside_box)
  
  if(nrow(wrong_locs) > 0) {
    wrong_locs <- wrong_locs |>
      mutate(
        distance = {
          nearest_long <- min(max(Longitude, min_clong), max_clong)
          nearest_lat <- min(max(Latitude, min_clat), max_clat)
          
          geosphere::distGeo(
            c(Longitude, Latitude), c(nearest_long, nearest_lat)
          ) / 1000
        }
      ) |>
      filter(distance > 20)
  }
  
  if(nrow(wrong_locs) > 0) {
    stop(
      "Found collection localizations more than 20 km
      from the correspondind city bounds.
      Inspect `wrong_locs` for details.
      Correct the source data in google sheet,
      `updated_cities` or `updated_dat`."
    )
  }
}

if(any(updated_dat[["new_dat"]])) {
  write.csv(
    updated_cities,
    file = "./intermediates/cities.csv",
    row.names = FALSE
  )
  
  df2json(
    updated_cities,
    updated_cities[["LID"]],
    "intermediates/cities.json"
  ) 
}
