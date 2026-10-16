library(dplyr)
library(tidyr)

source("populate-database/populate-helpers.R")

source_dat <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1DODfY94eCqR2Xv1rQHWj7cN3_o7_cIvrQvBeQrjPUfE/edit?usp=sharing",
  sheet = "Liczba odpowiedzi: 1"
)

# example
# source_dat <- source_dat |>
#   add_row(
#     `Sygnatura czasowa` = as.POSIXct("2026-10-07 20:04:05"),
#     `Adres e-mail` = "p",
#     DOI = "10.1016/j.jdent.2011.10.011",
#     `Publication year` = 2026,
#     Country = "Poland",
#     City = "Lubin",
#     `Collection year start` = 2025,
#     `Collection year end` = 2026,
#     Source = "Flagging",
#     `Tick species` = "Big",
#     `Exact location` = "Approximated",
#     `Collection site` = NA,
#     Latitude = "51.395",
#     Longitude = "16.202",
#     `Referenced by (later paper DOI)` = NA,
#     `Referencing (source paper DOI)` = NA,
#     Comment = NA) |>
#   add_row(
#     `Sygnatura czasowa` = as.POSIXct("2026-10-05 11:12:05"),
#     `Adres e-mail` = "p",
#     DOI = "10.1016/j.jdent.2011.10.011",
#     `Publication year` = 2026,
#     Country = "Poland",
#     City = "Wroclaw",
#     `Collection year start` = 2025,
#     `Collection year end` = 2026,
#     Source = "Flagging",
#     `Tick species` = "Big",
#     `Exact location` = "Approximated",
#     `Collection site` = NA,
#     Latitude = "51.395",
#     Longitude = "16.202",
#     `Referenced by (later paper DOI)` = NA,
#     `Referencing (source paper DOI)` = NA,
#     Comment = NA)

known_timestamps <- json2df("intermediates/timestamps.json")

updated_dat <- source_dat |>
  mutate(
    `Sygnatura czasowa` = format(
      `Sygnatura czasowa`,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  ) |>
  left_join(
    known_timestamps,
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
  
  if(nrow(new_cities) > 0) {
    new_cities_loc <- tidygeocoder::geocode(
      new_cities, city = City, country = Country, method = "osm", 
      full_results = TRUE, lat = clat, long = clong
    )
    
    # checking if any new city is already known
    possible_duplicates <- new_cities_loc |>
      drop_na(clat, clong) |>
      rowwise() |>
      mutate(
        known_city_row = list(which(
          clat >= known_cities[["min_clat"]] &
            clat <= known_cities[["max_clat"]] &
            clong >= known_cities[["min_clong"]] &
            clong <= known_cities[["max_clong"]]
        ))
      ) |>
      ungroup() |>
      unnest_longer(known_city_row) |>
      mutate(
        change_str = paste0(
          City, ", ", Country, " -> ",
          known_cities$City[known_city_row], ", ",
          known_cities$Country[known_city_row]
        )
      )
    
    if(nrow(possible_duplicates) > 0) {
      repeat {
        choice <- select.list(
          c(possible_duplicates[["change_str"]], "All"),
          multiple = TRUE,
          title = "Found possible duplicated cities. Choose which matches to swap."
        )
        
        duplicates <- if(all(choice != "All")) {
          possible_duplicates |>
            filter(change_str %in% choice)
        } else possible_duplicates
        
        cities_dist_n <- duplicates |>
          distinct(City, Country) |>
          nrow()
        
        if(cities_dist_n == nrow(duplicates)) {
          break
        }
        
        message(
          "Found conflicting city matches.\n",
          "Select at most one match for each city."
        )
      }
      
      if(nrow(duplicates) > 0) {
        new_cities_loc <- new_cities_loc |>
          anti_join(duplicates, by = join_by(City, Country))
        
        updated_dat <- updated_dat |>
          left_join(
            select(duplicates, City, Country, known_city_row),
            by = join_by(City, Country)
          ) |>
          rowwise() |>
          mutate(
            City = if(!is.na(known_city_row)) {
              known_cities$City[known_city_row]
            } else City,
            Country = if(!is.na(known_city_row)) {
              known_cities$Country[known_city_row]
            } else Country
          ) |>
          ungroup() |>
          select(-known_city_row)
      }
    }
    
    updated_cities <- if(nrow(new_cities_loc) > 0) {
      new_cities_loc |>
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
    } else known_cities
  } else {
    updated_cities <- known_cities
  }
  
  cities_without_loc  <- updated_cities |>
    filter(if_any(
      c(clat, clong, min_clat, max_clat, min_clong, max_clong),
      is.na
    ))
  
  if(nrow(cities_without_loc) > 0) {
    warning(
      paste0(
        "missing or incomplete localization of the following cities:\n",
        paste(
          cities_without_loc[["LID"]], paste0(cities_without_loc[["City"]], ","),
          cities_without_loc[["Country"]], collapse = "\n"
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
    anti_join(
      cities_without_loc,
      by = join_by(Country, City, clat, clong, min_clat, max_clat, min_clong,
                   max_clong, LID)
    ) |>
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
    ungroup() |>
    filter(!inside_box)
  
  if(nrow(wrong_locs) > 0) {
    # testing if collection localizations are max 20 km from city box
    wrong_locs <- wrong_locs |>
      mutate(
        nearest_long = pmin(pmax(Longitude, min_clong), max_clong),
        nearest_lat = pmin(pmax(Latitude, min_clat), max_clat),
        distance = geosphere::distGeo(
            cbind(Longitude, Latitude), cbind(nearest_long, nearest_lat)
        ) / 1000
      ) |>
      filter(distance > 20)
  }
  
  if(nrow(wrong_locs) > 0) {
    error_msg <- paste0(
      "Found ", nrow(wrong_locs),
      " collection localizations more than 20 km outside their city bounds:\n\n",
      paste0(
        wrong_locs[["ID"]], " ", wrong_locs[["City"]], ", ",
        wrong_locs[["Country"]], " ", round(wrong_locs[["distance"]], 2), "km",
        collapse = "\n"
      ),
      "\n\nHow would you like to proceed?"
    )
    
    error_choice <- select.list(
      c("Stop execution", "Continue anyway"),
      title = error_msg
    )
    
    if (error_choice == "Stop execution") {
      stop("Incorrect localizations found.")
    }
  }
}

if(any(updated_dat[["new_dat"]])) {
  known_publications <- json2df("intermediates/publications.json")
    
  new_publications <- updated_dat |>
    filter(new_dat) |>
    select(DOI) |>
    unique() |>
    anti_join(known_publications, by = join_by(DOI))
  
  if(nrow(new_publications) > 0) {
    updated_publications <- new_publications |>
      download_pubmed_by_id() |>
      bind_rows(known_publications)
    
    df2json(
      updated_publications,
      updated_publications[["DOI"]],
      "intermediates/publications.json"
    )
  } else {
    updated_publications <- known_publications
  }
  
  timestamps <- updated_dat |>
    select(ID, `Sygnatura czasowa`)
  
  final_dat <- updated_dat |> 
    select(-`Sygnatura czasowa`, -`Adres e-mail`, -Comment, -new_dat) |> 
    mutate(
      `Collection site` = ifelse(
        is.na(`Collection site`),
        "Unknown",
        `Collection site`
      )
    ) |>
    rename(lat = Latitude, long = Longitude) |>
    left_join(
      select(updated_cities, Country, City, clat, clong, LID),
      by = join_by(Country, City)
    ) |> 
    mutate(
      approxcloc = is.na(clat),
      clat = as.numeric(ifelse(approxcloc, lat, clat)),
      clong = as.numeric(ifelse(approxcloc, long, clong))
    ) |> 
    left_join(updated_publications, by = join_by(DOI)) |> 
    select(-AbstractText, -PMID)
  
  write.csv(final_dat, "intermediates/final_dat.csv", row.names = FALSE)
  df2json(final_dat, final_dat[["ID"]], "intermediates/final_dat.json")
  df2json(timestamps, timestamps[["ID"]], "intermediates/timestamps.json")
  
  if(nrow(new_cities) > 0) {
    df2json(
      updated_cities,
      updated_cities[["LID"]],
      "intermediates/cities.json"
    )
  }
}
