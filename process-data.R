library(dplyr)
library(rentrez)
library(xml2)

introduce_NA_unnested <- function(x) {
  lapply(x, function(i) {
    lapply(i, function(j) 
      ifelse(j == "NA", NA, j))
  })
}

df2json <- function(x, id, file_name) {
  jsonlite::write_json(
    setNames(purrr::transpose(x), id),
    file_name,
    auto_unbox = TRUE
  )
}
    
json2df <- function(file_name) {
  dplyr::bind_rows(lapply(jsonlite::fromJSON(file_name), function(high_level) {
    high_level[high_level == "NA"] <- NA
    data.frame(high_level, check.names = FALSE)
  }))
}

source_dat <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1DODfY94eCqR2Xv1rQHWj7cN3_o7_cIvrQvBeQrjPUfE/edit?usp=sharing",
  sheet = "Liczba odpowiedzi: 1"
)

# download cities data ------------------

known_cities <- jsonlite::read_json("intermediates/cities.json",
                                    simplifyVector = FALSE) |> 
  introduce_NA_unnested() |>
  bind_rows()

# add a test if found localizations are really close to the exact location

# first run
# cities_locations <- select(source_dat, City, Country) |>
#   unique() |>
#   tidygeocoder::geocode(city = City, country = Country, method = "osm",
#                         lat = clat, long = clong, full_results = TRUE) |>
#   clean_geocode_df()
# cities_locations[["LID"]] <- paste0("CIT", sprintf("%06d", 1:nrow(cities_locations)))
# LID like Location ID


# countries_locations <- data.frame(Country = c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", 
#                                               "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", 
#                                               "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
#                                               "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", 
#                                               "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Iceland", 
#                                               "Liechtenstein", "Norway")) |>
#   tidygeocoder::geocode(country = Country, method = "osm",
#                         lat = clat, long = clong)
# 
# write.csv(countries_locations, file = "./intermediates/countries.csv", row.names = FALSE)

new_cities <- select(source_dat, City, Country) |>
  unique() |> 
  anti_join(known_cities, by = c("City", "Country"))

# get locations of new cities

if(nrow(new_cities) > 0) {
  cities_locations <- tidygeocoder::geocode(
    new_cities, city = City, country = Country, method = "osm", 
    lat = clat, long = clong, full_results = TRUE
    ) |>
    clean_geocode_df() |>
    mutate(
      LID = paste0(
        "CIT",
        sprintf("%06d", (nrow(known_cities) + 1):(nrow(known_cities)
                                                         + nrow(new_cities)))
      )
    )
  
  # add a check if an ID already exists
  
  write.csv(
    rbind(known_cities, cities_locations),
    file = "./intermediates/cities.csv",
    row.names = FALSE
  )
}

# purrr::transpose(cities_locations) |> 
#   setNames(cities_locations[["LID"]]) |> 
#   jsonlite::write_json("intermediates/cities.json", auto_unbox = TRUE)

df2json(cities_locations, cities_locations[["LID"]], "intermediates/cities.json")


# download publication data ------------------

all_dois <- unique(source_dat[["DOI"]])
known_dois <- read.csv("intermediates/publications.csv")[["DOI"]]

# TODO: implement a test where we have <50 new DOIs
new_dois <- setdiff(all_dois, known_dois)

if(length(new_dois) > 0) {
  search_res <- entrez_search(
    db = "pubmed",
    term = paste0(new_dois, "[DOI]", collapse = " OR "),
    use_history = TRUE
  )
  
  fetch_res <- entrez_fetch(
    db = "pubmed", 
    web_history = search_res[["web_history"]],
    rettype = "xml",
    retmax = search_res[["count"]]
  )
  
  xml_data <- read_xml(fetch_res)
  articles <- xml_find_all(xml_data, "//PubmedArticle")
  
  new_publications <- lapply(articles, function(ith_article) {
    single_fields <- unlist(lapply(
      c("PMID", "ArticleId[@IdType='doi']", "ArticleTitle", "Title"),
      function(ith_field) {
        xml_text(xml_find_first(ith_article, paste0(".//", ith_field)))
      }
    ))
    
    abs_nodes <- xml_find_all(ith_article, ".//AbstractText")
    abs_text <- paste0(xml_text(abs_nodes), collapse = "\n\n")
    
    c(single_fields, abs_text)
  }) |> 
    do.call(rbind, args = _) |> 
    data.frame() |> 
    setNames(c("PMID", "DOI", "ArticleTitle", "Title", "AbstractText")) 
  
  # TODO: implement a test if all dois are downloaded
  # all(new_dois %in% new_publications[["DOI"]])
  
  rbind(read.csv("intermediates/publications.csv"), new_publications) |> 
    write.csv(file = "./intermediates/publications.csv", row.names = FALSE)
}

df2json(
  read.csv("intermediates/publications.csv"),
  read.csv("intermediates/publications.csv")[["DOI"]], 
  "intermediates/publications.json"
)

# build final data --------------

id_dat <- source_dat |>
  mutate(ID = paste0("TIC", sprintf("%06d", 1:n())))

timestamps <- id_dat |>
  select(ID, `Sygnatura czasowa`) |>
  mutate(`Sygnatura czasowa` = format(
    `Sygnatura czasowa`,
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ))
  
write.csv(timestamps, file = "intermediates/timestamps.csv", row.names = FALSE)
df2json(timestamps, timestamps[["ID"]], "intermediates/timestamps.json")

final_dat <- id_dat |> 
  select(
    -`Sygnatura czasowa`, -`Adres e-mail`, -`Referenced by (later paper DOI)`,
    -`Referencing (source paper DOI)`, -Comment
  ) |> 
  mutate(
    `Collection site` =
      ifelse(is.na(`Collection site`), "Unknown", `Collection site`)
  ) |> 
  rename(lat = Latitude, long = Longitude) |> 
  left_join(
    json2df("intermediates/cities.json"),
    by = join_by(Country, City)
  ) |> 
  mutate(
    approxcloc = is.na(clat),
    clat = as.numeric(ifelse(approxcloc, lat, clat)),
    clong = as.numeric(ifelse(approxcloc, long, clong))
  ) |> 
  left_join(
    json2df("intermediates/publications.json"),
    by = join_by(DOI)
  ) |> 
  select(-AbstractText, -PMID)

write.csv(final_dat, file = "intermediates/final_dat.csv", row.names = FALSE)
df2json(final_dat, final_dat[["ID"]], "intermediates/final_dat.json")
