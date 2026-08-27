introduce_NA_unnested <- function(x) {
  lapply(x, function(i) {
    lapply(i, function(j) 
      ifelse(j == "NA", NA, j))
  })
}

df2json <- function(x, id, file_name) {
  jsonlite::write_json(setNames(purrr::transpose(x), id), file_name, auto_unbox = TRUE)
}

json2df <- function(file_name) {
  dplyr::bind_rows(lapply(jsonlite::fromJSON(file_name), function(high_level) {
    high_level[high_level == "NA"] <- NA
    data.frame(high_level)
  }))
}

download_pubmed_by_id <- function(dois) {
  search_res <- entrez_search(db = "pubmed", term = paste0(dois, "[DOI]", collapse = " OR "), use_history = TRUE)
  
  fetch_res <- entrez_fetch(db = "pubmed", 
                            web_history = search_res[["web_history"]], 
                            rettype = "xml", 
                            retmax = search_res[["count"]])
  
  xml_data <- xml2::read_xml(fetch_res)
  articles <- xml2::xml_find_all(xml_data, "//PubmedArticle")
  
  new_publications <- lapply(articles, function(ith_article) {
    single_fields <- unlist(lapply(c("PMID", "ArticleId[@IdType='doi']", "ArticleTitle", "Title"),
                                   function(ith_field) {
                                     xml2::xml_text(xml2::xml_find_first(ith_article, paste0(".//", ith_field)))
                                   }))
    
    abs_nodes <- xml2::xml_find_all(ith_article, ".//AbstractText")
    abs_text <- paste0(xml2::xml_text(abs_nodes), collapse = "\n\n")
    
    c(single_fields, abs_text)
  }) |> 
    do.call(rbind, args = _) |> 
    data.frame() |> 
    setNames(c("PMID", "DOI", "ArticleTitle", "Title", "AbstractText")) 
}
