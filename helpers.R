render_link <- function(where)
  DT::JS(paste0("function(data, type, row, meta) {
                      return '<a href=\"", where, "/' + data + '.html\">' + data + '</a>';
                    }"))

render_doi <- DT::JS("function(data, type, row, meta) {
                      return '<a href=\"https://www.doi.org/' + data + '\" target=\"_blank\">' + data + '</a>';
                    }")

render_city_lid <- function(lid_column_id)
  DT::JS("function(data, type, row, meta) {
    return '<a href=\"city.html?' + row[",
         lid_column_id, 
         "] + '\" target=\"_blank\">' + data + '</a>';
  }")

render_publication <- DT::JS("function(data, type, row, meta) {
                      return '<a href=\"publication.html?' + data + '\" target=\"_blank\">' + data + '</a>';
                    }")

render_publication_doi <- function(doi_column_id)
  DT::JS("function(data, type, row, meta) {
    return '<a href=\"publication.html?' + row[",
         doi_column_id, 
         "] + '\" target=\"_blank\">' + data + '</a>';
  }")

render_collection <- DT::JS("function(data, type, row, meta) {
                      return '<a href=\"collection.html?' + data + '\" target=\"_blank\">' + data + '</a>';
                    }")

markdown_link <- function(x, link, ext = "")
  paste0("[", x, "](", link, x, ext, ")")

markdown_doi <- function(x)
  markdown_link(x, "https://www.doi.org/")

markdown_pmid <- function(x)
  markdown_link(x, "https://pubmed.ncbi.nlm.nih.gov/")

get_object_from_id <- function(id_col, value_col, source_data)
  DT::JS(
    paste0("const targetId = decodeURIComponent(window.location.search.substring(1));

            Papa.parse('path/to/your/file.csv', {
              download: true, // Tells PapaParse to fetch the external file
              complete: function(results) {
                const data = results.data;
                let foundValue = null;

                // Loop through the parsed data array
                for (let i = 0; i < data.length; i++) {
                  const row = data[i];
      
                  // If the first column matches the ID
                  if (row[0] === targetId) {
                    foundValue = row[1]; // Get the second column
                    break;
                  }
                }

});"
    ))

render_small_items_in_searchPanes <- DT::JS("function(data, type, row, meta) {
                                                if (type === 'sp') {
                                                  return data ? data.split(',').map(function(item) { return item.trim(); }) : data;
                                                }
                                                return data;
                                               }")

fancy_dt <- function(x, options)
  DT::datatable(x,
                escape = FALSE, 
                style = "bootstrap5",
                class = "wrap",
                filter = "top",
                extensions = "Buttons", 
                rownames = FALSE,
                options = options)
