render_link <- function(where)
  DT::JS(paste0("function(data, type, row, meta) {
                      return '<a href=\"", where, "/' + data + '.html\">' + data + '</a>';
                    }"))

render_doi <- DT::JS("function(data, type, row, meta) {
                      return '<a href=\"https://www.doi.org/' + data + '\" target=\"_blank\">' + data + '</a>';
                    }")

render_city <- DT::JS("function(data, type, row, meta) {
                      return '<a href=\"city.html?' + data + '\" target=\"_blank\">' + data + '</a>';
                    }")

render_publication <- DT::JS("function(data, type, row, meta) {
                      return '<a href=\"publication.html?' + data + '\" target=\"_blank\">' + data + '</a>';
                    }")

markdown_link <- function(x, link, ext = "")
  paste0("[", x, "](", link, x, ext, ")")

markdown_doi <- function(x)
  markdown_link(x, "https://www.doi.org/")

markdown_pmid <- function(x)
  markdown_link(x, "https://pubmed.ncbi.nlm.nih.gov/")

fancy_dt <- function(x, options)
  DT::datatable(x,
                escape = FALSE, 
                style = "bootstrap5",
                class = "wrap",
                filter = "top",
                extensions = "Buttons", 
                rownames = FALSE,
                options = options)
