# Scrape data off of website

library(xml2)

source_code <- read_html("http://europeelects.eu/data")
links <- xml_find_all(xml_child(source_code, 2), ".//a")

# Only keep CSV data files, and only the summary ones (with both European and National scopes)
data_links <- list()
for (i in 1:length(links)){
  tag <- as.character(links[[i]])
  if (grepl("csv", tag)) {
    name_path <- regmatches(tag, regexpr("http.*csv", tag))
    name <- gsub(">", "", regmatches(name_path, regexpr(">.*csv$", name_path)))
    path <- gsub('>|([\\])|"', "", regmatches(name_path, regexpr("^.*>", name_path)))
    if (!grepl("-[N|E]", name)) {
      data_links[[name]] <- path
    }
  }
}

data_load <- list()
for (i in 1:length(data_links)) {
  data_load[[names(data_links)[i]]] <- read.csv(data_links[[i]])
}



