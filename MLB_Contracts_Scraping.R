

## load packages
library(rvest)
library(tidyverse)
library(glue)
library(polite)
library(stringr)


# splash page

hostpage = "https://legacy.baseballprospectus.com/compensation/cots/"
hostpage_html <- read_html(hostpage)
list_of_links = html_attr(html_nodes(hostpage_html, "a"), "href")

# clean up links

list_of_links = list_of_links[grepl("/compensation/cots/", list_of_links, ignore.case = TRUE)]

list_of_links = list_of_links[nchar(list_of_links) > 56]

list_of_links = list_of_links[!grepl("league-info", list_of_links, ignore.case = TRUE)]

list_of_links = unique(list_of_links)

list_of_links = list_of_links[-c(1,7, 13, 19, 25, 31, 37, 38)]

# loop

#basetable = tablehold[0,]

finaltable = basetable

for(page_stub in list_of_links) {
  
  page_html = read_html(page_stub)
  
  tablehold = html_nodes(page_html, "table") %>%
    html_table(fill = TRUE)
  
  tablehold = tablehold[[2]]
  
  teamname = tablehold[1,1]
  
  tablehold = tablehold[-1,] %>%
    row_to_names(row_number = 1) %>%
    mutate(Team = sub(" PAY.*", "", teamname))
  
  finaltable = rbind.fill(finaltable, tablehold)
  
  print(sub(" PAY.*", "", teamname))
  
}

write.csv(finaltable, "S:/DSP_DOCUMENTS/RAs/GORMAN/Topoleski, John/MLBContracts/mlbpayrolls.csv")


