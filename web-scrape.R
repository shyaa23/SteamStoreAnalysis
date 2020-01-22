scrape_show_info <- function(df){
  show = read_html(df)

  title <- show %>%
    #identifies the html/css of interest
    html_nodes("h1") %>%
    #pulls the text
    html_text()

  time <- show %>%
    #identifies the html/css of interest
    html_nodes("#titleDetails time") %>%
    #pulls the text
    html_text() %>%
    #the text includes the parenthesis, this removes the parenthesis which is a special character and requires the \\ escape character
    str_replace(" min", "") %>%
    #converts class to numeric from character
    as.numeric()

  #parses out the year from webpage code
  gen <- show %>%
    #identifies the html/css of interest
    html_nodes(".subtext a+ a , .subtext a:nth-child(4)") %>%
    html_text()%>%
    paste(collapse = ", ")

  #parses out the year from webpage code
  gen <- show %>%
    #identifies the html/css of interest
    html_nodes(".subtext a+ a , .subtext a:nth-child(4)") %>%
    html_text()%>%
    paste(collapse = ", ")
key = show %>%
    #identifies the html/css of interest
    html_nodes(".itemprop") %>%
    html_text()%>%
    paste(collapse = ", ")

  details = tibble(title=title,
                   runtime=time,
                   genre=gen,
                    keywords = key)

  return(details)
}

imdb_page <- read_html("https://www.imdb.com/chart/tvmeter?ref_=nv_tvv_mptv")

imdb_tv_urls = imdb_page %>%
  html_nodes(".titleColumn a")%>%
  #identifies the html/css of interest
  html_attr("href")%>%
  paste("http://www.imdb.com", ., sep = "")

top_100_imdb_shows =
  map_df(imdb_tv_urls, scrape_show_info)

write.csv(top_100_imdb_shows,file="data/topTV.csv")