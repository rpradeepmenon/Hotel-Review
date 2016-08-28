setwd("~/AscottReview")
library("rvest")

getOnePage=function(url){
  reviews <- url %>%
    read_html() %>%
    html_nodes("#REVIEWS .innerBubble")
  
  id <- reviews %>%
    html_node(".quote a") %>%
    html_attr("id")
  
  quote <- reviews %>%
    html_node(".quote span") %>%
    html_text()
  
  rating <- reviews %>%
    html_node(".rating .rating_s_fill") %>%
    html_attr("alt") %>%
    gsub(" of 5 stars", "", .) %>%
    as.integer()
  
  date <- reviews %>%
    html_node(".rating .ratingDate") %>%
    html_attr("title") %>%
    strptime("%b %d, %Y") %>%
    as.POSIXct()
  
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  
  # put all the fields in a dataframe
  dfrating=data.frame(id=id,quote=quote,date=date,rating=rating,review=review)
  dfrating$rating=as.numeric(substr(dfrating$rating,1,1),1,1)
  
  return(dfrating)

}
