require(rvest)
require(stringr)

get_response<-function(url){
 #we should get every film content as a whole section,then we can get some misssing value in right order 
  content<-read_html(url,encoding = "utf-8")
  section_css<-"div.lister-list > div > div.lister-item-content"
  section<-html_nodes(content,css = section_css)
  
  rank_css= "h3 > span.lister-item-index.unbold.text-primary"
  #it's important to use "html_node",thus we can get the missing value
  rank<-html_node(section,css =rank_css) %>% html_text()
  rank<-as.numeric(rank)
  
  title_css="h3 > a"
  title<-html_node(section,css =title_css) %>% html_text()
  
  genre_css="p:nth-child(2) > span.genre"
  genre<-html_node(section,css =genre_css) %>% html_text()
  #Data-Preprocessing: removing excess spaces
  genre<-trimws(genre)
  #taking only the first genre of each movie,and coverting from text to factor
  genre<-as.factor(gsub(",.*","",genre))
  
  rating_css="div > div.inline-block.ratings-imdb-rating > strong"
  rating<-html_node(section,css = rating_css) %>% html_text()
  rating<-as.numeric(rating)
  #as the director and actor in the same div tag,we can get them alongside
  director_actor_css="p:nth-child(5)"
  director_actor<-html_node(section,director_actor_css) %>% html_text()
  #removing the enter escape
  temp<-gsub("\n","",director_actor)
  #split the director and actor
  temp<-strsplit(temp,"|",fixed = T)
  # use the sapply to get vector from the list type
  director<-trimws(sapply(temp, "[",1))
  director<-sapply(strsplit(director,":",fixed = T),"[",2)
  director<-as.factor(director)
  actor<-trimws(sapply(temp, "[",2))
  actor<-sapply(strsplit(actor,":",fixed = T),"[",2)
  actor<-as.factor(actor)
  
  gross_box<-html_node(section,css = "p.sort-num_votes-visible > span:nth-child(5)") %>% html_text()
  #use the regular expression to remove the "$" and "M"
  gross_box<-as.numeric(gsub("[$M]","",gross_box))
  
  year<-html_node(section,css = "h3 > span.lister-item-year.text-muted.unbold") %>% html_text()
  year<-unlist(str_extract_all(year,"\\d+"))
  
  run_time<-html_node(section,css = "p:nth-child(2) > span.runtime") %>% html_text()
  run_time<-as.numeric(unlist(str_extract_all(run_time,"\\d+")))
  
  votes_css<-"p.sort-num_votes-visible > span:nth-child(2)"
  votes<-html_node(section,css = votes_css) %>% html_text()
  votes<-as.numeric(unlist(gsub(",","",votes)))
  data<-data.frame(year,title,genre,rank,rating,votes,director,actor,gross_box,run_time)
  return(data)
}

