##---------------------------------------------------\
##-------------------------------------------------| |
##                                                 | |
##          ***Beer Review Analysis***             | |
##                                                 | |
##-------------------------------------------------| |
##---------------------------------------------------/
##----Load Libraries----
library(XLConnect)
library(XML)
library(tm)
library(Rstem)
library(Snowball)
library(RWeka)
library(TSA)

setwd("C:/Users/Nick/Documents/RCode/BeerScraper")

top_250_list = "http://beeradvocate.com/lists/top/"

##----Define WebScrapers-----
get_beer_links = function(top_250_list){
  #After trail and error, we find the 207th line read is the key table we want.
  beer_list = unlist(strsplit(readLines(top_250_list)[207]," "))
  invisible(gc()) # Always helpful to free memory after a large web read
  beer_links = regmatches(beer_list,regexpr("/beer/profile/[0-9//+]+",beer_list,perl=TRUE))
  beer_links = beer_links[seq(1,length(beer_links),by=2)] # Removes the brewery page links
  link.front = "http://beeradvocate.com" # Need to append this to the beer pages
  return(sapply(beer_links,function(x) paste(link.front,x,sep="")))
}

get_beer_info = function(top_250_list){
  beer_info_list = unlist(strsplit(readLines(top_250_list)[207],"<[/b]>"))
  invisible(gc()) # Always helpful to free memory after a large web read
  beer_info_list = beer_info_list[seq(2,length(beer_info_list),by=3)]
  beer_name_list = sapply(beer_info_list,function(x) strsplit(x,"<")[[1]][1])
  beer_style_list = sapply(beer_info_list,function(x) strsplit(strsplit(x,"<")[[1]][5],">")[[1]][2])
  beer_abv_list = sapply(beer_info_list,function(x) as.numeric(gsub("%","",strsplit(strsplit(x,"<")[[1]][6]," ")[[1]][3])))
  beer_brewery_list = sapply(beer_info_list,function(x) strsplit(strsplit(x,"<")[[1]][8],">")[[1]][2])
  names(beer_name_list)=1:(length(beer_name_list))
  names(beer_style_list)=1:(length(beer_style_list))
  names(beer_abv_list)=1:(length(beer_abv_list))
  names(beer_brewery_list)=1:(length(beer_brewery_list))
  beer_info = data.frame("name"=beer_name_list, "style"=beer_style_list,
                         "abv"=beer_abv_list, "brewery"=beer_brewery_list)
  return(beer_info)
}

get_beer_review_info = function(beer_link,cutoff_date){
  raw_table_info = readLines(beer_link)
  invisible(gc()) # Always helpful to free memory after a large web read
  beer_name = strsplit(raw_table_info[grep("<title>",raw_table_info)[1]],"[>|]")[[1]][2]
  beer_name = gsub("^\\s+|\\s+$","",beer_name)
  num.reviews.line = grep("Reviews:",raw_table_info)[1]
  num.reviews = as.numeric(strsplit(strsplit(raw_table_info[num.reviews.line],"Reviews: ")[[1]][2],"<br>")[[1]][1])
  review.pages = max(floor(num.reviews/25),1)
  start.seq = seq(1,review.pages*25,by=25)
  review_date = rep(0,num.reviews)
  review_text = rep(0,num.reviews)
  review_rating = rep(0,num.reviews)
  review_state = rep(0,num.reviews)
  store_counter = 1
  for (p in start.seq){
    reviews_page = paste(beer_link,"?view=beer&sort=&start=",p,sep="")
    review_page_info_raw = readLines(reviews_page)[314]# first page = #314
    split_review_info = strsplit(review_page_info_raw,"/community/members/")[[1]]
    split_review_info = split_review_info[2:length(split_review_info)]
    for (r in 1:length(split_review_info)){
      split.review = strsplit(split_review_info[r],"<br>")
      review_text[store_counter]=split.review[[1]][7]
      review_date[store_counter]=as.Date(substr(split.review[[1]][length(split.review[[1]])],1,10),format="%m-%d-%Y")
      review_rating[store_counter]=as.numeric(strsplit(strsplit(split.review[[1]][4],"BAscore_norm")[[1]][2],"[<>]")[[1]][2])
      review_state[store_counter]=split.review[[1]][2]
      store_counter = store_counter + 1
    }# End Review Loop on current page (r)
    if(as.Date(min(review_date[review_date!=0],na.rm=T),origin=as.Date("1970-01-01"))<cutoff_date){
      break
      review_date = review_date[1:store_counter]
      review_text = review_text[1:store_counter]
      review_rating = review_rating[1:store_counter]
    }
  }# End Page loop (p)
  review.frame = data.frame("name"=rep(beer_name,length(review_date)),"date"=review_date,"rating"=review_rating,
                            "state"=review_state,"review"=review_text)
  return(review.frame)
}

##---Retrieve Information from Webscrapers----
top_250_links = get_beer_links(top_250_list)
names(top_250_links)=paste("Beer",1:length(top_250_links),sep="")

top_beer_info = get_beer_info(top_250_list)
top_beer_info$link = top_250_links

beer_data = data.frame("name"=rep(0,0),"date"=rep(0,0),"rating"=rep(0,0),
                       "state"=rep(0,0),"review"=rep(0,0))

for (b in 95:length(top_250_links)){
  beer_name = top_beer_info$name[b]
  print(paste("Retrieving Beer Review Info for",beer_name,"(",b," out of ",length(top_250_links),")."))
  temp_data = get_beer_review_info(top_beer_info$link[b],as.Date("2000-01-01"))
  temp_data = temp_data[temp_data$date!=0,]
  temp_data = temp_data[!is.na(temp_data$date),]
  beer_data = rbind(beer_data,temp_data)
}
rm(b,beer_name,temp_data)
invisible(gc())

##-----merge beer info with review info----
data = merge(beer_data,top_beer_info)
rm(beer_data,top_beer_info)

data$date = as.Date(data$date,origin=as.Date("1970-01-01"))

##----Plot Ratings of Hoppy Beers----

hoppy_beerstyles = c("American Double / Imperial IPA","American IPA","American Pale Ale (APA)")

date.seq = seq(from=as.Date("2008-01-01"),to=range(data$date)[2],by=1)

hoppy_ratings_avg = sapply(date.seq,function(x){
  mean(data$rating[data$style%in%hoppy_beerstyles & data$date==x],na.rm=TRUE)
  })

date.seq = date.seq[!is.na(hoppy_ratings_avg)]
hoppy_ratings_avg = hoppy_ratings_avg[!is.na(hoppy_ratings_avg)]

hoppy_ratings_smooth = lowess(date.seq,hoppy_ratings_avg,f=0.08)$y
hoppy_over_smooth = lowess(date.seq,hoppy_ratings_avg,f=0.4)$y

grid_lines=seq(as.Date("2008-01-01"),as.Date("2014-01-01"), by="+6 month")

plot(date.seq,hoppy_ratings_smooth,main="Avg.Daily Rating of Hoppy Beers",type="l",
     xlab="Date",ylab="Avg. Daily Rating")
abline(v=axis.Date(1, x=grid_lines),col = "lightgray", lty = "dotted", lwd = par("lwd"))
lines(date.seq,hoppy_over_smooth,col="red")

##----remove hoppy trend/find periodicity-----
hoppy_no_trend = hoppy_over_smooth - hoppy_ratings_smooth
periodogram(hoppy_no_trend)
hoppy_spec = spectrum(hoppy_no_trend, method = "ar")

hoppy_spec_max = hoppy_spec$freq[which.max(hoppy_spec$spec)]