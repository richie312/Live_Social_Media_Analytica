## Live  Tweet

library(rtweet)
library(httr)
library(readr)
library(ggmap)
library(leaflet)
library(dplyr)

## Read the credentials
credentials = read.delim2("oauth_details.txt",sep=',',header = FALSE)
credentials$V2=as.character(credentials$V2)

token<-create_token(app = 'RichieApp',consumer_key = credentials$V2[1],
                    consumer_secret = credentials$V2[[2]],
                    access_token = credentials$V2[[3]],
                    access_secret = credentials$V2[[4]])

createTokenNoBrowser<- function(appName, consumerKey, consumerSecret, 
                                accessToken, accessTokenSecret) {
  app <- httr::oauth_app(appName, consumerKey, consumerSecret)
  params <- list(as_header = TRUE)
  credentials <- list(oauth_token = accessToken, 
                      oauth_token_secret = accessTokenSecret)
  token <- httr::Token1.0$new(endpoint = NULL, params = params, 
                              app = app, credentials = credentials)
  return(token)
}

token <- createTokenNoBrowser("RichieApp",credentials$V2[1],credentials$V2[2],
                              credentials$V2[3],credentials$V2[4])

# Twitter Live Stream
# Define the length of the stream
streamtime = 0.5*60
# Stream data where it will be stored
filename <- "live.json"
# Function to call the data
rt <- stream_tweets(q = 'starbucks',timeout = streamtime,file_name = filename)
# parse the data
live_data<-parse_stream(filename)

# Get the user's location details through geocoding and filterout the NA values
live_data = live_data%>%filter(live_data$location != 'NA')
live_data$geo_coords
#Register the geocode API for google
register_google(credentials$V2[credentials$V1=='geocode_api'])

#instantiate the empty dataframe()
live_data = live_data %>%filter(geo_coor != 'Universe' & location != 'WORLDWIDE')

## Instantiate two list

longitude = rep(c(0,nrow(live_data)))
latitude = rep(c(0,nrow(live_data)))

for (i in 1:length(live_data)){
  user_loc_geocode <- if(is.na(live_data$location[i])){} else{geocode(live_data$location[i])}
  tryCatch({
  longitude[i]=user_loc_geocode$lon
  latitude[i]=user_loc_geocode$lat},
  error = function(e){}
  )
}

## Add the list in the live_data dataframe
live_data$longitude=longitude
live_data$latitude = latitude

# Convert the lon and lat into numeric value
live_data$longitude=as.numeric(live_data$longitude)
live_data$latitude=as.numeric(live_data$latitude)

## Remove the NA values from  lon and lat and then slect the dataframe

live_data = live_data%>%filter(longitude != 'NA' & latitude != 'NA')

## Select the column names and save it
live_data_required = live_data%>%select(name,longitude,latitude,location,profile_image_url,created_at,text,quoted_followers_count,quoted_retweet_count)
live_data_required=as.data.frame(live_data_required)
# Write the .csv file
write.table(live_data_required, file = "user_loc_df.csv",row.names=FALSE,col.names=TRUE, sep=",")

## Read the .csv file
user_loc_df=read.csv('user_loc_df.csv',stringsAsFactors = FALSE)

user_loc_df = user_loc_df[user_loc_df$location != 'Bharat',]

# Visualise the live users on the map(leaflet)
popup_tpl <- paste0("<font color= #E34F4F><h3>User: </h3></font>", user_loc_df$name,"<br>",
                    popup = paste0("<img src = ", user_loc_df$profile_image_url, ">"),"<br>",
                    "<font color= #E34F4F><h3>Quoted Followers Count: </h3></font>",user_loc_df$quoted_followers_count,"<br>",
                    "<font color= #E34F4F><h3>Quoted Retweet Count: </h3></font>",user_loc_df$quoted_retweet_count,"<br>",
                    "<font color= #E34F4F><h3>Time: </h3></font>",user_loc_df$created_at,"<br>",
                    "<font color= #E34F4F><h3>City: </h3></font>",user_loc_df$location,"<br>",
                    "<font color= #E34F4F><h3>Comments: </h3></font>",user_loc_df$text,"<br>")
                      
# Get the bounding box for leaflet

minLong<-min(user_loc_df$longitude,na.rm = TRUE)
maxLong<-max(user_loc_df$longitude,na.rm = TRUE)
minLat<-min(user_loc_df$latitude,na.rm = TRUE)
maxLat<-max(user_loc_df$latitude,na.rm = TRUE)



## Get the fill color based on retweet count

getColor <- function(retweet_counts) {
  sapply(user_loc_df$quoted_retweet_count, function(x) {
  if (is.na(x) == TRUE) {
    "#DEB887"
  } 
    else {
    "#FF0000"
  } })
}


# plot it on leaflet
leaflet(user_loc_df)%>%addProviderTiles(providers$Esri.WorldImagery)%>%
  addCircleMarkers(lng=~user_loc_df$longitude,lat=~user_loc_df$latitude,
                   radius=20,fillColor = getColor(user_loc_df$quoted_retweet_count),color = '#8B4513',
                   fillOpacity = 0.8,popup = popup_tpl)%>%
  fitBounds(minLong,minLat,maxLong,maxLat)




