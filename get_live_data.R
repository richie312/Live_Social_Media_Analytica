

## Read the credentials
credentials = read.delim2("oauth_details.txt",sep=',',header = FALSE)
credentials$V2=as.character(credentials$V2)

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



get_live_data<-function(topic,minute){
  
  # Twitter Live Stream
  # Define the length of the stream
  streamtime = minute*60
  # Stream data where it will be stored
  filename <- "live.json"
  # Function to call the data
  rt <- stream_tweets(q = topic,timeout = streamtime,file_name = filename)
  # parse the data
  live_data<-parse_stream(filename)
  
  # Get the user's location details through geocoding and filterout the NA values
  live_data = live_data%>%filter(live_data$location != 'NA')
  
  #Register the geocode API for google
  register_google(credentials$V2[credentials$V1=='geocode_api'])
  
  #instantiate the empty dataframe()
  live_data = live_data %>%filter(location != 'Universe' & location != 'WORLDWIDE')
  
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
  
  user_loc_df<-read.csv('user_loc_df.csv',stringsAsFactors = FALSE)
  
  return(user_loc_df)

}

