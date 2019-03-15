library(devtools)
#devtools::install_github("AdamSpannbauer/wordcloud2",force = TRUE)
library(rsconnect)
library(shinyalert)
library(ggplot2)
library(shiny)
library(shinythemes)
library(leaflet)
library(wordcloud2)
library(tm)
library(rtweet)
library(httr)
library(readr)
library(ggmap)
library(leaflet)
library(dplyr)
library(DT)
source('text_mining.R')
source('get_live_data.R')
source('get_sentiment.R')

#setwd("C:/Users/aritra.chatterjee/Desktop/Starbucks")

server<-shinyServer(function(input,output,session){
  
## Create the reactive function for live data
  
  live_data<- observeEvent(input$Refresh_Live_Data, {
    
    data<-get_live_data(input$topic_text,input$Minute_Range)
  
    })
  
  daily_data <- reactive({
    invalidateLater(100000,session)
    data = read.csv('user_loc_df.csv',stringsAsFactors = FALSE)
    data = data[-5]
    data$text=iconv(data$text, "latin1", "UTF-8")
    return(data)
  })
  
  output$table<-DT::renderDataTable(daily_data())
  output$map<-renderLeaflet({
    
    invalidateLater(100000,session)
    ## Read the .csv file
    user_loc_df=read.csv('user_loc_df.csv',stringsAsFactors = FALSE)
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
    map<-leaflet(user_loc_df)%>%addProviderTiles(providers$Esri.WorldImagery)%>%
      addCircleMarkers(lng=~user_loc_df$longitude,lat=~user_loc_df$latitude,
                       radius=20,fillColor = getColor(user_loc_df$quoted_retweet_count),color = '#8B4513',
                       fillOpacity = 0.8,popup = popup_tpl)%>%fitBounds(minLong,minLat,maxLong,maxLat)
    map
  })

  output$twitter_wordcloud<-renderWordcloud2({
    invalidateLater(100000,session)
    data = read.csv('user_loc_df.csv',stringsAsFactors = FALSE)
    words<-get_words(data$text)
    words<-words[words$freq>2,]
    wordcloud2(words,size=1.5,color="random-light")
  })
  
  observeEvent(input$get_sentences,{
    
    shinyalert(text = get_sentences(strsplit(input$wc2_clicked_word,":")[[1]][1]),
               closeOnClickOutside = TRUE, animation = 'slide-from-bottom')
  })
  
  output$plot_emotion_twitter<-renderPlot({
    invalidateLater(100000,session)
    data = read.csv('user_loc_df.csv',stringsAsFactors = FALSE)
    Plot_Result<-get_sentimentscore(data$text)

    ggplot(data=Plot_Result,aes(x=Plot_Result$Degree_of_Emotion,y=Plot_Result$Number_of_Tweets))+
      geom_bar(aes(fill=Plot_Result$Degree_of_Emotion),stat="identity",width=0.4)+
      scale_fill_brewer(palette="Dark2")+xlab("Degree of Emotion")+
      ylab("Number of Tweets")+
      coord_flip()+
      geom_text(aes(label=Plot_Result$Number_of_Tweets),
                vjust=-0.5,colour="brown",stat="identity")+theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),legend.position = 'none')
    
    })
  master_data<-reactive({data<-read.csv("youttube_starbucks_masterdata.csv",stringsAsFactors = FALSE)
      return(data)})
  output$Youtube_MasterData<-DT::renderDataTable(master_data())
  Video_channel_ID<-reactive({data<-read.csv('video_channel_ith.csv',stringsAsFactors = FALSE)
    return(data)})
  output$Video_channel_ID<-DT::renderDataTable(Video_channel_ID())
})




