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
                       fillOpacity = 0.8,popup = popup_tpl,clusterOptions = markerClusterOptions())%>%fitBounds(minLong,minLat,maxLong,maxLat)
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
    sent = get_sentences(strsplit(input$wc2_clicked_word,":")[[1]][1])
    shinyalert(   
      text = if(is.null(sent)){
      sent = "Not valid UTF Character"
      sent
    }
    else{unique(get_sentences(strsplit(input$wc2_clicked_word,":")[[1]][1]))}
      
               ,
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
      data<-data%>%select('video_id','publishedAt','title','channelTitle','thumbnails.medium.url')
      # Title Frequency Distribution
      words<-strsplit(data$channelTitle," ")
      #Instantiate empty list
      bag_of_words = vector('list',nrow(data))
      
      for (i in 1:nrow(data)){
        bag_of_words[[i]]= data$channelTitle[i]
      }
      # Count frequenct of each title
      title_freq = table(unlist(bag_of_words))
      
      ## Make a dataframe for title and frequency
      words=names(title_freq)
      freq=as.integer(title_freq)
      word_table=cbind(words,freq)
      word_table=as.data.frame(word_table,stringsAsFactors = FALSE)
      word_table$freq=as.numeric(word_table$freq)
      word_table_order=word_table%>%arrange(desc(word_table$freq))
      colnames(word_table_order)=c("Channel_Title","Frequency")
      top = head(word_table_order)
      ## Filter the master_data with channel_title(top frequency)
      
      data_filter<-data%>%filter(channelTitle %in% top$Channel_Title)
      
      return(data_filter[,1:4])})
  output$video_stats<-renderPlot({
    
    video_stats<-read.csv('video_stat.csv',stringsAsFactors = FALSE)
    ggplot(data=video_stats,aes(x=video_stats$id,y=video_stats$dislike_Count))+
      geom_bar(aes(fill=Plot_Result$Degree_of_Emotion),stat="identity",width=0.4)+
      scale_fill_brewer(palette="Dark2")+xlab("Degree of Emotion")+
      ylab("Number of Tweets")+
      coord_flip()+
      geom_text(aes(label=Plot_Result$Number_of_Tweets),
                vjust=-0.5,colour="brown",stat="identity")+theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),legend.position = 'none')
    
    
    
  })
  
  
  
  output$Youtube_MasterData<-DT::renderDataTable(master_data())
  Video_channel_ID<-reactive({data<-read.csv('video_channel_ith.csv',stringsAsFactors = FALSE)
    return(data)})
  output$Video_channel_ID<-DT::renderDataTable(Video_channel_ID())
})




