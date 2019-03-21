
#setwd("C:/Users/aritra.chatterjee/Desktop/Starbucks")

server<-shinyServer(function(input,output,session){
  
## Create the reactive function for live data
  
  live_data<- observeEvent(input$Refresh_Live_Data, {
    
    data<-get_live_data(input$topic_text,input$Minute_Range)
  
    })
  
  daily_data <- reactive({
    invalidateLater(100000,session)
    data = read.csv('user_loc_df.csv',stringsAsFactors = FALSE)
    data = data%>%select('name', 'location',
                          'created_at','text','quoted_followers_count','quoted_retweet_count')
    data$text=iconv(data$text, "latin1", "UTF-8")
    return(data)
  })
  
  output$table<-DT::renderDataTable(daily_data(),options=list(lengthMenu = c(5, 30, 50),
                          rowCallback = I('function(row,data){
                          max(daily_data()$quoted_followers_count)
                          $("td", row).css("background", "orange");}')))
  
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
    map<-leaflet(user_loc_df,width='100%')%>%addProviderTiles(providers$Esri.WorldImagery)%>%
      addCircleMarkers(lng=~user_loc_df$longitude,lat=~user_loc_df$latitude,
                       radius=20,fillColor = getColor(user_loc_df$quoted_retweet_count),color = '#8B4513',
                       fillOpacity = 0.8,popup = popup_tpl,clusterOptions = markerClusterOptions())%>%fitBounds(minLong,minLat,maxLong,maxLat)%>%setView(minLong+50,maxLat-50,zoom=3)
    map
  })

  output$twitter_wordcloud<-renderWordcloud2({
    invalidateLater(100000,session)
    data = read.csv('user_loc_df.csv',stringsAsFactors = FALSE)
    words<-get_words(data$text)
    words<-words[words$freq>1,]
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

    ggplot(data=Plot_Result,aes(x=reorder(Plot_Result$Degree_of_Emotion,Plot_Result$Number_of_Tweets),y=Plot_Result$Number_of_Tweets))+
      geom_bar(aes(fill=Plot_Result$Degree_of_Emotion),stat="identity",width=0.4)+
      scale_fill_brewer(palette="YlOrRd")+xlab("Degree of Emotion")+
      ylab("Number of Tweets")+
      coord_flip()+
      geom_text(aes(label=Plot_Result$Number_of_Tweets),
                vjust=-0.5,colour="brown",stat="identity")+theme_bw() +
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle=0, hjust=1))+
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
      
      return(datatable(data_filter[,1:4],options = list(lengthMenu = c(6, 30, 50))))
             })
  output$video_stats<-renderPlot({
    
    video_stats<-read.csv('video_stat.csv',stringsAsFactors = FALSE)
    
    data<-video_stats[video_stats$id == input$video_ID,]
    labels=colnames(data)
    labels_count=labels[3:6]
    values=c(data$like_Count,data$favorite_Count,
             data$comment_Count,data$dislike_Count)
    values=as.numeric(values)
    data_1=cbind(labels_count,values)
    colnames(data_1)= c('labels','values')
    data_1=as.data.frame(data_1)
    data_1$values=as.character(data_1$values)
    data_1$values = as.integer(data_1$values)
    
    ggplot(data=data_1,aes(x=reorder(data_1$labels,data_1$values),y=data_1$values))+
      geom_bar(aes(fill=data_1$labels),stat="identity",width=0.4)+
      scale_fill_brewer(palette="PuBuGn")+xlab("Count")+
      ylab("Video(ith)")+
      coord_flip()+
      geom_text(aes(label=data_1$values),
                vjust=-0.5,colour="brown",stat="identity")+theme_bw() +
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle=0, hjust=1)) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),legend.position = 'none')
  })
  
  output$video_Popularity_Ratio<-renderPlot({
    video_stats<-read.csv('video_stat.csv',stringsAsFactors = FALSE)
    data<-video_stats[video_stats$id == input$video_ID,]
    data$view_Count=as.numeric(data$view_Count)
    data$like_Count=as.numeric(data$like_Count)
    if (is.na(data$like_Count)){
      data$like_Count = 0
    }
    value = data$like_Count/data$view_Count
    gg.gauge(value*100,breaks=c(0,30,70,100))    
    })
  
  output$video_Engagement_Ratio<-renderPlot({
    video_stats<-read.csv('video_stat.csv',stringsAsFactors = FALSE)
    data<-video_stats[video_stats$id == input$video_ID,]
    data$view_Count=as.numeric(data$view_Count)
    data$comment_Count=as.numeric(data$comment_Count)
    if (is.na(data$comment_Count)){
      data$like_Count = 0
    }
    value = data$comment_Count/data$view_Count
    gg.gauge(value*100,breaks=c(0,30,70,100))    
  })
  
  output$video_dislike_Ratio<-renderPlot({
    video_stats<-read.csv('video_stat.csv',stringsAsFactors = FALSE)
    data<-video_stats[video_stats$id == input$video_ID,]
    data$like_Count=as.numeric(data$like_Count)
    if (is.na(data$like_Count)){
      data$like_Count = 0
    }
    data$dislike_Count=as.numeric(data$dislike_Count)
    if (is.na(data$dislike_Count)){
      data$dislike_Count = 0
    }
    value = data$dislike_Count/data$like_Count
    gg.gauge(if(value == 'NaN'){value = 0}else{value}*100,breaks=c(0,30,70,100))    
    
  })
  

  output$Youtube_MasterData<-DT::renderDataTable(master_data())
  Video_channel_ID<-reactive({data<-read.csv('video_channel_ith.csv',stringsAsFactors = FALSE)
    return(data)})
  output$Video_channel_ID<-DT::renderDataTable(Video_channel_ID())
  
  # Render Source Link for the YouTube Ratio Metrics
  url <- a("Youtube Metrics", href="https://tubularinsights.com/3-metrics-youtube-success/")
  output$source_url<-renderUI(
    tagList("Source: ", url)
  )
  
  output$VideoID_polarity<- renderPlot({
    batch1_comments<-read.csv('batch1_comments.csv',stringsAsFactors = FALSE)
    comments = batch1_comments$V2[batch1_comments$video_id_seq == 'XUBeH7VQaFY']
    polarity_data = get_sentimentscore(comments)
    
      ggplot(data=polarity_data,aes(x=reorder(polarity_data$Degree_of_Emotion,polarity_data$Number_of_Tweets),y=polarity_data$Number_of_Tweets))+
      geom_bar(aes(fill=polarity_data$Degree_of_Emotion),stat="identity",width=0.4)+
      scale_fill_brewer(palette="RdYlGn",direction = -1)+xlab("Degree of Polarity")+
      ylab("Comment Count")+
      coord_flip()+
      geom_text(aes(label=polarity_data$Number_of_Tweets),
                vjust=-1.5,colour="brown",stat="identity")+theme_bw() +
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle=0, hjust=1))+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),legend.position = 'none')
    
    
  })
  
  output$Adverb<-renderWordcloud2({
    batch1_comments<-read.csv('batch1_comments.csv', stringsAsFactors = FALSE)
    colnames(batch1_comments)<-c("ID","comments")
    ## Get the sentiment for the ith video
    # Get the taggings and store it
    pos_tags = readRDS('pos_tags.RDS')
    
    # Instantiate the empty dataframe in order to store the terms
    #  Separate the POStagging and terms for each sentence and combine them into once dataframe.
    pos_term<-vector('list',length(pos_tags))
    term<-vector('list',length(pos_tags))
    # get the length of sentence wrt pos terms
    for (i in 1:length(pos_tags)){
      a<-vector('list',length(pos_tags[[i]]))
      b<-vector('list',length(pos_tags[[i]]))
      for(j in 1:length(pos_tags[[i]])){
        a[j]<-strsplit(as.character(pos_tags[[i]][j]),"/")[1]
        b[j]<-strsplit(as.character(pos_tags[[i]][j]),"/")[2]
      }
      term[[i]]<- a
      pos_term[[i]]<-b
    }
    # Read the pos abbreviation for the Standford NLP
    pos_abb=read.delim2('pos_abb.txt',sep=' ',header = FALSE)
    pos_abb=as.data.frame(pos_abb)
    colnames(pos_abb)=c("Abb","POS","add_param")
    pos_abb$Abb=as.character(pos_abb$Abb)
    pos_abb$POS=as.character(pos_abb$POS)
    pos_abb$add_param=as.character(pos_abb$add_param)
    Noun_Index = which(pos_abb$POS == 'Noun')
    Adverb_Index = which(pos_abb$POS == 'Adverb')
    # make a table of pos_terms and terms after unlisting
    pos_term_extend=unlist(pos_term)
    term_extend=unlist(term)
    term_df=cbind(pos_term_extend,term_extend)%>%as.data.frame()
    colnames(term_df)= c("POS","Words")
    term_df$Words = as.character(term_df$Words)
    term_df$POS = as.character(term_df$POS)
    terms<-get_Adverb_freq(freq=input$Adverb_Freq)
    wordcloud2(terms,size=0.5,color="random-light")
  })
  output$Noun<-renderWordcloud2({
    batch1_comments<-read.csv('batch1_comments.csv', stringsAsFactors = FALSE)
    colnames(batch1_comments)<-c("ID","comments")
    ## Get the sentiment for the ith video
    # Get the taggings and store it
    pos_tags = readRDS('pos_tags.RDS')
    #  Separate the POStagging and terms for each sentence and combine them into once dataframe.
    pos_term<-vector('list',length(pos_tags))
    term<-vector('list',length(pos_tags))
    # get the length of sentence wrt pos terms
    for (i in 1:length(pos_tags)){
      a<-vector('list',length(pos_tags[[i]]))
      b<-vector('list',length(pos_tags[[i]]))
      for(j in 1:length(pos_tags[[i]])){
        a[j]<-strsplit(as.character(pos_tags[[i]][j]),"/")[1]
        b[j]<-strsplit(as.character(pos_tags[[i]][j]),"/")[2]
      }
      term[[i]]<- a
      pos_term[[i]]<-b
    }
    # Read the pos abbreviation for the Standford NLP
    pos_abb=read.delim2('pos_abb.txt',sep=' ',header = FALSE)
    pos_abb=as.data.frame(pos_abb)
    colnames(pos_abb)=c("Abb","POS","add_param")
    pos_abb$Abb=as.character(pos_abb$Abb)
    pos_abb$POS=as.character(pos_abb$POS)
    pos_abb$add_param=as.character(pos_abb$add_param)
    Noun_Index = which(pos_abb$POS == 'Noun')
    Adverb_Index = which(pos_abb$POS == 'Adverb')
    # make a table of pos_terms and terms after unlisting
    pos_term_extend=unlist(pos_term)
    term_extend=unlist(term)
    term_df=cbind(pos_term_extend,term_extend)%>%as.data.frame()
    colnames(term_df)= c("POS","Words")
    term_df$Words = as.character(term_df$Words)
    term_df$POS = as.character(term_df$POS)
    terms<-get_Noun_freq(freq=input$Adverb_Freq)
    wordcloud2(terms,size=1.5,color="random-light")
  })
  
})




