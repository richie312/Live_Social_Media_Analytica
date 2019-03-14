
get_sentimentscore<-function(text){

  Text<- text
  Text<-iconv(Text, "latin1", "UTF-8")
  
  
  ##Clean the tweet for sentiment analysis
  #  remove html links, which are not required for sentiment analysis
  tweet1=gsub("https://","",Text)
  tweet2=gsub("#","",tweet1)
  tweet3=gsub("t.co/","",tweet2)
  tweet4=gsub("@","",tweet3)
  tweet5=gsub("RT|via","",tweet4)
  tweet6=gsub("[[:digit:]]","",tweet5)
  tweet7=gsub("'\'"," ", tweet6)
  tweet7<-tweet7%>%data.frame()
  colnames(tweet7)<-c("Tweets")
  
  
  ## Sentiment Analysis
  
  demondf=tweet7
  demondf=demondf[!is.na(demondf$Tweets),]
  demondf<-demondf%>%data.frame()
  colnames(demondf)<-c("tweets")
  
  ## Scan the Lexicon words (English) database which is in txt format
  opinion.lexicon.pos<-scan('positive-words.txt',what='character',comment.char = ';')
  opinion.lexicon.neg<-scan('negative-words.txt', what='character', comment.char=';')
  # upgrade the positive and negative word list
  pos.words=c(opinion.lexicon.pos,'cpimspeak','aayog')
  neg.words=c(opinion.lexicon.neg,'haunt','modi','fcuk','cancel')
  ## Create the function for the sentiment score
  getsentimentscore=function(sentences,words.positive,words.negative,.progress='none'){
    require(plyr)
    require(stringr)
    scores=laply(sentences,function(sentence,words.positive,words.negative,.progress='none'){
      # let us split each sentence by space delimiter
      words=unlist(str_split(sentence,'\\s+'))
      # Let us match with our database
      pos.matches=!is.na(match(words,pos.words))
      neg.matches=!is.na(match(words,neg.words))
      # get the score
      score= sum(pos.matches)-sum(neg.matches)
      return(score)},words.positive,words.negative,.progress=.progress)
    ## Return the dataframe with the respective sentence and scores
    return(data.frame(text=sentences,score=scores))}
  Result<-getsentimentscore(demondf$tweets)
  
  Result<-as.data.frame(Result)
  
  
  ## Categorise the words to very negative to very positive from tweets of Sully
  vNeg<-nrow(as.data.frame(Result$text[Result$score==-2]))
  Neg<-nrow(as.data.frame(Result$score[Result$score==-1]))
  Neutral<-nrow((as.data.frame(Result$score[Result$score==0])))
  Pos<-nrow(as.data.frame(Result$score[Result$score==2|Result==1]))
  vPos<-nrow(as.data.frame(Result$score[Result$score==3]))
  ## Build the data frame
  Tweets=as.data.frame(c(vNeg,Neg,Neutral,Pos,vPos))
  colnames(Tweets)=c("Number_of_Tweets")
  Emotion=as.data.frame(c("vNeg","Neg","Neutral","Pos","vPos"))
  colnames(Emotion)=c("Degree_of_Emotion")
  data=as.data.frame(cbind(Emotion,Tweets))
  return (data)
}

## Retrace Function to sentence

get_sentences= function(word){
  # Instantiate empt;y list to store sentences from the loop
  sentences = list()
  for (i in 1:length(user_loc_df$text)){
  if (grepl(word,strsplit(user_loc_df$text[i]," ")) == TRUE){
    sentences[i]=user_loc_df$text[i]
  }
}
  return(sentences)
}

# word='about:1'
# get_sentences(strsplit(word,":")[[1]][1])[!unlist(lapply(get_sentences(strsplit(word,":")[[1]][1]), is.null))]
                                                                          

#strsplit(string,":")[[1]][1]

## Translate the language

#library(googleLanguageR)




