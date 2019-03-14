
get_words<-function(text){
  
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
  ## Load the tm package to clean the corpus
  ## Corpus
  Corpus=Corpus(VectorSource(tweet7$Tweets))
  ## Convert to plain text document
  Corpus=tm_map(Corpus,PlainTextDocument)
  ## lower case
  Corpus=tm_map(Corpus,content_transformer(tolower))
  ## Strip Whitespace
  Corpus<-tm_map(Corpus, stripWhitespace)
  # Remove Punctuation
  Corpus=tm_map(Corpus,removePunctuation)
  ## Document term matrix
  myDTM = TermDocumentMatrix(Corpus)
  m = as.matrix(myDTM)
  v = sort(rowSums(m), decreasing = TRUE)
  d<-data.frame(word=names(v),freq=v)
  return(d)
}




