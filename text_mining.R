
get_words<-function(text){
  # user_loc_df<-read.csv('user_loc_df.csv',stringsAsFactors = FALSE)
  # Text<-user_loc_df$text
  # stopwords = read.delim("stopwords.txt",sep=',')
  Text<-iconv(text, "latin1", "ASCII",sub='')
  
  ##Clean the tweet for sentiment analysis
  #  remove html links, which are not required for sentiment analysis
  tweet1=gsub("https://","",Text)
  tweet2=gsub("#","",tweet1)
  tweet3=gsub("t.co/","",tweet2)
  tweet4=gsub("@","",tweet3)
  tweet5=gsub("RT|via","",tweet4)
  tweet6=gsub("[[:digit:]]","",tweet5)
  tweet7=gsub("'\'"," ", tweet6)
  tweet8=gsub("[[:punct:]]","",tweet7)
  tweet9=gsub("<","",tweet8)
  tweet10=gsub(">","",tweet9)
  tweet11=gsub("+","",tweet9)
  
  tweet8<-tweet8%>%data.frame()
  colnames(tweet8)<-c("Tweets")
  ## Load the tm package to clean the corpus
  ## Corpus
  Corpus=Corpus(VectorSource(tweet8$Tweets))
  ## Convert to plain text document
  Corpus=tm_map(Corpus,PlainTextDocument)
  ## lower case
  Corpus=tm_map(Corpus,content_transformer(tolower))
  ## Strip Whitespace
  Corpus<-tm_map(Corpus, stripWhitespace)
  # Remove Punctuation
  Corpus=tm_map(Corpus,removePunctuation)
  Corpus = tm_map(Corpus, removeWords, stopwords('SMART'))
  Corpus = tm_map(Corpus, removeWords, stopwords)
  
  ## Document term matrix
  myDTM = TermDocumentMatrix(Corpus)
  m = as.matrix(myDTM)
  v = sort(rowSums(m), decreasing = TRUE)
  d<-data.frame(word=names(v),freq=v)
  return(d)
}


