source('get_sentiment.R')
library(StanfordCoreNLP)
library(NLP)
library(dplyr)
#setwd('C:/Users/aritra.chatterjee/Desktop/Live_Social_Media_Analytica')

batch1_comments<-read.csv('batch1_comments.csv', stringsAsFactors = FALSE)
colnames(batch1_comments)<-c("ID","comments")


## Get the sentiment for the ith video

#get_sentimentscore(batch1_comments$comments)

## Parts of Speech

get_POS<-function(text){

## Remove the invalid characters

text1=gsub("https://","",text)
text2= gsub(",","",text1)
text3=gsub("#","",text2)
text4=gsub(">","",text3)
text5=gsub("<","",text4)
text6=gsub("/","",text5)
text7=gsub("[[:punct:]]","",text6)
# Annotation Pipline
s<-text7
p <- StanfordCoreNLP_Pipeline(c("pos", "lemma", "parse"))
doc <- AnnotatedPlainTextDocument(s, p(s))
pos_term<-tagged_sents(doc)
## Get the POS tagging for each and every sentecnce of ,the selected videoo ID
}



# Get the taggings and store it
# Instantiate the empty dataframe in order to store the terms
# pos_tags = vector('list',nrow(batch1_comments))
# 
# for (i in 1:length(pos_tags)){
#   pos_tags[i]=get_POS(batch1_comments$comments[i])
# }

save(pos_tags, file = "pos_tags.RData")
saveRDS(pos_tags,file='pos_tags.RDS')

# lOAD the saved model for pos_tags

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

## Get the most frequent Noun Terms(function)
get_Noun_freq<-function(freq=10){Noun_terms=term_df$Words[term_df$POS==pos_abb$Abb[Noun_Index[1]]|term_df$POS==pos_abb$Abb[Noun_Index[2]]]
Noun_words=as.factor(Noun_terms)
Noun_freq=table(Noun_words)%>%as.data.frame()
Noun_freq=Noun_freq[Noun_freq$Freq >= freq,]
return(Noun_freq)
}
## Get the most frequent Adverb Terms(function)
get_Adverb_freq<-function(freq=5){Adverb_terms=term_df$Words[term_df$POS==pos_abb$Abb[Adverb_Index[1]]|term_df$POS==pos_abb$Abb[Adverb_Index[2]]|term_df$POS==pos_abb$Abb[Adverb_Index[3]]]
Adverb_words=as.factor(Adverb_terms)
Adverb_freq=table(Adverb_words)%>%as.data.frame()
Adverb_freq=Adverb_freq[Adverb_freq$Freq >= freq,]
return(Adverb_freq)
}


############################################################################################################
# ## NGrams Plotting
get_bigram<-function(text){
  sentences<-text
  # Clean
  clean<-function(sentences){
    text1=gsub("https://","",sentences)
    text2= gsub(",","",text1)
    text3=gsub("#","",text2)
    text4=gsub(">","",text3)
    text5=gsub("<","",text4)
    text6=gsub("/","",text5)
    text7=gsub("[[:punct:]]","",text6)
      
  }
  
  ## Corpus
  corpus = VCorpus(VectorSource(clean(sentences)))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  ## Bigram Tokenizer function
  Bigramtokenizer<-function(x){
    unlist(lapply(ngrams(words(x),2),paste,collapse =" "),use.names = FALSE)
  }
  ## Creating doc matrix
  bigramdtm <- TermDocumentMatrix(corpus,control = list(tokenize = Bigramtokenizer))
  # Find frequency
  bigramf <- findFreqTerms(bigramdtm,lowfreq = 10)
  # Convert it into the data form
  Bigram_Terms<-rowSums(as.matrix(bigramdtm[bigramf,]))
  Bigram_Terms<-data.frame(word=names(Bigram_Terms),frequency=Bigram_Terms)
  return(Bigram_Terms)

}

get_trigram<-function(text){
  
  sentences<-text
  # Clean
  clean<-function(sentences){
    text1=gsub("https://","",sentences)
    text2= gsub(",","",text1)
    text3=gsub("#","",text2)
    text4=gsub(">","",text3)
    text5=gsub("<","",text4)
    text6=gsub("/","",text5)
    text7=gsub("[[:punct:]]","",text6)
    
  }
  
  ## Corpus
  corpus = VCorpus(VectorSource(clean(sentences)))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  ## Bigram Tokenizer function
  Trigramtokenizer<-function(x){
    unlist(lapply(ngrams(words(x),3),paste,collapse =" "),use.names = FALSE)
  }
  ## Creating doc matrix
  Trigramdtm <- TermDocumentMatrix(corpus,control = list(tokenize = Trigramtokenizer))
  # Find frequency
  Trigramf <- findFreqTerms(Trigramdtm,lowfreq = 5)
  # Convert it into the data form
  Trigram_Terms<-rowSums(as.matrix(Trigramdtm[Trigramf,]))
  Trigram_Terms<-data.frame(word=names(Trigram_Terms),frequency=Trigram_Terms)
  return(Trigram_Terms)
  
}



