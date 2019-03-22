
options(repos = c(CRAN = "https://cran.rstudio.com"))
#devtools::install_github("AdamSpannbauer/wordcloud2",force = TRUE)
library(devtools)
library(rsconnect)
library(shinyalert)
library(ggplot2)
library(shiny)
library(shinythemes)
library(leaflet)
require(wordcloud2)
library(tm)
library(httr)
library(readr)
library(rtweet)
library(ggmap)
library(leaflet)
library(dplyr)
library(DT)
source('text_mining.R')
source('get_live_data.R')
source('get_sentiment.R')
source('ggplot_gauge.R')
source('youtube_sentiment_analysis.R')
shinyUI(fluidPage(theme = shinytheme('cerulean'),
                  navbarPage(title='Starbucks Social Media Information',
                             id='nav',
                             tabPanel("twitteR",value="twitteR",
                             fluidRow(includeCSS('mystyle.css'),
                                             column(12 , style = "background-color:white",
                                                    column(3,  style = "border-left: 10px solid #00ccff;text-align:center;",titlePanel("Tweets Count"),
                                                           fluidRow(textOutput('Tweets_Count'),style="color:#008b00 ;font-size:30px;text-align:center;")),
                                                    column(3, style = "border-left: 10px solid #47F518;text-align:center", titlePanel("% Positive Tweets"),
                                                           fluidRow(textOutput('Positive_Tweet(%)'),style="color:#47F518;font-size:30px;text-align:center;")),
                                                    column(3,style = "border-left: 10px solid #FF4500;text-align:center", titlePanel("% Negative Tweets"),
                                                           fluidRow(textOutput('Negative_Tweet(%)'),style="color:#E9361D ;font-size:30px;text-align:center;")),
                                                    column(3, style = "border-left: 10px solid #F5E818;text-align:center", titlePanel("% Neutral Tweets"),
                                                           fluidRow(textOutput('Neutral_Tweet(%)'),style="color:#F5E818 ;font-size:30px;text-align:center;"))
                                             ),br(),
                                             column(6,helpText(h3("Enter Topic",style="text-align:center;")),
                                             br(),
                                             textInput(inputId='topic_text',label='',value="starbucks")),
                                             column(6,helpText(h3("Select Time to update data",style="text-align:center;")),
                                             column(6,sliderInput("Minute_Range","Select Minute",min = 0,max=10,value=0.5,step=0.2),align='center'),
                                             br(),br(),
                                             
                                             column(6,actionButton(inputId='Refresh_Live_Data',label = "Refresh Data")),
                                             div(style = "height:30px;background-color:white;")
                                             )
                                      ),
                             ## Next row
                             fluidRow(column(6,leafletOutput('map',height='800px')),includeCSS('mystyle.css'),
                                      column(6,
                                             fluidRow(helpText(h3("WordCloud",style='text-align:center;', div(style = 'background-color:white;'))),
                                                     wordcloud2Output(outputId = "twitter_wordcloud",clickedWordInputId = "wc2_clicked_word",height = '250px'),div(style = "height:50px;"),
                                                      useShinyalert(),
                                                     actionButton("get_sentences", "Get Context for the selected word") 
                                             ),
                                             fluidRow(helpText(h3("twitter Polarity",style='text-align:center;')),
                                                      plotOutput(outputId='plot_emotion_twitter'),div(style = "height:100px;")))),
                            ## Next Row for Data Table
                            fluidRow(column(8,DT::dataTableOutput(outputId = 'table')))
                             
                             ),
                             
                             ## Next Page(Tab Panel)
                             tabPanel('Youtube Analytica',value="Youtube",

                              fluidRow(column(6,
                                              
                                      helpText(h3('Top Most Occuring Channel on Starbuck: '), 
                                                  h4('CNBC, TheEllenShow,BuzzFeed,Starbucks & ThreadBanger')),        
                                      DT::dataTableOutput(outputId="Youtube_MasterData")),
                                       
                                       column(4,
                                              helpText(h3('YouTube Video Statistics'),align='center'),
                                        
                                       fluidRow(textInput('video_ID',value='XUBeH7VQaFY',label='paste video ID'),align='right'),
                                       fluidRow(plotOutput("video_stats")))),
                      
                              fluidRow(
                                column(4,
                                       fluidRow(helpText(h3('Popularity Ratio',style="text-align:center;"))),
                                       fluidRow(plotOutput('video_Popularity_Ratio'))),
                                column(4,
                                       fluidRow(helpText(h3('Engagement Ratio',style="text-align:center;"))),
                                       plotOutput('video_Engagement_Ratio')),
                                column(4,
                                       fluidRow(helpText(h3('Like-Dislike Ratio',style="text-align:center;"))),
                                       plotOutput('video_dislike_Ratio')),
                                       fluidRow(uiOutput('source_url'))  
                                ),
                              
                            fluidRow(column(4,
                                            fluidRow(helpText(h3('Sentiment Polarity',style="text-align:center;"))),
                                            br(),plotOutput('VideoID_polarity')),
                                       column(4,
                                              fluidRow(helpText(h3('Word Cloud For Top Occuring Adverb',style="text-align:center;"))),
                                              fluidRow(sliderInput("Adverb_Freq","Select frequency",min = 5,max=12,value=7,step=1),align='center'),
                                              wordcloud2Output("Adverb")),
                                       column(4,
                                              fluidRow(helpText(h3('Word Cloud For Top Occuring Noun',style="text-align:center;"))),
                                              wordcloud2Output("Noun"))),
                            fluidRow(column(4,fluidRow(helpText(h3('Top occuring Bi-grams in current decade',style="text-align:center;"))),
                                            plotOutput('Bigram')),
                                     column(4,fluidRow(helpText(h3('Top occuring Tri-grams in current decade',style="text-align:center;"))),
                                            plotOutput('Trigram')),
                                     column(4,fluidRow(helpText(h3('Top occuring n-grams in last decade',style="text-align:center;")))))

                                )

                              )
                             
                           ))
        
        
                  
        
        
        

















