
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
library(rtweet)
library(readr)
library(ggmap)
library(leaflet)
library(dplyr)
library(DT)
source('text_mining.R')
source('get_live_data.R')
source('get_sentiment.R')
source('ggplot_gauge.R')

shinyUI(fluidPage(theme = shinytheme('cerulean'),includeCSS('mystyle.css'),
                  navbarPage(title='Starbucks Social Media Information',
                             id='nav',
                             tabPanel("twitteR",value="twitteR",
                             fluidRow(
                                      column(12,div(style = "height:10px;background-color:green;"),
                                             column(6,helpText(h2("Enter Topic",style="color:black;")),
                                             br(),
                                             textInput(inputId='topic_text',label='Type Topic',value="starbucks")),
                                             column(6,helpText(h2("Select Time to update data",style="color:black;")),
                                             column(6,sliderInput("Minute_Range","Select Minute",min = 0,max=10,value=0.5,step=0.2)),
                                             br(),
                                             
                                             column(6,actionButton(inputId='Refresh_Live_Data',label = "Refresh Data")),
                                             div(style = "height:30px;background-color:white;")
                                             ))
                                      ),
                             ## Next row
                             fluidRow(column(6,leafletOutput('map',height='600px')),includeCSS('mystyle.css'),
                                      column(6,
                                             fluidRow(helpText(h2("WordCloud", div(style = 'background-color:white;'))),
                                                     wordcloud2Output(outputId = "twitter_wordcloud",clickedWordInputId = "wc2_clicked_word",height = '250px'),div(style = "height:50px;"),
                                                      useShinyalert(),
                                                     actionButton("get_sentences", "Get Context for the slected word")
                                             ),
                                             fluidRow(helpText(h2("Bar Plot/Sentiment")),
                                                      plotOutput(outputId='plot_emotion_twitter'),div(style = "height:100px;")))),
                            ## Next Row for Data Table
                            fluidRow(DT::dataTableOutput(outputId = 'table'))
                             
                             ),
                             
                             ## Next Page(Tab Panel)
                             tabPanel('Youtube Analytica',value="Youtube",

                              fluidRow(column(6,
                                              
                                      helpText(h3('Top Most Occuring Channel on Starbuck: 
                                                  CNBC, TheEllenShow,BuzzFeed,Starbucks & ThreadBanger')),        
                                      DT::dataTableOutput(outputId="Youtube_MasterData")),
                                       
                                       column(4,
                                              helpText(h3('YouTube Video Statistics'),align='center'),
                                        
                                       fluidRow(textInput('video_ID',value='_FGUkxn5kZQ',label='paste video ID'),align='right'),
                                       fluidRow(plotOutput("video_stats")))),
                      
                              fluidRow(
                                column(4,
                                       fluidRow(helpText(h3('Popularity Ratio'))),
                                       fluidRow(plotOutput('video_Popularity_Ratio'))),
                                column(4,
                                       fluidRow(helpText(h3('Engagement Ratio'))),
                                       plotOutput('video_Engagement_Ratio')),
                                column(4,
                                       fluidRow(helpText(h3('Like-Dislike Ratio'))),
                                       plotOutput('video_dislike_Ratio')),
                                       fluidRow(uiOutput('source_url'))  
                                ),
                              
                              fluidRow(column(6,helpText(h2('Words Polarity of Selected Video ID'))),
                                       column(6,helpText(h2('YTD...'))))

                                )

                              )
                             
                           ))
        
        
                  
        
        
        

















