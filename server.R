## This file is where the server functionality is implemented.
library(shiny)
library(dplyr)
library(stringr)
oneWordWithStopWords <- readRDS("oneWordWithStopWordsMod.Rds")
twoWordWithStopWords <- readRDS("twoWordWithStopWordsMod.Rds")
threeWordWithStopWords <- readRDS("threeWordWithStopWordsMod.Rds")
source("kneserNeySmoothingAlgorithmFinal.R")
shinyServer(
  function(input, output)
  {
    prediction <- eventReactive(input$predictionButton, {kneserNeySmoothingAlgorithmFinal(input$inputString, oneWordWithStopWords, twoWordWithStopWords, threeWordWithStopWords)})    
    output$predictionWord <- renderPrint(prediction()$term[1])
    output$predictionWordProbability <- renderPrint(prediction()$probability[1])
    output$otherOptions <- renderPrint(if(nrow(prediction()) > 1)
                                          {
                                            prediction()[-1, ]
                                          })
  }
)