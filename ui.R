## This file is where the user interface is defined.
library(shiny)
library(BH)
shinyUI
(
  fluidPage
  (
    titlePanel("Next word prediction application"),
    sidebarLayout
    (
      sidebarPanel
      (
        h2("Instructions"),
        p("Please type in the text which next word you want to predict and press the submit button to get the most likely following word."),
        p("Other highly likely options are shown but the most likely is the one presented first with the highest probability"),
        p("The algorithm used for the word prediction takes a couple of seconds."),
        textInput(inputId = "inputString", label = "Input text"),
        actionButton(inputId = "predictionButton", label = "Predict next word")
      ),
      mainPanel
      (
        h2("Results panel"),
        h3("The most likely prediction is:"),
        verbatimTextOutput(outputId = "predictionWord"),
        h3("The probability for the most likely word is:"),
        verbatimTextOutput(outputId = "predictionWordProbability"),
        h3("Other options are:"),
        verbatimTextOutput(outputId = "otherOptions")
      )
    )
  )
)




# shinyUI
# (fluidPage
#  (
#    titlePanel("Next word prediction application"),
#    fluidRow
#    (
#      column
#      (
#        3,
#        fluidRow
#        (
#          column
#          (
#            12,
#            h3("App purpose"),
#            p("This app can be used to conduct an exploratory analysis on the the miles per galon (mpg) variable 
#              on the mtcars set from R.", style="text-align:justify")
#            )
#          ),
#        fluidRow
#        (
#          column
#          (
#            12,
#            h3("App use"),
#            p("Choose the predictor values to filter the data accordingly. You will be able to seee exploratory graphs
#              based on your selection.", style="text-align:justify"),
#            p("If you just want to observe a pair plot for the whole dataset, choose the ",  strong("Pairs plot"), " option 
#              to the left of the submit button. If on the other hand you wish to observe the exploratory graphs choose the ",
#              strong("Exploratory plots"), " radio button.")
#            )
#        )
#        ),
#      column
#      (
#        9,
#        plotOutput(outputId = "plotResults", height = "600px")
#      )
#        ),
#    fluidRow
#    (
#      column
#      (
#        4,
#        radioButtons(inputId = "summarySelection", label = "Type of summary:", choices = list("Pairs plot" = 1, "Exploratory plots" = 2))
#      ),
#      column
#      (
#        4,
#        selectInput(inputId = "predictorSelectionInput", label = "Predictor variable", choices = list("Displacement" = 1, "Horsepower" = 2, "Rear axle ratio" = 3, "Weight" = 4, "1/4 mile time" = 5))
#      ),
#      column
#      (
#        4,
#        radioButtons(inputId = "groupDataByInput", label = "Group data by:", choices = list("Number of cylinders" = 1, "Number of forward gears" = 2, "Number of carburetors" = 3))
#      )
#    ),
#    fluidRow
#    (
#      column
#      (
#        6,
#        radioButtons(inputId = "conditionalQueryInput", label = "Filter data by:", choices = list("Transmission" = 1, "V/S" = 2))
#      ),
#      column
#      (
#        6,
#        conditionalPanel(condition = "input.conditionalQueryInput == 1", radioButtons(inputId = "amInput", label = "Transmission:", choices = list("Automatic" = 1, "Manual" = 2))),
#        conditionalPanel(condition = "input.conditionalQueryInput == 2", radioButtons(inputId = "vsInput", label = "V/S:", choices = list("0" = 1, "1" = 2)))
#      )
#    )
#        )
#  )
