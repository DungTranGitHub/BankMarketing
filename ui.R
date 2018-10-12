#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bank Credit Marketing - Tall Machine Learning Group"),
  tabsetPanel(id ="dataTabSet",
              tabPanel("Data Analysis and Preprocessing",
                       fluidRow(
                         column(12,
                                selectInput("datasetOption", 
                                            label = "Please choose dataset option:",
                                            choices = list("Original", 
                                                           "Preprocessed"
                                            ),
                                            selected = "Original"))
                       ),
                       fluidRow(
                         column(12,
                                tabsetPanel(id="tabset",
                                            tabPanel("Data",
                                                     br(),
                                                     br(),
                                                     DT:: dataTableOutput("data_table")
                                            ),
                                            
                                            tabPanel("Summary",
                                                     fluidRow(
                                                       column(9,
                                                              br(),
                                                              verbatimTextOutput("summary"),
                                                              br()
                                                       ),
                                                       column(3,
                                                              br(),
                                                              strong("Correlation of numeric atributes:"),
                                                              plotOutput("correlationGraph"))
                                                     )
                                            ),
                                            
                                            tabPanel("Histogram",
                                                     br(),
                                                     selectInput("var", 
                                                                 "Choose an attriubte to display historgram",
                                                                 "placeholder"),
                                                     
                                                     strong("Attribute description:"),
                                                     textOutput("var_description"),
                                                     plotOutput("distPlot")
                                            )
                                )
                         )
                       
                       )
              ),
              tabPanel("Modelling",
                       br(),
                       tabsetPanel(id="tabset",
                                   
                                   tabPanel("Models",
                                            br(),
                                            plotOutput("modelPlot"),
                                            selectInput("model", 
                                                        label = "Choose model to show result details:",
                                                        choices = list("RPart", 
                                                                       "Naive Bayes",
                                                                       "Random Forest",
                                                                       "GLM Net",
                                                                       "GLM PCA",
                                                                       "GLM Manual Feature Selection",
                                                                       "Stack"),
                                                        selected = "RPart"
                                            ),
                                            br(),
                                            br(),
                                            DT:: dataTableOutput("model_details")
                                   ),
                                   
                                   tabPanel("ROI on Best Model",
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                sliderInput("call_cost","Cost Per Call:",
                                                            min = 0, max = 100,
                                                            value = 10),
                                                sliderInput("gain_success", "Gain Per Success:",
                                                            min = 0, max = 100,
                                                            value = 20),
                                                br(),
                                                br(),
                                                tableOutput("roi"),
                                                br(),
                                                br(),
                                                strong("Confusion Matrix and Statistics"),
                                                verbatimTextOutput("matrix")
                                              ),
                                              mainPanel(
                                                plotOutput("threshold"),
                                                br(),
                                                strong("AUC"),
                                                plotOutput("auc"),
                                                br(),
                                                strong("Important Attributes"),
                                                plotOutput("importantAttributes")
                                              )
                                            )
                                   )
                       )

                    
           )
  )
           
  )
)
