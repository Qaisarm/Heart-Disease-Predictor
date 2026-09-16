##################################################################
### Rshiny App Code
### Group 2
### HI Enthusiasts
###
##################################################################

rm(list = ls())
options(shiny.sanitize.errors = FALSE)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

my_username <- "doctor"
my_password <- "abc"


##################################################################
###########################/UI.R/#################################
##################################################################
# UI for Login Page 
ui1 <- function(){
  
  tagList(
    
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  h5("Hint: doctor"),
                  passwordInput("passwd", "Password"),
                  h5("Hint: abc"),
                  br(),
                  actionButton("Login", "Log in"),
                  verbatimTextOutput("dataInfo")
        )
    ),
    tags$style(type="text/css", "#login {font-size:24px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}"),
    div( h1(img(src="p2.png"),"Tibbna CDS"))
  
  )}
# UI for patirnt data input and result page 

ui2 <- function(){tagList(
  
  # Application title
  titlePanel(title=div(img(src="p2.png"), "Tibbna CDS")),
  ui <- fluidPage(
    fluidRow(
      column(12,
             "",
             fluidRow(
               column(6,
                      fluidRow(
                        column(6,
                               sliderInput("age",label = ("Enter Your Age"),min = 1,max = 120,value = 42)
                               
                               
                        ),
                        column(6, 
                               selectInput("sex", label= ("Enter Your Gender"), selectize = TRUE, choices = c("Female" = "0", "Male" = "1")),
                               selected = "1",helpText("1=male,0=female")
                        )
                      ),
                      
                        
                        fluidRow(
                          column(6,
                                 selectInput("cp",label = ("Chest Pain Type:"),selectize = TRUE, choices = c("Typical Angina" = "1", "Atypical Angina" = "2", "Non Anginal" = "3", "Asymptotic" = "4")), 
                                 helpText("1. Typical Angina   2. Atypical Angina    3.Non Anginal   4. Asymptotic")
                                 
                          ),
                          column(6, sliderInput("trestbps", label = ("Resting Blood Pressure"),min = 0, max = 300,value = 120),
                                 helpText("Range=0-300 mm/Hg")
                                 
                          )
                        ),
                      
                      
                      fluidRow(
                        column(6, 
                               
                               sliderInput("chol", label = ("Serum Cholestrol"),min = 0, max = 700,value = 200), 
                               helpText("Range=0-700 mg/dL")
                               ),
                        column(6,
                               selectInput("fbs", label = ("Fasting Blood Sugar"), selectize = TRUE, choices = c("<120mg/dl" = "0", ">120mg/dl" = "1")), 
                               helpText(" >120mg/dl -> 1; <120mg/dl -> 0")
                        )
                      ),
                
                      fluidRow(
                        column(6,
                               selectInput("restecg", label =("Resting ECG result"), selectize = TRUE, choices = c("Normal" = "0", "ST-T wave abnormality" = "1", "Ventricular hypertrophy" = "2")), 
                               helpText("0: normal, 1:having ST-T wave abnormality , 2: showing ventricular hypertrophy ")),
                        column(6, 
                               sliderInput("thalach", label =("Max Heart Rate Achieved"), min = 0, max = 250,value = 80)
                        )
                      ),
                      
                      fluidRow(
                        column(6, 
                               selectInput("exang", label =("Exercise induced angina"),  selectize = TRUE, choices = c("No" = "0", "Yes" = "1")), 
                               helpText("0 -> no, 1 -> yes")),
                        column(6,
                               textInput("oldpeak", label =("ST depression induced due to exercise"),value = "0"), helpText("Range=-3-6.2"))
                      ),
                      
               column(width = 6,
                      
                      actionButton("predict", "Predict"))
             ),
             column(6, 
                    tabsetPanel(type = "tabs",
                                # Rsults Tab
                                tabPanel("Results", h3(textOutput("pred1")), p(textOutput(outputId = "PPN")),
                                         p(textOutput(outputId = "age")),
                                         p(textOutput(outputId = "sex")), p(textOutput(outputId = "cp")),
                                         p(textOutput(outputId = "trestbps")),p(textOutput(outputId = "chol")),
                                         p(textOutput(outputId = "fbs")),p(textOutput(outputId = "restecg")),
                                         p(textOutput(outputId = "thalach")),p(textOutput(outputId = "exang")),
                                         p(textOutput(outputId = "oldpeak"))
                                         ),
                                
                                # Instructions Tab
                                tabPanel("Instruction",
                                         p(" "),
                                         p("This web application calculates the whether you have Heart Disease based on ten variables."),
                                         p("Just change the ten values and see the probability value changes correspondingly."))
                                
                    ) )
             )
    
  )
    )
  )
)}

header <- dashboardHeader(title = "Tibbna CDS",  dropdownMenu(
))
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  tags$head(tags$style("#dataInfo{color: red")),
  htmlOutput("page")
)

ui <- dashboardPage(header, sidebar, body)

######################################################################
###########################/server.R/#################################
######################################################################

# Classifer
source('naivebayes.R')

server = (function(input, output,session) {
# Logic for Login Page  
  Logged <- FALSE
  Security <- TRUE
  
  USER <- reactiveValues(Logged = Logged)
  SEC <- reactiveValues(Security = Security)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(my_username == Username & my_password == Password) {
            USER$Logged <- TRUE
          } else {SEC$Security <- FALSE}
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {output$page <- renderUI({ui1()})}
    if (USER$Logged == TRUE) {output$page <- renderUI({ui2()})}
  })
  
  observe({
    output$dataInfo <- renderText({
      if (SEC$Security) {""}
      else {"Your username or password is not correct"}
    })
  })

    set.seed(1)
    predictevent <- eventReactive(input$predict, {
      df <- data.frame('age' = as.integer(input$age),
                       'sex' = factor(input$sex, levels = c("0", "1")),
                       'cp' = factor(input$cp, levels = c("1", "2", "3", "4")),
                       'trestbps' = as.integer(as.character(input$trestbps)),
                       'chol' = as.integer(as.character(input$chol)),
                       'fbs' = factor(input$fbs, levels = c("0", "1")),
                       'restecg' = factor(input$restecg, levels = c("0", "1", "2")),
                       'thalach' = as.integer(as.character(input$thalach)),
                       'exang' = factor(input$exang, levels = c("0", "1")),
                       'oldpeak' = as.numeric(as.character(input$oldpeak))
      )
      prediction <- tryCatch(
        predict(naive_model, df),
        error = function(e) paste("PREDICTION ERROR:", conditionMessage(e))
      )
      
      prediction
    })
    
    # Prediction Logic
    
    observeEvent(input$predict, {
      pred <- as.character(predictevent())
      if (identical(pred, "1")) {
        showModal(modalDialog(
          title = "Prediction Result",
          div(style = "background-color: #f8d7da; padding: 20px; border-radius: 6px;",
              h3("The patient has a heart disease", style = "color: #a94442; margin: 0;")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else if (grepl("PREDICTION ERROR", pred)) {
        showModal(modalDialog(
          title = "Prediction Result",
          div(style = "background-color: #f8d7da; padding: 20px; border-radius: 6px;",
              p(pred, style = "color: #a94442; margin: 0;")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else {
        showModal(modalDialog(
          title = "Prediction Result",
          div(style = "background-color: #d4edda; padding: 20px; border-radius: 6px;",
              h3("The patient don't have a heart disease", style = "color: #155724; margin: 0;")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })
    
    output$pred1 <- renderText({
      pred <- as.character(predictevent())
      if (identical(pred, "1")) {
        "The patient has a heart disease"
      } else if (grepl("PREDICTION ERROR", pred)) {
        pred
      } else {
        "The patient don't have a heart disease"
      }
    }
    )
    # OutPut Text Code
    output$age <- renderText({paste("The heart disease prediction of patient of age = ", input$age, "Years")
    })
    output$sex <- renderText({paste("Gender = ", input$sex, "     (1 = Male, 0 = Female)")
    })
    output$cp <- renderText({paste("Chest Pain Type = ", input$cp, "   (1= Typical Angina   2= Atypical Angina    3=Non Anginal   4= Asymptotic)")
    })
    output$trestbps <- renderText({paste("Resting Blood Pressure = ", input$trestbps, "mm/Hg")
    })
    output$chol <- renderText({paste("Serum Cholestrol = ", input$chol, "mg/dL")
    })
    output$fbs<- renderText({paste("Fasting Blood Sugar = ", input$fbs, 
                                   "(>120mg/dl = 1; <120mg/dl = 0)")
    })
    output$restecg <- renderText({paste("Resting ECG result = ", input$restecg,
                                        "(0 = normal, 1 = having ST-T wave abnormality , 2 = showing ventricular hypertrophy)")
    })
    output$thalach <- renderText({paste("Max Heart Rate Achieved = ", input$thalach, "bpm")
    })
    output$exang <- renderText({paste("Exercise induced angina = ", input$exang, "    (0 = no, 1 = yes)")
    })
    output$oldpeak <- renderText({paste("ST depression induced due to exercise = ", input$oldpeak)
    })
    
  
})


# Run the application 
shinyApp(ui = ui, server = server)

