library(shiny)
library(shinydashboard)
library(listviewer)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("R-Quiz", tabName = "R-Quiz", icon = icon("book"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "R-Quiz",
      fluidRow(
        tabBox(
          id = "tabset1", width = 9,
          tabPanel("Student Information", 
                   h3("Student Information", align="center"),
                   textInput(inputId = "Name",
                             label = "Name",
                             placeholder = "Your Name..."),
                   textInput(inputId = "Surname",
                             label = "Surname",
                             placeholder = "Your Surname..."),
                   textInput(inputId = "StudentID",
                             label = "Student ID",
                             placeholder = "Your Student ID...")),
          tabPanel("Question 1",
                   h3("Question 1", align = "center"),
                   br(),
                   h4("Write an R function that takes a vector of real numbers and returns minimum, 
                                average and sum of the elements respectively as a vector object."),
                   br(),
                   h5("Do not use loop systems!"),
                   br(),
                   h4("Name your function as your surname, Surname_Q1, initial letter is capital"),
                   p("Eg. Oltulu_Q1"),
                   p("Try your functions with different appropriate values"),
                   br(),
                   h4("Reminder:"),
                   p("Do not write more than 30 characters on a line"),
                   p("If you have to write more than 30 characters, split them into 
                               multiple lines."),
                   br(),
                   textAreaInput(inputId = "Question1_code",
                                 label = "Code",
                                 placeholder = "Type your code here...",
                                 rows = 15,
                                 resize = "both"),
                   actionButton("Run_Code1", label = "Run Code"),
                   br(),
                   h5("Question 1 Output"),
                   br(),
                   verbatimTextOutput("Question1_output")),
          tabPanel("Question 2",
                   h3("Question 2", align = "center"),
                   br(),
                   h4("Write an R function that that given a vector of real numbers and an integer,
                                meaning your function must take 2 objects,
                                will return how many times the integer appears inside the vector."),
                   h4("Hint: Vectorization is the key!!"),
                   h4("Your function must return a single count value"),
                   br(),
                   h5("Do not use loop systems!"),
                   br(),
                   h4("Name your function as your surname, Surname_Q2, initial letter is capital"),
                   p("Eg. Oltulu_Q2"),
                   p("Try your functions with different appropriate values"),
                   br(),
                   h4("Reminder:"),
                   p("Do not write more than 30 characters on a line"),
                   p("if you have to write more than 30 characters, split them into 
                               multiple lines."),
                   br(),
                   textAreaInput(inputId = "Question2_code",
                                 label = "Code",
                                 placeholder = "Type your code here...",
                                 rows = 15,
                                 resize = "both"),
                   actionButton("Run_Code2", label = "Run Code"),
                   br(),
                   h5("Question 2 Output"),
                   br(),
                   verbatimTextOutput("Question2_output")),
          tabPanel("Question 3",
                   h3("Question 3", align = "center"),
                   br(),
                   h4("Write an R function that takes only a vector of values and returns 
                                a line graph. You have to add a custom title 
                                as 'your surname', y-axis label as 'y', 
                                and color to your plot."),
                   br(),
                   h5("Do not use loop systems!"),
                   br(),
                   h4("Name your function as your surname, Surname_Q3, initial letter is capital"),
                   p("Eg. Oltulu_Q3"),
                   p("Try your functions with different appropriate values"),
                   br(),
                   h4("Reminder:"),
                   p("Do not write more than 30 characters on a line"),
                   p("if you have to write more than 30 characters, split them into 
                               multiple lines."),
                   br(),
                   textAreaInput(inputId = "Question3_code",
                                 label = "Code",
                                 placeholder = "Type your code here...",
                                 rows = 15,
                                 resize = "both"),
                   actionButton("Run_Code3", label = "Run Code"),
                   br(),
                   h5("Question 3 Output"),
                   br(),
                   plotOutput("Question3_output")),
          tabPanel("DOWNLOAD",
                   h3("DOWNLOAD", align = "center"),
                   h3("A NameSurnameID.HTML document will be downloaded"),
                   br(),
                   br(),
                   downloadButton("btn", "Download"))
        )
      )
    )
  )
)

ui <- dashboardPage(dashboardHeader(title = "R - QUIZ - APP"),
                    sidebar,
                    body)

server <- function(input, output){
  
  toMatch <- c("while" , "for" , "repeat", "stop", "break")
  
  Question1 <- reactive({
    
    if(!grepl(paste(toMatch,collapse="|"), input$Question1_code)){
      eval(parse(text=input$Question1_code))
    }else{
      'no need to use a loop system'
    }
    
  })
  
  output$Question1_output <- renderText({
    input$Run_Code1
    isolate(paste(Question1()))
  })
  
  Question2 <- reactive({
    if(!grepl(paste(toMatch,collapse="|"), input$Question2_code)){
      eval(parse(text=input$Question2_code))
    }else{
      'no need to use a loop system'
    }
  })
  
  output$Question2_output <- renderText({
    input$Run_Code2
    isolate(paste(Question2()))
  })
  
  Question3 <- reactive({
    if(!grepl(paste(toMatch,collapse="|"), input$Question3_code)){
      eval(parse(text=input$Question3_code))
    }else{
      'no need to use a loop system'
    }
  })
  
  output$Question3_output <- renderPlot({
    input$Run_Code3
    isolate(paste(Question3()))
  })
  
  
  ##### Download Quiz #####
  
  output$btn <- downloadHandler(
    
    filename = paste(input$Name, input$Surname, input$StudentID, ".HTML",sep=""),
    content = function(file) {
      
      tempReport <- file.path(tempdir(),"R_Quiz_App.Rmd")
      
      file.copy("R_Quiz_App.Rmd", tempReport, overwrite = TRUE)
      library(rmarkdown)
      params <- list(Name = input$Name,
                     Surname = input$Surname,
                     StudentID = input$StudentID,
                     Question1_code = input$Question1_code,
                     Question2_code = input$Question2_code,
                     Question3_code = input$Question3_code
      )
      
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        clean = F, encoding = "utf-8",
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
}

shinyApp(ui, server)

#rsconnect::setAccountInfo(name='oltulu', 
#                          token='4CEA1BD06D97FCED5A4235508B6AF107', 
#                          secret='2lKuH3FKuzCsuuzzIQHVF9pFW1RXTRcX/f7AHKmr')
#rsconnect::deployApp()
