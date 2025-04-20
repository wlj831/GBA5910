#install.packages(c("shiny", "tidyverse", "shinydashboard", "plotly", "DT"))

library(shiny)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(DT)

# Grade mapping
grade_points <- c("A" = 4.0, "A-" = 3.7, "B+" = 3.3, "B" = 3.0, "B-" = 2.7,
                  "C+" = 2.3, "C" = 2.0, "C-" = 1.7, "D" = 1.0, "F" = 0)



ui <- fluidPage(
  titlePanel("GPA Calculator"),
  
  fluidRow(
    column(4,
           numericInput("num_courses", "Number of Courses", 3, min = 1, max = 10)
    )
  ),
  
  uiOutput("course_inputs"),
  
  hr(),
  h3("Calculated GPA:"),
  verbatimTextOutput("gpa")
)

server <- function(input, output, session) {
  
  # Dynamic UI for entering course data
  output$course_inputs <- renderUI({
    req(input$num_courses)
    n <- input$num_courses
    
    course_inputs <- lapply(1:n, function(i) {
      fluidRow(
        column(4, textInput(paste0("course", i), paste("Course", i), placeholder = "e.g. Bio 101")),
        column(4, numericInput(paste0("units", i), "Units", value = 3, min = 0, step = 0.5)),
        column(4, selectInput(paste0("grade", i), "Grade", choices = names(grade_points)))
      )
    })
    
    do.call(tagList, course_inputs)
  })
  
  # GPA calculation
  output$gpa <- renderText({
    req(input$num_courses)
    n <- input$num_courses
    
    total_points <- 0
    total_units <- 0
    
    for (i in 1:n) {
      units <- input[[paste0("units", i)]]
      grade <- input[[paste0("grade", i)]]
      
      if (!is.null(units) && !is.null(grade)) {
        total_points <- total_points + (units * grade_points[[grade]])
        total_units <- total_units + units
      }
    }
    
    if (total_units == 0) {
      return("Please enter valid units.")
    } else {
      round(total_points / total_units, 2)
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
