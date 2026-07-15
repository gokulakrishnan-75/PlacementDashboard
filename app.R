library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)
library(plotly)

# Load Dataset
placement <- read.csv("placement.csv", header = TRUE)

# UI
ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "College Placement Dashboard"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      
      valueBoxOutput("studentsBox", width = 4),
      valueBoxOutput("placedBox", width = 4),
      valueBoxOutput("cgpaBox", width = 4)
      
    ),
    
    fluidRow(
      
      box(
        title = "Placement Status",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        plotOutput("placementPlot")
      ),
      
      box(
        title = "CGPA Distribution",
        width = 6,
        status = "success",
        solidHeader = TRUE,
        plotOutput("cgpaPlot")
      )
      
    ),
    
    fluidRow(
      
      box(
        title = "Placement Percentage",
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        plotOutput("piePlot")
      ),
      
      box(
        title = "CGPA vs IQ",
        width = 6,
        status = "info",
        solidHeader = TRUE,
        plotOutput("scatterPlot")
      )
      
    ),
    
    fluidRow(
      
      box(
        title = "Student Dataset",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        DTOutput("table")
      )
      
    )
    
  )
)

# Server
server <- function(input, output) {
  
  output$studentsBox <- renderValueBox({
    
    valueBox(
      value = nrow(placement),
      subtitle = "Total Students",
      icon = icon("users"),
      color = "blue"
    )
    
  })
  
  output$placedBox <- renderValueBox({
    
    valueBox(
      value = sum(placement$Placement == "Yes"),
      subtitle = "Placed Students",
      icon = icon("briefcase"),
      color = "green"
    )
    
  })
  
  output$cgpaBox <- renderValueBox({
    
    valueBox(
      value = round(mean(placement$CGPA),2),
      subtitle = "Average CGPA",
      icon = icon("graduation-cap"),
      color = "yellow"
    )
    
  })
  
  output$placementPlot <- renderPlot({
    
    ggplot(placement,
           aes(x = Placement,
               fill = Placement)) +
      geom_bar() +
      theme_minimal() +
      labs(
        title = "Placement Status",
        x = "Placement",
        y = "Number of Students"
      )
    
  })
  
  output$cgpaPlot <- renderPlot({
    
    ggplot(placement,
           aes(CGPA)) +
      geom_histogram(
        fill = "steelblue",
        color = "white",
        bins = 20
      ) +
      theme_minimal() +
      labs(title = "CGPA Distribution")
    
  })
  
  output$piePlot <- renderPlot({
    
    ggplot(placement,
           aes(x = "",
               fill = Placement)) +
      geom_bar(width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Placement Percentage")
    
  })
  
  output$scatterPlot <- renderPlot({
    
    ggplot(placement,
           aes(
             x = CGPA,
             y = IQ,
             color = Placement
           )) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "CGPA vs IQ")
    
  })
  
  output$table <- renderDT({
    
    datatable(
      placement,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
}

# Run App
shinyApp(ui, server)