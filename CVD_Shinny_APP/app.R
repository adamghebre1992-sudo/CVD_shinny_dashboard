library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)

# ===============================
# LOAD DATA
# ===============================

heart_clean <- read.csv("D:/Digital health project/CVD_Shinny_APP/CVD_cleaned_dataset.csv")
# UI
# ===============================

ui <- dashboardPage(
  
  dashboardHeader(title = "CVD Risk Dashboard"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Overview",
               tabName = "overview",
               icon = icon("dashboard")),
      
      menuItem("Risk Factor Explorer",
               tabName = "explorer",
               icon = icon("search")),
      
      menuItem("Clinical Factors",
               tabName = "clinical",
               icon = icon("heartbeat")),
      
      menuItem("Lifestyle Factors",
               tabName = "lifestyle",
               icon = icon("walking")),
      
      menuItem("Metabolic Factors",
               tabName = "metabolic",
               icon = icon("chart-line"))
    ),
    
    hr(),
    
    selectInput(
      "risk_filter",
      "Select Risk Level",
      choices = c("All",
                  unique(heart_clean$cvd_risk_level)),
      selected = "All"
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # ==================================
      # OVERVIEW
      # ==================================
      
      tabItem(
        
        tabName = "overview",
        
        fluidRow(
          
          valueBoxOutput("participants"),
          
          valueBoxOutput("highrisk"),
          
          valueBoxOutput("averageage")
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "CVD Risk Distribution",
            
            plotlyOutput("riskDist")
          )
          
        )
        
      ),
      
      # ==================================
      # EXPLORER
      # ==================================
      
      tabItem(
        
        tabName = "explorer",
        
        fluidRow(
          
          box(
            width = 6,
            title = "Age vs CVD Risk",
            
            plotlyOutput("agePlot")
          ),
          
          box(
            width = 6,
            title = "Sex vs CVD Risk",
            
            plotlyOutput("sexPlot")
          )
          
        )
        
      ),
      
      # ==================================
      # CLINICAL
      # ==================================
      
      tabItem(
        
        tabName = "clinical",
        
        fluidRow(
          
          box(
            width = 6,
            title = "BMI vs Risk",
            
            plotlyOutput("bmiPlot")
          ),
          
          box(
            width = 6,
            title = "Blood Pressure Category",
            
            plotlyOutput("bpPlot")
          )
          
        )
        
      ),
      
      # ==================================
      # LIFESTYLE
      # ==================================
      
      tabItem(
        
        tabName = "lifestyle",
        
        fluidRow(
          
          box(
            width = 6,
            title = "Physical Activity",
            
            plotlyOutput("activityPlot")
          ),
          
          box(
            width = 6,
            title = "Smoking Status",
            
            plotlyOutput("smokingPlot")
          )
          
        )
        
      ),
      
      # ==================================
      # METABOLIC
      # ==================================
      
      tabItem(
        
        tabName = "metabolic",
        
        fluidRow(
          
          box(
            width = 6,
            title = "Cholesterol vs Risk",
            
            plotlyOutput("cholPlot")
          ),
          
          box(
            width = 6,
            title = "Blood Sugar vs Risk",
            
            plotlyOutput("sugarPlot")
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Diabetes Status vs Risk",
            
            plotlyOutput("diabetesPlot")
          )
          
        )
        
      )
      
    )
  )
)

# ===============================
# SERVER
# ===============================

server <- function(input, output) {
  
  # FILTER DATA
  
  filtered_data <- reactive({
    
    if(input$risk_filter == "All"){
      
      heart_clean
      
    } else {
      
      heart_clean %>%
        filter(cvd_risk_level == input$risk_filter)
      
    }
    
  })
  
  # KPI CARDS
  
  output$participants <- renderValueBox({
    
    valueBox(
      nrow(filtered_data()),
      "Participants",
      icon = icon("users"),
      color = "blue"
    )
    
  })
  
  output$highrisk <- renderValueBox({
    
    valueBox(
      sum(filtered_data()$cvd_risk_level == "HIGH"),
      "High Risk",
      icon = icon("heartbeat"),
      color = "red"
    )
    
  })
  
  output$averageage <- renderValueBox({
    
    valueBox(
      round(mean(filtered_data()$age,
                 na.rm = TRUE),1),
      "Average Age",
      icon = icon("calendar"),
      color = "green"
    )
    
  })
  
  # RISK DISTRIBUTION
  
  output$riskDist <- renderPlotly({
    
    p <- ggplot(
      heart_clean,
      aes(
        x = cvd_risk_level,
        fill = cvd_risk_level
      )
    )+
      geom_bar()+
      scale_fill_manual(values=c(
        "HIGH"="red",
        "INTERMEDIARY"="orange",
        "LOW"="blue"
      ))+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # AGE
  
  output$agePlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = age,
        fill = cvd_risk_level
      )
    )+
      geom_histogram(
        bins = 20,
        alpha = 0.7
      )+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # SEX
  
  output$sexPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = sex,
        fill = cvd_risk_level
      )
    )+
      geom_bar(position="dodge")+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # BMI
  
  output$bmiPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = cvd_risk_level,
        y = bmi,
        fill = cvd_risk_level
      )
    )+
      geom_boxplot()+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # BLOOD PRESSURE
  
  output$bpPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = blood_pressure_category,
        fill = cvd_risk_level
      )
    )+
      geom_bar(position="dodge")+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ACTIVITY
  
  output$activityPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = physical_activity_level,
        fill = cvd_risk_level
      )
    )+
      geom_bar(position="dodge")+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # SMOKING
  
  output$smokingPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = smoking_status,
        fill = cvd_risk_level
      )
    )+
      geom_bar(position="dodge")+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # CHOLESTEROL
  
  output$cholPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = cvd_risk_level,
        y = total_cholesterol_mg_d_l,
        fill = cvd_risk_level
      )
    )+
      geom_boxplot()+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # BLOOD SUGAR
  
  output$sugarPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = cvd_risk_level,
        y = fasting_blood_sugar_mg_d_l,
        fill = cvd_risk_level
      )
    )+
      geom_boxplot()+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # DIABETES
  
  output$diabetesPlot <- renderPlotly({
    
    p <- ggplot(
      filtered_data(),
      aes(
        x = diabetes_status,
        fill = cvd_risk_level
      )
    )+
      geom_bar(position="dodge")+
      theme_minimal()
    
    ggplotly(p)
    
  })
  
}

shinyApp(ui, server)
   