# ---------------- GLOBAL ----------------
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)

# Load data
data <- read.csv("RTA Dataset.csv")

# Ensure columns exist
data$Sex_of_driver <- as.character(data$Sex_of_driver)
data$Area_accident_occured <- as.character(data$Area_accident_occured)

# Create Severe / Not Severe column
data <- data %>%
  mutate(severe_binary = ifelse(
    Accident_severity %in% c("Serious Injury", "Fatal injury"),
    "Severe",
    "Not Severe"
  ))

# Numeric severity
data <- data %>%
  mutate(severity_score = case_when(
    Accident_severity == "Slight Injury" ~ 1,
    Accident_severity == "Serious Injury" ~ 2,
    Accident_severity == "Fatal injury" ~ 3
  ))

# ---------------- UI ----------------
ui <- fluidPage(
  titlePanel("Road Traffic Accident Severity Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Gender filter
      selectInput("gender_filter", "Select Gender:",
                  choices = c("All", unique(data$Sex_of_driver)),
                  selected = "All"),
      
      # Area filter
      selectInput("area_filter", "Select Area:",
                  choices = c("All", unique(data$Area_accident_occured)),
                  selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Severity by Area", plotlyOutput("plot_area", height = "500px")),
        tabPanel("Severe Injuries by Day", plotlyOutput("plot_day", height = "500px")),
        tabPanel("Vehicle Severity", plotlyOutput("plot_vehicle", height = "500px"))
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output) {
  
  # Reactive filtered data based on Gender and Area
  filtered_data <- reactive({
    df <- data
    if(input$gender_filter != "All") {
      df <- df %>% filter(Sex_of_driver == input$gender_filter)
    }
    if(input$area_filter != "All") {
      df <- df %>% filter(Area_accident_occured == input$area_filter)
    }
    df
  })
  
  # Plot 1: Severity by Area
  output$plot_area <- renderPlotly({
    summary1 <- filtered_data() %>%
      filter(!is.na(Area_accident_occured), !is.na(Accident_severity)) %>%
      group_by(Area_accident_occured, Accident_severity) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(percent = n / sum(n))
    
    p1 <- ggplot(summary1, aes(
      x = Area_accident_occured,
      y = percent,
      fill = Accident_severity,
      text = paste(
        "Area:", Area_accident_occured,
        "<br>Severity:", Accident_severity,
        "<br>Cases:", n,
        "<br>Percent:", scales::percent(percent, accuracy = 0.1)
      )
    )) +
      geom_col() +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal()
    
    ggplotly(p1, tooltip = "text")
  })
  
  # Plot 2: Severe Injuries by Day
  output$plot_day <- renderPlotly({
    summary2 <- filtered_data() %>%
      filter(!is.na(Day_of_week), !is.na(Area_accident_occured)) %>%
      group_by(Day_of_week, Area_accident_occured) %>%
      summarise(total = n(), severe = sum(severe_binary == "Severe"), .groups = "drop") %>%
      mutate(percent_severe = severe / total)
    
    p2 <- ggplot(summary2, aes(
      x = Day_of_week,
      y = percent_severe,
      fill = Area_accident_occured,
      text = paste(
        "Day:", Day_of_week,
        "<br>Area:", Area_accident_occured,
        "<br>Percent Severe:", scales::percent(percent_severe, accuracy = 0.1)
      )
    )) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal()
    
    ggplotly(p2, tooltip = "text")
  })
  
  # Plot 3: Vehicle Severity
  output$plot_vehicle <- renderPlotly({
    summary3 <- filtered_data() %>%
      filter(!is.na(Type_of_vehicle)) %>%
      group_by(Type_of_vehicle) %>%
      summarise(mean_severity = mean(severity_score, na.rm = TRUE))
    
    p3 <- ggplot(summary3, aes(
      x = Type_of_vehicle,
      y = mean_severity,
      text = paste(
        "Vehicle:", Type_of_vehicle,
        "<br>Mean Severity:", round(mean_severity, 2)
      )
    )) +
      geom_line(group = 1) +
      geom_point(size = 3) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p3, tooltip = "text")
  })
  
}

# ---------------- RUN APP ----------------
shinyApp(ui = ui, server = server)
