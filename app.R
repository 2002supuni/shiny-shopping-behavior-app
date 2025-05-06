library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

# Load Data
shopping_data <- read.csv("C:\\Users\\Supuni Tharushika\\Documents\\App\\shopping_behavior_updated.csv", stringsAsFactors = FALSE)

#Check for Any Missing Values in the Entire Dataset
any(is.na(shopping_data))
#Count Missing Values in Each Column
colSums(is.na(shopping_data))
#Since there are no any missing values

# Ensure correct min/max calculation
min_age <- min(shopping_data$Age, na.rm = TRUE)
max_age <- max(shopping_data$Age, na.rm = TRUE)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    body { background-color: #ffffff; font-family: Arial, sans-serif; }
    .navbar { background: linear-gradient(to right, #7793f4, #77ddf4); }
    .content-box { background: #e4fbff; padding: 20px; border-radius: 10px; margin: 15px; } 
    .overview-box { background: #e4fbff; padding: 20px; border-radius: 10px; margin: 15px; } 
    .title-box { text-align: center; font-size: 32px; font-weight: bold; color: #000000; padding: 20px; 
                 background: linear-gradient(to right, #2331ee, #23d0ee); border-radius: 10px; }
    .sidebar { background-color: #d0e4e8; padding: 20px; border-radius: 10px; margin-bottom: 20px; }
    .plot-bg { background-color: rgba(255, 255, 255, 0.8); padding: 15px; border-radius: 10px; }
  "))
  ),
  
  fluidRow(column(12, div(class = "title-box", "Shopping Behavior Data Explorer"))),
  
  navbarPage("",
             tabPanel(HTML("<span style='font-size: 20px; font-weight: bold; color: #000000;'>Overview</span>"),
                      div(class = "overview-box", 
                          fluidRow(
                            column(8, 
                                   h3("Dataset Overview"),
                                   p("The Consumer Behavior and Shopping Habits Dataset provides valuable insights into customer shopping preferences, habits, and purchasing patterns."),
                                   h4("Key Variables:"),
                                   tags$ul(
                                     tags$li("Customer ID – Unique identifier for each customer."),
                                     tags$li("Age & Gender – Demographic details."),
                                     tags$li("Item Purchased & Category – Product details."),
                                     tags$li("Purchase Amount (USD) – Transaction value."),
                                     tags$li("Review Rating – Customer feedback."),
                                     tags$li("Previous Purchases – Purchase history."),
                                     tags$li("Payment Method – Preferred payment mode."),
                                     tags$li("Shipping Type – Delivery method used.")
                                   )
                            ),
                            column(4, tags$img(src = "1.PNG", width = "90%", height = "auto", 
                                               style = "border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);"))
                          )
                      ),
                      div(class = "content-box", h3("Dataset Preview"), DT::dataTableOutput("data_table"))
             ),
             
             tabPanel(HTML("<span style='font-size: 20px; font-weight: bold; color: #000000;'>Visualizations</span>"),
                      sidebarLayout(
                        sidebarPanel(
                          div(class = "sidebar",
                              h4("Select Age Range:"),
                              sliderInput("age_range", "Age Range:", min = min_age, max = max_age, value = c(min_age, max_age))
                          ),
                          div(class = "sidebar",
                              h4("Filter Review Ratings & Categories:"),
                              sliderInput("review_range", 
                                          label = "Select Review Rating Range:",
                                          min = min(shopping_data$Review.Rating, na.rm = TRUE),
                                          max = max(shopping_data$Review.Rating, na.rm = TRUE),
                                          value = c(min(shopping_data$Review.Rating, na.rm = TRUE)+1,  
                                                    max(shopping_data$Review.Rating, na.rm = TRUE)-1),
                                          step = 0.5),
                              checkboxGroupInput("category_filter", "Select Categories:", choices = unique(shopping_data$Category), selected = unique(shopping_data$Category))
                          ),
                          width = 3  # Reduced width from default 4 to 3
                        ),
                        
                        mainPanel(
                          # First row: Two graphs side by side
                          fluidRow(
                            column(5, div(class = "content-box plot-bg", h3("Gender-wise Purchase Distribution"), plotlyOutput("gender_bar_chart", height = "300px"),br(),uiOutput("gender_chart_description"))),
                            column(7, div(class = "content-box plot-bg", h3("Review Rating vs Purchase Amount"), plotlyOutput("review_boxplot", height = "300px"),br(),uiOutput("Review_Rating_description")))
                          ),
                          
                          # Second row: Single full-width graphs
                          fluidRow(
                            div(class = "content-box plot-bg", h3("Purchase Frequency Trend"), plotlyOutput("line_chart", height = "300px"),br(),uiOutput("line_chart_description"))),
                            div(class = "content-box plot-bg", h3("Stacked Bar Chart: Season vs Item Category"), plotlyOutput("stacked_bar_chart", height = "300px"),br(),uiOutput("Season_Category_description")),
                            div(class = "content-box plot-bg", h3("Previous Purchases vs Shipping Type"), plotlyOutput("boxplot_prev_purchases", height = "300px"),br(),uiOutput("Shipping_description"))
                          )
                        )
                        
                      )
             )
  )

# Server
server <- function(input, output) {
  
  # Filtered dataset based on Age Range
  filtered_data <- reactive({
    shopping_data %>% filter(Age >= input$age_range[1], Age <= input$age_range[2])
  })
  
  # Filter Review Ratings & Categories
  filtered_review_data <- reactive({
    shopping_data %>%
      filter(Review.Rating >= input$review_range[1], 
             Review.Rating <= input$review_range[2],
             Category %in% input$category_filter)
  })
  
  # Render Data Table
  output$data_table <- DT::renderDataTable({
    DT::datatable(shopping_data, options = list(pageLength = 10, scrollX = TRUE), class = "display nowrap") %>%
      formatStyle(columns = names(shopping_data), backgroundColor = "#b8e4f7")
  })
  
  # Gender-wise Purchase Distribution Bar Chart (Interactive with Hover Info)
  output$gender_bar_chart <- renderPlotly({
    gender_data <- filtered_data() %>%
      count(Gender)
    
    p <- ggplot(gender_data, aes(x = Gender, y = n, fill = Gender)) + 
      geom_bar(stat = "identity", width = 0.5) +
      theme_minimal() +
      labs(x = "Gender", y = "Number of Purchases", fill = "Gender") +
      scale_fill_manual(values = c("#3498db", "#e74c3c"))+
      theme(
        plot.background = element_rect(fill = "#edf7fa", color = NA),  # Light blue background
        panel.background = element_rect(fill = "#edf7fa", color = NA),  # Match panel background
        legend.background = element_rect(fill = "#edf7fa"),  # Background for legend
        legend.key = element_rect(fill = "#edf7fa")  # Background for legend keys
      )
    
    # Convert ggplot to interactive Plotly with hover tooltips
    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white")) %>%
      config(displayModeBar = FALSE)
  })
  # Add description below the gender-wise purchase distribution graph
  output$gender_chart_description <- renderUI({
    HTML("<p style='text-align:center; font-size:14px; color:#333; margin-top:5px;'>
        <em>This bar chart illustrates the number of purchases made by males and females. 
        It highlights which gender contributes more to the overall shopping activity.The chart dynamically updates based on the selected age range, reflecting how shopping behavior varies across different age groups.</em>
        </p>")
  })
  
  # Boxplot: Review Rating vs Purchase Amount
  output$review_boxplot <- renderPlotly({
    cleaned_data <- filtered_review_data() %>%
      mutate(
        Review.Rating = as.numeric(Review.Rating),
        Purchase.Amount..USD. = as.numeric(Purchase.Amount..USD.)  # Ensure correct column name
      ) %>%
      filter(!is.na(Review.Rating), !is.na(Purchase.Amount..USD.))
    
    # Debugging output
    print(head(cleaned_data))
    
    # If no data is available, prevent error
    if (nrow(cleaned_data) == 0) {
      showNotification("No data available for selected filters.", type = "error")
      return(NULL)
    }
    # Create the Boxplot
    p <- ggplot(cleaned_data, aes(x = as.factor(Review.Rating), y = Purchase.Amount..USD., fill = as.factor(Review.Rating))) +
      geom_boxplot(alpha = 0.7, outlier.color = "red") +
      theme_minimal() +
      labs(x = "Review Rating", y = "Purchase Amount (USD)", fill = "Review Rating") +
      scale_fill_brewer(palette = "Set3") +
      theme(
        plot.background = element_rect(fill = "#edf7fa", color = NA),  # Light blue background
        panel.background = element_rect(fill = "#edf7fa", color = NA),  # Match panel background
        legend.background = element_rect(fill = "#edf7fa"),  # Background for legend
        legend.key = element_rect(fill = "#edf7fa")  # Background for legend keys
      )
    
    ggplotly(p)
  })
  
  # Add description below Review Rating vs Purchase Amount Boxplot:
  output$Review_Rating_description <- renderUI({
    HTML("<p style='text-align:center; font-size:14px; color:#333; margin-top:5px;'>
        <em>This box plot visualizes how purchase amounts vary with different review ratings, showing trends in customer spending based on rating scores.The plot updates based on the selected review rating range and the selected categories checkbox, allowing for an analysis of purchase patterns across different rating levels and product categories.</em>
        </p>")
  })
  
  
  
  
  
  
  # Interactive Line Chart: Purchase Frequency Over Time
  output$line_chart <- renderPlotly({
    purchase_trend <- shopping_data %>%
      count(Frequency.of.Purchases)
    
    p <- ggplot(purchase_trend, aes(x = Frequency.of.Purchases, y = n, group = 1)) +
      geom_line(color = "#3b6978", size = 1.5) +
      geom_point(size = 3, color = "#204051") +
      theme_minimal() +
      labs(x = "Frequency", y = "Number of Customers")+
      theme(
        plot.background = element_rect(fill = "#edf7fa", color = NA),  # Light blue background
        panel.background = element_rect(fill = "#edf7fa", color = NA),  # Match panel background
        legend.background = element_rect(fill = "#edf7fa"),  # Background for legend
        legend.key = element_rect(fill = "#edf7fa")  # Background for legend keys
      )
    
    ggplotly(p)
  })
  
  # Add description below the Line Chart: Purchase Frequency Over Time
  output$line_chart_description <- renderUI({
    HTML("<p style='text-align:center; font-size:14px; color:#333; margin-top:5px;'>
        <em>This line chart shows how often customers make purchases over different time intervals. It helps identify the most common shopping frequency among consumers.</em>
        </p>")
  })
  
  # Stacked Bar Chart - Season vs Item Category
  output$stacked_bar_chart <- renderPlotly({
    season_data <- shopping_data %>%
      count(Season, Category)
    
    p <- ggplot(season_data, aes(x = Season, y = n, fill = Category)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      labs(x = "Season", y = "Number of Purchases", fill = "Category")+
      theme(
        plot.background = element_rect(fill = "#edf7fa", color = NA),  # Light blue background
        panel.background = element_rect(fill = "#edf7fa", color = NA),  # Match panel background
        legend.background = element_rect(fill = "#edf7fa"),  # Background for legend
        legend.key = element_rect(fill = "#edf7fa")  # Background for legend keys
      )
    
    ggplotly(p)
  })
  
  # Add description below Stacked Bar Chart - Season vs Item Category
  output$Season_Category_description <- renderUI({
    HTML("<p style='text-align:center; font-size:14px; color:#333; margin-top:5px;'>
        <em>This stacked bar chart illustrates the distribution of item categories purchased across different seasons. It highlights seasonal trends in shopping preferences.</em>
        </p>")
  })
  
  # Interactive Boxplot: Previous Purchases vs Shipping Type
  output$boxplot_prev_purchases <- renderPlotly({
    p <- ggplot(shopping_data, aes(x = Shipping.Type, y = Previous.Purchases, fill = Shipping.Type)) +
      geom_boxplot(outlier.color = "red", outlier.shape = 15, alpha = 0.5) +
      theme_minimal() +
      labs(x = "Shipping Type", y = "Number of Previous Purchases")+
      theme(
        plot.background = element_rect(fill = "#edf7fa", color = NA),  # Light blue background
        panel.background = element_rect(fill = "#edf7fa", color = NA),  # Match panel background
        legend.background = element_rect(fill = "#edf7fa"),  # Background for legend
        legend.key = element_rect(fill = "#edf7fa")  # Background for legend keys
      )
    
    ggplotly(p)
  })
  
  # Add description below Review Rating vs Purchase Amount Boxplot:
  output$Shipping_description <- renderUI({
    HTML("<p style='text-align:center; font-size:14px; color:#333; margin-top:5px;'>
        <em>This boxplots compare the number of previous purchases across different shipping types. It helps analyze how shipping preferences relate to customers' shopping behavior.</em>
        </p>")
  })
}

# Run App
shinyApp(ui, server)