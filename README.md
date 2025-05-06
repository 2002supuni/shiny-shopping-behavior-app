# shiny-shopping-behavior-app
Interactive Shiny Dashboard for exploring consumer shopping behavior using demographic and transactional data. Includes multiple visualizations such as gender-wise distribution, review ratings, seasonal trends, and more.
# 🛍️ Shopping Behavior Data Explorer - Shiny App

This Shiny app provides an interactive dashboard for visualizing and analyzing consumer shopping behavior using a dataset containing demographic and transactional variables.

## 📊 Features

- **Dataset Overview**: Displays the structure and preview of the dataset.
- **Interactive Filtering**: Filter by age, review ratings, and product categories.
- **Dynamic Visualizations**:
  - Gender-wise Purchase Distribution (Bar Chart)
  - Review Rating vs Purchase Amount (Boxplot)
  - Purchase Frequency Over Time (Line Chart)
  - Seasonal Category Trends (Stacked Bar Chart)
  - Previous Purchases vs Shipping Type (Boxplot)

## 🗃️ Dataset

- File: `shopping_behavior_updated.csv`
- Key Variables:
  - `Customer ID`
  - `Age`, `Gender`
  - `Item Purchased`, `Category`
  - `Purchase Amount (USD)`
  - `Review Rating`
  - `Previous Purchases`
  - `Payment Method`, `Shipping Type`, `Season`
  
## 🛠️ Tech Stack

- **R**
- **Shiny**, **shinydashboard**
- **ggplot2**, **plotly**
- **dplyr**, **DT**

