install.packages(c("shiny", "shinydashboard", "plotly", "ggplot2", "datarium", 
                   "dplyr", "wordcloud", "tm", "RColorBrewer", "reshape2"))
install.packages(c("shinydashboardPlus", "dashboardthemes", "viridis"))
install.packages('ggcorrplot')
install.packages('shinyjs')
install.packages('cluster')
# Load packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(viridis)
library(ggcorrplot)
library(shinyjs)
library(cluster)

# Load marketing dataset
data("marketing", package = "datarium")

# Function to perform clustering
kmeans_clustering <- function(data, k = 3) {
  set.seed(123)
  kmeans_result <- kmeans(data, centers = k)
  data$cluster <- as.factor(kmeans_result$cluster)
  return(data)
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = span("📊 Enhanced Marketing Dashboard", style = "color: white")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualizations", tabName = "viz", icon = icon("chart-bar")),
      menuItem("Clustering", tabName = "clustering", icon = icon("cogs")),
      menuItem("Data Export", tabName = "export", icon = icon("download")),
      
      sliderInput("minSales", "Filter: Minimum Sales", min = 0, max = max(marketing$sales), value = 5),
      sliderInput("youtubeSpend", "Max YouTube Spend", min = 0, max = max(marketing$youtube), value = max(marketing$youtube)),
      sliderInput("facebookSpend", "Max Facebook Spend", min = 0, max = max(marketing$facebook), value = max(marketing$facebook)),
      sliderInput("newspaperSpend", "Max Newspaper Spend", min = 0, max = max(marketing$newspaper), value = max(marketing$newspaper)),
      sliderInput("obsRange", "Select Observation Range", min = 1, max = nrow(marketing), value = c(1, nrow(marketing))),
      
      selectInput("palette", "Choose Color Palette", 
                  choices = c("Dark2", "Set3", "Pastel1", "Spectral", "Viridis"), selected = "Dark2"),
      
      actionButton("downloadData", "Download Filtered Data", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    shinyjs::useShinyjs(),
    
    tabItems(
      tabItem(tabName = "viz",
              fluidRow(
                # Dashboard title and statistics display
                box(title = "📊 Dashboard Statistics", width = 12, status = "primary", solidHeader = TRUE, 
                    valueBoxOutput("avgSalesBox"), valueBoxOutput("totalSpendBox"), valueBoxOutput("maxFacebookBox"))
              ),
              
              fluidRow(
                # Add 3 charts in a row
                box(title = "Ad Spend vs Sales (Scatterplot)", width = 4, status = "primary", solidHeader = TRUE, plotlyOutput("scatterPlot")),
                box(title = "3D Plot: Youtube, Facebook vs Sales", width = 4, status = "info", solidHeader = TRUE, plotlyOutput("plot3D")),
                box(title = "Line Chart: Spend Over Observations", width = 4, status = "warning", solidHeader = TRUE, plotlyOutput("lineChart"))
              ),
              
              fluidRow(
                # Add 3 charts in a row
                box(title = "Bar Chart: Stacked Ad Spend", width = 4, status = "success", solidHeader = TRUE, plotlyOutput("barChart")),
                box(title = "Pie Chart", width = 4, status = "danger", solidHeader = TRUE, plotlyOutput("pieChart")),
                box(title = "Donut Chart", width = 4, status = "purple", solidHeader = TRUE, plotlyOutput("donutChart"))
              ),
              
              fluidRow(
                # Add 3 charts in a row
                box(title = "Heatmap: Variable Correlations", width = 4, status = "info", solidHeader = TRUE, plotlyOutput("heatMap")),
                box(title = "Word Cloud of Keywords", width = 4, status = "primary", solidHeader = TRUE, plotOutput("wordCloud")),
                box(title = "Clustered Ad Spend vs Sales", width = 4, status = "primary", solidHeader = TRUE, plotlyOutput("clusteredScatter"))
              )
      ),
      
      tabItem(tabName = "clustering",
              fluidRow(
                # Add 2 charts in a row
                box(title = "Radar Chart per Cluster", width = 6, status = "info", solidHeader = TRUE, plotlyOutput("radarChart")),
                box(title = "Clustered Ad Spend vs Sales", width = 6, status = "primary", solidHeader = TRUE, plotlyOutput("clusteredScatter"))
              )
      ),
      
      tabItem(tabName = "export",
              fluidRow(
                box(title = "Download Filtered Data", width = 12, status = "success", solidHeader = TRUE, 
                    downloadButton("downloadDataCSV", "Download CSV"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    marketing %>%
      filter(
        sales >= input$minSales,
        youtube <= input$youtubeSpend,
        facebook <= input$facebookSpend,
        newspaper <= input$newspaperSpend
      ) %>%
      slice(input$obsRange[1]:input$obsRange[2])
  })
  
  # Clustering Data
  clustered_data <- reactive({
    data <- filtered_data()[, c("youtube", "facebook", "newspaper", "sales")]
    kmeans_clustering(data)
  })
  
  # Statistics display
  output$avgSalesBox <- renderValueBox({
    valueBox(round(mean(filtered_data()$sales), 2), "📈 Average Sales", icon = icon("chart-line"), color = "blue")
  })
  
  output$totalSpendBox <- renderValueBox({
    valueBox(sum(rowSums(filtered_data()[, 1:3])), "💸 Total Ad Spend", icon = icon("money-bill"), color = "green")
  })
  
  output$maxFacebookBox <- renderValueBox({
    valueBox(max(filtered_data()$facebook), "📘 Max Facebook Spend", icon = icon("facebook"), color = "purple")
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = youtube + facebook + newspaper, y = sales)) +
      geom_point(color = "darkorange", size = 3) +
      labs(x = "Total Ad Spend", y = "Sales") +
      theme_minimal()
    ggplotly(p)
  })
  
  # 3D Plot
  output$plot3D <- renderPlotly({
    plot_ly(filtered_data(), x = ~youtube, y = ~facebook, z = ~sales,
            type = "scatter3d", mode = "markers",
            marker = list(size = 5, color = ~sales, colorscale = 'Viridis')) %>%
      layout(title = "3D Spend vs Sales")
  })
  
  # Line Chart
  output$lineChart <- renderPlotly({
    df <- filtered_data() %>% mutate(obs = row_number())
    df_long <- pivot_longer(df, cols = c(youtube, facebook, newspaper), names_to = "Platform", values_to = "Spend")
    
    p <- ggplot(df_long, aes(x = obs, y = Spend, color = Platform)) +
      geom_line(size = 1.2) +
      labs(title = "Ad Spend Trend", x = "Observation", y = "Spend") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Bar Chart
  output$barChart <- renderPlotly({
    df <- filtered_data() %>% mutate(obs = row_number())
    df_long <- pivot_longer(df, cols = c(youtube, facebook, newspaper), names_to = "Platform", values_to = "Spend")
    
    p <- ggplot(df_long, aes(x = obs, y = Spend, fill = Platform)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_brewer(palette = input$palette)
    
    ggplotly(p)
  })
  
  # Pie Chart
  output$pieChart <- renderPlotly({
    spend <- colSums(filtered_data()[, 1:3])
    plot_ly(labels = names(spend), values = spend, type = "pie",
            marker = list(colors = brewer.pal(3, input$palette)),
            textinfo = "label+percent") %>%
      layout(title = "Pie Chart: Spend Share")
  })
  
  # Donut Chart
  output$donutChart <- renderPlotly({
    spend <- colSums(filtered_data()[, 1:3])
    plot_ly(labels = names(spend), values = spend, type = "pie", hole = 0.5,
            marker = list(colors = brewer.pal(3, input$palette)),
            textinfo = "label+percent") %>%
      layout(title = "Donut Chart: Spend Share")
  })
  
  # Heatmap
  output$heatMap <- renderPlotly({
    corr <- cor(filtered_data())
    plot_ly(x = colnames(corr), y = rownames(corr), z = corr, type = "heatmap",
            colorscale = "Viridis", showscale = TRUE) %>%
      layout(title = "Correlation Heatmap")
  })
  
  # Word Cloud
  output$wordCloud <- renderPlot({
    words <- c("YouTube", "Facebook", "Newspaper", "Sales", "Spend", "Ad", "Clicks", "ROI")
    freqs <- c(50, 40, 35, 30, 25, 20, 18, 15)
    wordcloud(words = words, freq = freqs, colors = brewer.pal(8, input$palette), random.order = FALSE)
  })
  
  # Clustered Scatter Plot
  output$clusteredScatter <- renderPlotly({
    p <- ggplot(clustered_data(), aes(x = youtube + facebook + newspaper, y = sales, color = cluster)) +
      geom_point(size = 3) +
      labs(x = "Total Ad Spend", y = "Sales", color = "Cluster") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Radar Chart for Clustered Data
  output$radarChart <- renderPlotly({
    df <- clustered_data() %>% group_by(cluster) %>% summarise(
      youtube = mean(youtube), facebook = mean(facebook), newspaper = mean(newspaper)
    )
    
    fig <- plot_ly(type = 'scatterpolar', mode = 'markers+lines', fill = 'toself') %>%
      add_trace(r = df$youtube, theta = c("YouTube", "Facebook", "Newspaper"), name = 'Cluster 1') %>%
      layout(polar = list(radialaxis = list(visible = TRUE)))
    
    fig
  })
  
  # Download filtered data
  output$downloadDataCSV <- downloadHandler(
    filename = function() { paste("filtered_data.csv") },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

shinyApp(ui, server)
