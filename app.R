
# =====================================================
# AUTOMOBILE DATA ANALYSIS AND VISUALIZATION DASHBOARD
# R PROGRAMMING MINI PROJECT
# =====================================================

# 1. LOAD PACKAGES

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(bslib)
library(tidyr)


# 2. LOAD DATASET

data("mpg", package = "ggplot2")

air <- mpg


# 3. DASHBOARD UI

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  titlePanel("AUTOMOBILE ANALYTICS DASHBOARD"),
  
  p(
    "Explore vehicle characteristics and fuel efficiency",
    style = "color: #526579; font-size: 16px;"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      selectInput(
        "manufacturer",
        "Select Manufacturer:",
        choices = c(
          "All Manufacturers",
          sort(unique(air$manufacturer))
        ),
        selected = "All Manufacturers"
      ),
      
      selectInput(
        "class",
        "Select Vehicle Class:",
        choices = c(
          "All Classes",
          sort(unique(air$class))
        ),
        selected = "All Classes"
      ),
      
      hr(),
      
      h5("Dashboard Information"),
      
      p("Dataset: Automobile MPG"),
      p("Source: ggplot2 built-in dataset"),
      p("Total original records: 234"),
      
      hr(),
      
      downloadButton(
        "download_data",
        "Download Filtered Data",
        class = "btn-primary"
      )
      
    ),
    
    mainPanel(
      
      h3("Overview"),
      
      fluidRow(
        
        column(
          3,
          wellPanel(
            h5("Total Vehicles"),
            h2(
              textOutput("total"),
              style = "color: #2874A6;"
            )
          )
        ),
        
        column(
          3,
          wellPanel(
            h5("Average City MPG"),
            h2(
              textOutput("city"),
              style = "color: #148F77;"
            )
          )
        ),
        
        column(
          3,
          wellPanel(
            h5("Average Highway MPG"),
            h2(
              textOutput("highway"),
              style = "color: #8E44AD;"
            )
          )
        ),
        
        column(
          3,
          wellPanel(
            h5("Manufacturers"),
            h2(
              textOutput("manufacturers"),
              style = "color: #D35400;"
            )
          )
        )
        
      ),
      
      hr(),
      
      h3("Fuel Efficiency Analysis"),
      
      fluidRow(
        
        column(
          6,
          plotlyOutput(
            "barplot",
            height = "350px"
          )
        ),
        
        column(
          6,
          plotlyOutput(
            "histogram",
            height = "350px"
          )
        )
        
      ),
      
      hr(),
      
      h3("Engine Performance Analysis"),
      
      plotlyOutput(
        "scatterplot",
        height = "400px"
      ),
      
      hr(),
      
      h3("Comparative Analysis"),
      
      fluidRow(
        
        column(
          6,
          plotlyOutput(
            "mileage_comparison",
            height = "400px"
          )
        ),
        
        column(
          6,
          plotOutput(
            "vehicle_distribution",
            height = "400px"
          )
        )
        
      ),
      
      hr(),
      
      h3("Key Insights"),
      
      wellPanel(
        textOutput("insights")
      ),
      
      hr(),
      
      h3("Automobile Data Records"),
      
      DTOutput("datatable")
      
    )
    
  )
  
)


# =====================================================
# 4. SERVER LOGIC
# =====================================================

server <- function(input, output, session) {
  
  
  # FILTER DATA
  
  filtered_data <- reactive({
    
    result <- air
    
    if (input$manufacturer != "All Manufacturers") {
      
      result <- result %>%
        filter(manufacturer == input$manufacturer)
      
    }
    
    if (input$class != "All Classes") {
      
      result <- result %>%
        filter(class == input$class)
      
    }
    
    result
    
  })
  
  
  # CHECK FILTERED DATA
  
  check_data <- function() {
    
    df <- filtered_data()
    
    validate(
      need(
        nrow(df) > 0,
        "No records found. Please change your filters."
      )
    )
    
    df
    
  }
  
  
  # KPI 1: TOTAL VEHICLES
  
  output$total <- renderText({
    
    nrow(filtered_data())
    
  })
  
  
  # KPI 2: AVERAGE CITY MPG
  
  output$city <- renderText({
    
    df <- check_data()
    
    round(mean(df$cty), 2)
    
  })
  
  
  # KPI 3: AVERAGE HIGHWAY MPG
  
  output$highway <- renderText({
    
    df <- check_data()
    
    round(mean(df$hwy), 2)
    
  })
  
  
  # KPI 4: MANUFACTURERS
  
  output$manufacturers <- renderText({
    
    df <- check_data()
    
    n_distinct(df$manufacturer)
    
  })
  
  
  # =================================================
  # GRAPH 1: BAR CHART
  # =================================================
  
  output$barplot <- renderPlotly({
    
    df <- check_data()
    
    plot_data <- df %>%
      group_by(class) %>%
      summarise(
        average_mpg = mean(hwy),
        .groups = "drop"
      )
    
    p <- ggplot(
      plot_data,
      aes(
        x = reorder(class, average_mpg),
        y = average_mpg,
        fill = class
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        title = "Average Highway MPG by Vehicle Class",
        x = "Vehicle Class",
        y = "Average Highway MPG"
      ) +
      
      theme_minimal(base_size = 12) +
      
      theme(
        legend.position = "none"
      )
    
    ggplotly(p)
    
  })
  
  
  # =================================================
  # GRAPH 2: HISTOGRAM
  # =================================================
  
  output$histogram <- renderPlotly({
    
    df <- check_data()
    
    p <- ggplot(
      df,
      aes(x = hwy)
    ) +
      
      geom_histogram(
        binwidth = 3,
        fill = "#3498DB",
        color = "white"
      ) +
      
      labs(
        title = "Highway Mileage Distribution",
        x = "Highway MPG",
        y = "Number of Vehicles"
      ) +
      
      theme_minimal(base_size = 12)
    
    ggplotly(p)
    
  })
  
  
  # =================================================
  # GRAPH 3: SCATTER PLOT
  # =================================================
  
  output$scatterplot <- renderPlotly({
    
    df <- check_data()
    
    p <- ggplot(
      df,
      aes(
        x = displ,
        y = hwy,
        color = class
      )
    ) +
      
      geom_point(
        size = 3,
        alpha = 0.7
      ) +
      
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "black"
      ) +
      
      labs(
        title = "Engine Displacement vs Highway Mileage",
        x = "Engine Displacement (Litres)",
        y = "Highway MPG",
        color = "Vehicle Class"
      ) +
      
      theme_minimal(base_size = 12)
    
    ggplotly(p)
    
  })
  
  
  # =================================================
  # GRAPH 4: CITY VS HIGHWAY COMPARISON
  # =================================================
  
  output$mileage_comparison <- renderPlotly({
    
    df <- check_data()
    
    plot_data <- df %>%
      group_by(class) %>%
      summarise(
        City = mean(cty),
        Highway = mean(hwy),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = c(City, Highway),
        names_to = "Mileage_Type",
        values_to = "MPG"
      )
    
    p <- ggplot(
      plot_data,
      aes(
        x = class,
        y = MPG,
        fill = Mileage_Type
      )
    ) +
      
      geom_col(
        position = "dodge"
      ) +
      
      labs(
        title = "City vs Highway Mileage",
        x = "Vehicle Class",
        y = "Average MPG",
        fill = "Mileage Type"
      ) +
      
      theme_minimal(base_size = 11) +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
    ggplotly(p)
    
  })
  
  
  # =================================================
  # GRAPH 5: DONUT CHART
  # FIXED USING renderPlot()
  # =================================================
  
  output$vehicle_distribution <- renderPlot({
    
    df <- check_data()
    
    plot_data <- df %>%
      count(class)
    
    ggplot(
      plot_data,
      aes(
        x = 2,
        y = n,
        fill = class
      )
    ) +
      
      geom_col(
        width = 1,
        color = "white"
      ) +
      
      coord_polar(
        theta = "y"
      ) +
      
      xlim(0.5, 2.5) +
      
      labs(
        title = "Vehicle Category Distribution",
        fill = "Vehicle Class"
      ) +
      
      theme_void() +
      
      theme(
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        ),
        legend.position = "right"
      )
    
  })
  
  
  # =================================================
  # AUTOMATIC INSIGHTS
  # =================================================
  
  output$insights <- renderText({
    
    df <- check_data()
    
    best_class <- df %>%
      group_by(class) %>%
      summarise(
        avg = mean(hwy),
        .groups = "drop"
      ) %>%
      arrange(desc(avg)) %>%
      slice(1)
    
    engine_corr <- cor(
      df$displ,
      df$hwy
    )
    
    paste0(
      "Vehicle Class with Highest Average Highway MPG: ",
      best_class$class,
      " | Average Highway Mileage: ",
      round(best_class$avg, 2),
      " MPG",
      " | Correlation between Engine Size and Highway MPG: ",
      round(engine_corr, 2)
    )
    
  })
  
  
  # =================================================
  # DOWNLOAD FILTERED DATA
  # =================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      "Automobile_Filtered_Data.csv"
    },
    
    content = function(file) {
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
  
  # =================================================
  # INTERACTIVE DATA TABLE
  # =================================================
  
  output$datatable <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
    
  })
  
}


# =====================================================
# 5. RUN DASHBOARD
# =====================================================

shinyApp(
  ui = ui,
  server = server
)
