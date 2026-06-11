install.packages(c("shiny", "shinydashboard", "ggplot2", "plotly", "DT", "dplyr", "readr"))
path <- "."

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(readr)

# Load data
steam <- read_csv("steam.csv")

# Clean data
steam <- steam %>%
  filter(!is.na(positive_ratings), !is.na(price)) %>%
  mutate(
    total_ratings = positive_ratings + negative_ratings,
    rating_pct = ifelse(total_ratings > 0, round(positive_ratings / total_ratings * 100, 1), NA),
    release_year = as.integer(substr(release_date, 1, 4)),
    price = as.numeric(price)
  ) %>%
  filter(total_ratings >= 10, release_year >= 2000, release_year <= 2024)

# Split genres
genre_counts <- steam %>%
  mutate(genre = strsplit(as.character(genres), ";")) %>%
  tidyr::unnest(genre) %>%
  count(genre, sort = TRUE) %>%
  head(15)

# ---- UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "🎮 Steam Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Games Explorer", tabName = "explorer", icon = icon("gamepad")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    hr(),
    sliderInput("year_range", "Release Year:",
                min = 2000, max = 2024, value = c(2010, 2024), sep = ""),
    sliderInput("price_range", "Max Price ($):",
                min = 0, max = 60, value = 30),
    sliderInput("min_rating", "Min Rating (%):",
                min = 0, max = 100, value = 50)
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-black .main-header .logo { background-color: #1b2838; }
      .skin-black .main-header .navbar { background-color: #1b2838; }
      .skin-black .main-sidebar { background-color: #1b2838; }
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius: 8px; }
      .info-box { border-radius: 8px; }
    "))),
    tabItems(
      
      # --- OVERVIEW TAB ---
      tabItem(tabName = "overview",
              fluidRow(
                infoBoxOutput("total_games"),
                infoBoxOutput("avg_price"),
                infoBoxOutput("avg_rating")
              ),
              fluidRow(
                box(title = "Games Released Per Year", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("releases_chart")),
                box(title = "Top 15 Genres", status = "primary",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("genre_chart"))
              ),
              fluidRow(
                box(title = "Price vs Rating", status = "info",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("scatter_chart")),
                box(title = "Price Distribution", status = "info",
                    solidHeader = TRUE, width = 6,
                    plotlyOutput("price_hist"))
              )
      ),
      
      # --- EXPLORER TAB ---
      tabItem(tabName = "explorer",
              fluidRow(
                box(title = "Game Table — click a row to see details",
                    status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("game_table"))
              ),
              fluidRow(
                box(title = "Selected Game Detail",
                    status = "success", solidHeader = TRUE, width = 12,
                    uiOutput("game_detail"))
              )
      ),
      
      # --- ABOUT TAB ---
      tabItem(tabName = "about",
              box(title = "About this Dashboard", status = "primary",
                  solidHeader = TRUE, width = 12,
                  h4("🎮 Steam Game Analytics Dashboard"),
                  p("This dashboard explores the Steam game catalog using data from Kaggle."),
                  p("It was built with R Shiny as part of a data visualization course project."),
                  h4("Dataset"),
                  p("Source: Kaggle — Steam Store Games dataset (~27,000 games)"),
                  h4("Features"),
                  tags$ul(
                    tags$li("Filter games by year, price, and rating"),
                    tags$li("Explore genre distributions"),
                    tags$li("Analyze price vs rating relationships"),
                    tags$li("Browse and select individual games for details")
                  ),
                  h4("Built with"),
                  p("R, Shiny, Plotly, DT, ggplot2, dplyr")
              )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Filtered data
  filtered <- reactive({
    steam %>%
      filter(
        release_year >= input$year_range[1],
        release_year <= input$year_range[2],
        price <= input$price_range,
        rating_pct >= input$min_rating
      )
  })
  
  # Info boxes
  output$total_games <- renderInfoBox({
    infoBox("Total Games", nrow(filtered()), icon = icon("gamepad"), color = "blue")
  })
  output$avg_price <- renderInfoBox({
    infoBox("Avg Price", paste0("$", round(mean(filtered()$price, na.rm=TRUE), 2)),
            icon = icon("dollar-sign"), color = "green")
  })
  output$avg_rating <- renderInfoBox({
    infoBox("Avg Rating", paste0(round(mean(filtered()$rating_pct, na.rm=TRUE), 1), "%"),
            icon = icon("star"), color = "yellow")
  })
  
  # Releases per year
  output$releases_chart <- renderPlotly({
    d <- filtered() %>% count(release_year)
    plot_ly(d, x = ~release_year, y = ~n, type = "bar",
            marker = list(color = "#1b9aaa")) %>%
      layout(xaxis = list(title = "Year"), yaxis = list(title = "Number of Games"),
             plot_bgcolor = "white", paper_bgcolor = "white")
  })
  
  # Genre chart
  output$genre_chart <- renderPlotly({
    plot_ly(genre_counts, x = ~n, y = ~reorder(genre, n), type = "bar",
            orientation = "h", marker = list(color = "#ef476f")) %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = ""),
             plot_bgcolor = "white", paper_bgcolor = "white")
  })
  
  # Scatter
  output$scatter_chart <- renderPlotly({
    d <- filtered() %>% filter(price > 0) %>% sample_n(min(500, nrow(.)))
    plot_ly(d, x = ~price, y = ~rating_pct, type = "scatter", mode = "markers",
            text = ~name, hoverinfo = "text+x+y",
            marker = list(color = "#06d6a0", opacity = 0.6, size = 6)) %>%
      layout(xaxis = list(title = "Price ($)"), yaxis = list(title = "Rating (%)"),
             plot_bgcolor = "white", paper_bgcolor = "white")
  })
  
  # Price histogram
  output$price_hist <- renderPlotly({
    d <- filtered() %>% filter(price > 0, price <= 60)
    plot_ly(d, x = ~price, type = "histogram", nbinsx = 30,
            marker = list(color = "#ffd166")) %>%
      layout(xaxis = list(title = "Price ($)"), yaxis = list(title = "Count"),
             plot_bgcolor = "white", paper_bgcolor = "white")
  })
  
  # Game table
  output$game_table <- renderDT({
    filtered() %>%
      select(name, release_year, price, rating_pct, total_ratings) %>%
      rename(Game = name, Year = release_year, Price = price,
             `Rating %` = rating_pct, `Total Ratings` = total_ratings) %>%
      datatable(selection = "single", rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Game detail on row click
  output$game_detail <- renderUI({
    req(input$game_table_rows_selected)
    row <- filtered()[input$game_table_rows_selected, ]
    div(
      h3(row$name),
      p(strong("Developer: "), row$developer),
      p(strong("Publisher: "), row$publisher),
      p(strong("Release Date: "), row$release_date),
      p(strong("Price: "), paste0("$", row$price)),
      p(strong("Rating: "), paste0(row$rating_pct, "% positive (", row$total_ratings, " total ratings)")),
      p(strong("Genres: "), row$genres),
      p(strong("Categories: "), row$categories)
    )
  })
}

shinyApp(ui, server)

