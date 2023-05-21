library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)
library(geosphere)
library(TSP)
library(ggplot2)
library(plotly)
library(gridExtra)
library(DT)
library(leaflet.providers)
data <- read.csv("data/cities_data.csv")

ui <-  fluidPage(
  navbarPage(title = div("The Traveling Salesman Problem for Polish cities", style = "text-align:center; font-family: Candara; margin-left:auto;margin-right:auto;"),
             
  tags$head(
  tags$style(HTML("
      body {
        background-color: #dbebf0; 
        font-family:Verdana,sans-serif;
        font-size:115%;
      }
      .navbar{
        background-color:#306578;
        height:75px;
        width:100%;
        text-align:center;
        border-bottom:2px solid #222222;
      }
      .navbar-brand{
          color:#ffffff!important; 
          line-height: 40px!important;
          justify-content: center; 
          align-items: center;
          font-size:26px!important;
      }
      .navbar-nav>li>a {
          background-color:#306578!important;
      }
      #total_distance_output{
          font-size:26px;
          text-align:center;
          font-family:Verdana,sans-serif;
          font-weight:bold;
          
      }
      #dt_sidebar {
          background-color: #2c5c6d;
          color:#ffffff!important;
          height:85vh;
          border:none!important;
          border-right:1px solid #eeeeee!important;
      }
      #distance_plot{
          margin-bottom:20px;
      }
      #map{
          margin-bottom:40px;
      }
      #time_plot{
          margin-bottom:20px;
      }
      
    "))
),
  #titlePanel("The Traveling Salesman Problem for Polish cities"),
  sidebarLayout(
    sidebarPanel(id = "dt_sidebar", style = "width: 85%; height 700px!important;",
      sliderInput("num_cities", "Select cities (ranked by population):", min = 2, max = 66,value = c(2, 66), step = 1, width = "85%"),
      selectInput("start_city", "Starting city:", choices = c("None", data$Nazwa), width = "85%"),
      selectInput("end_city", "End city:", choices = c("None", data$Nazwa), width = "85%"),
      selectInput("weight_column", "Weight column:", choices = c("None", colnames(data)[2:(ncol(data)-2)]), width = "85%"),
      fluidRow(
         column(6,  selectInput("method", "TSP calculation method:", choices = c("farthest_insertion", "nearest_insertion"))),
         column(4, numericInput("iterations", "Iterations:", value = 50, min = 1, max = 200)),
      ),
      actionButton("generate_button", "Generate route")
    ),
    mainPanel(id = "mainpanel",
              tabsetPanel(id = "tabesetPanel",
                          tabPanel("Interative Map", style = "padding:20px;",
                                  leafletOutput("map", height = "600px"),
                                  textOutput("total_distance_output")
                          ),
                          tabPanel("Data Frame Route", style = "padding:20px;",
                                   splitLayout(
                                      plotlyOutput("distance_plot", height = "300px", width = "98%"),
                                      plotlyOutput("time_plot", height = "300px", width = "98%")
                                   ),
                                   DT::dataTableOutput("table_route", height = "400px")
                          )
              )
    )
  )
))

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    #arrange(id_order) %>% 
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 19.3900, lat = 52.1300, zoom = 6) %>%
      addProviderTiles(
        "Esri.WorldGrayCanvas",
        group = "Esri.WorldGrayCanvas"
      ) %>%
      addProviderTiles(
        "OpenStreetMap",
        group = "OpenStreetMap"
      ) %>%
      addProviderTiles(
        "Esri.WorldStreetMap",
        group = "Esri.WorldStreetMap"
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldStreetMap"
        ),
        # position it on the topleft
        position = "topleft"
      )
  })
 
 ## zdefiniowanie wyboru z listy miast na podstawie slidera
  
  start_city_choices <- reactive({
    data$Nazwa[input$num_cities[1]:input$num_cities[2]]
  })
  
  end_city_choices <- reactive({
    data$Nazwa[input$num_cities[1]:input$num_cities[2]]
  })
  
  observe({
    choices_start <- start_city_choices()
    updateSelectInput(session, "start_city", choices = c("None", choices_start))
    
    choices_end <- end_city_choices()
    updateSelectInput(session, "end_city", choices = c("None", choices_end))
  })
  
  

  
  
  observeEvent(input$generate_button, {
    
    # Utworzenie ramki danych tylko z wybranymi miastami
    
    selected_data <- data[input$num_cities[1]:input$num_cities[2], ]
    selected_data <- tibble::rowid_to_column(selected_data, "id")
    
    
     # Zdefiniowanie miasta startowego i końcowego
    start_city <- input$start_city
    start_city_row <- which(selected_data$Nazwa == start_city)
    
    end_city <- input$end_city
    end_city_row <- which(selected_data$Nazwa == end_city)
    
    
    
    
### UTWORZENIE TSP DLA OBLICZENIA ODLEGŁOŚCI, BEZ WYKORZYSTANIA WAG
    # Utworzenie macierzy odległości
    data_coords <- selected_data %>% select(longitude, latitude)
    dist_mat <- as.matrix(distm(data_coords, fun = distHaversine)) / 1000
    
#### 
    
    
    
### UTWORZENIE TSP Z WAGAMI
    # Wybór odpowiedniej kolumny z wagami i utworzenie skalowanej macierzy
    if (input$weight_column != "None") {
        weight_column <- input$weight_column
        selected_data$weight <- selected_data[[weight_column]]
        
        scaled_dist_mat <- dist_mat * selected_data$weight
        symmetric_dist_mat <- (scaled_dist_mat + t(scaled_dist_mat)) / 2
    } else {
        symmetric_dist_mat <- dist_mat
    }
    # Utworzenie obiektu TSP
    tsp_prob <- TSP(symmetric_dist_mat)
    
    tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')
    
    # Rozwiązanie problemu TSP
    tour <- solve_TSP(tsp_prob, method = input$method, control = list(rep = input$iterations))
#### 
    
    
    
    # Wyznaczenie optymalnej trasy
    if (start_city == "None" || (start_city == "None" && end_city != "None")) {
        path <<- cut_tour(tour, 'dummy')
    } else {
        path <<- cut_tour(tour, cut = start_city_row, exclude_cut = FALSE)
        
        dummy_index <- which(names(path) == "dummy")
        path <<- path[-dummy_index]
    }
    
    
    
    # Utworzenie nowej trasy, zaktualizowanej o punkt startowy lub końcowy
    if (start_city == "None" && end_city == "None") {
        new_path <<- path
    } else if (start_city != "None" && end_city == "None") {
        new_path <<- append(start_city_row, path[path != start_city_row])
    } else if (start_city == "None" && end_city != "None") {
        new_path <<- append(path[path != end_city_row], end_city_row)
    } else if (start_city != "None" && end_city != "None") {
        index <- which(names(path) == end_city_row)
        new_path <<- path[-index]
        new_path <<- append(new_path, end_city_row)
    } else {
        new_path <<- NULL
    }
    
    
    #&& end_city != start_city
    if (!is.null(new_path)) {
      if (end_city != "None") {
        names(new_path)[length(new_path)] <- end_city_row
      }
      #if ((start_city == "None" && end_city != "None") || (start_city != "None" && end_city == "None")){
      #dummy_index <- which(names(new_path) == "dummy")
      #new_path <- new_path[-dummy_index]
      #}
    }
    
    
selected_data <- selected_data %>% mutate(id_order = order(as.integer(new_path)))
    

if ((start_city == "None" && end_city == "None") || start_city == end_city) {
  
  first_city_row <- selected_data[selected_data$id_order == 1, ]
  first_city_row$id = selected_data$id[max(selected_data$id)] + 1
  first_city_row$id_order = selected_data$id[max(selected_data$id_order)] + 1
  
  selected_data <- rbind(selected_data, first_city_row)
  new_path <- append(new_path, first_city_row$id_order)
}

    # Mapa
    output$map <- renderLeaflet({
        #arrange(id_order) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(
          data = selected_data,
          radius = 5,
          fillColor = '#050505',
          fillOpacity = 0.7,
          stroke = FALSE,
          label = ~Nazwa
        ) %>% 
        addPolylines(
          data = selected_data %>% arrange(id_order),
          lng = ~longitude,
          lat = ~latitude,
          color = '#222222'
        ) %>%
        addProviderTiles(
          "Esri.WorldGrayCanvas",
          group = "Esri.WorldGrayCanvas"
        ) %>%
        addProviderTiles(
          "OpenStreetMap",
          group = "OpenStreetMap"
        ) %>%
        addProviderTiles(
          "Esri.WorldStreetMap",
          group = "Esri.WorldStreetMap"
        ) %>%
        addLayersControl(
          baseGroups = c(
            "Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldStreetMap"
          ),
          # position it on the topleft
          position = "topleft"
        )
    })
    
    
    # Przygotowanie danych
    route_data <- selected_data %>% filter(id_order %in% new_path) %>% arrange(id_order)
    
    # Obliczenie łącznej długości trasy
    coords <- route_data[, c("longitude", "latitude")]
    total_distance <- round(sum(geosphere::distHaversine(coords[1:nrow(coords),]))/1000, 3)
    
    
    # Wyświetlanie łącznej długości trasy
    output$total_distance_output <- renderText(
      paste(total_distance, "km")
      )
    
    
    
    
    
    # Utworzenie wektora przechowującego długości tras w kolejnych iteracjach
    distances <- c()
    
    # Rozwiązanie problemu TSP
    for (i in 1:input$iterations) {
      tour <- solve_TSP(tsp_prob, method = input$method, control = list(rep = i))
      distance <- tour_length(tour)
      distances <- c(distances, distance)
    }
    
    
    
    plot_theme <- theme(
      plot.background = element_rect(fill = "#306578"),
      panel.background = element_rect(fill = "#306578"), 
      axis.title = element_text(size = 10,
                                color = "#dddddd"), 
      plot.title = element_text(size = 8,
                                color = "#dddddd",
                                vjust = 2,
                                hjust = 0.5), 
      legend.background = element_rect(color = "#306578", 
                                       fill = "#777777"),  
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      axis.text = element_text(size = 8, 
                               color = "#dddddd"))
    
    data_plots <- data.frame(Iteration = 1:isolate(input$iterations), Distance = distances, Time = runif(isolate(input$iterations)))
    data_plots$CumulativeTime <- cumsum(data_plots$Time)
    
    # Wykres
    output$distance_plot <- renderPlotly({

      g <- ggplot(data_plots, aes(x = Iteration, y = Distance)) +
        geom_line() +
        geom_point() +
        xlab("Iteration") +
        ggtitle("Total distance for each iteration") +
        ylab("Route length") + plot_theme
      
      ggplotly(g)%>% config(displayModeBar = F)
      
    })
    
    output$time_plot <- renderPlotly({
      
      g2 <- ggplot(data_plots, aes(x = Iteration, y = CumulativeTime)) +
        geom_line() +
        geom_point() +
        xlab("Iteration") +
        ggtitle("Cumulative time for each iteration") +
        ylab("Iteration time (ms)") + plot_theme
      
      
      ggplotly(g2)%>% config(displayModeBar = F)
      
    })
    
    output$table_route <- DT::renderDataTable(DT::datatable(selected_data, options = list(
      rownames = FALSE,
      pageLength = 8,
      scrollX = TRUE,
      lengthMenu = c(6, 8))
    ))
    
    
  })
}

shinyApp(ui = ui, server = server)

