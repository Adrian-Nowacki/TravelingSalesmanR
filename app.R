library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)
library(geosphere)
library(TSP)

data <- read.csv("data/cities_data.csv")

ui <- fluidPage(
  titlePanel("The Traveling Salesman Problem for Polish cities"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_cities", "Select cities (ranked by population):", min = 2, max = 66,value = c(2, 66), step = 1),
      selectInput("method", "TSP calculation method:", choices = c("farthest_insertion", "nearest_insertion")),
      selectInput("weight_column", "Weight column:", choices = colnames(data)[2:(ncol(data)-2)]),
      selectInput("start_city", "Starting city:", choices = c("None", data$Nazwa)),
      selectInput("end_city", "End city:", choices = c("None", data$Nazwa)),
      numericInput("iterations", "Iterations:", value = 50, min = 1, max = 100),
      actionButton("generate_button", "Generate route")
    ),
    mainPanel(
      leafletOutput("map"),
      textOutput("total_distance_output"),
      plotOutput("distance_plot")
    )
  )
)

server <- function(input, output, session) {
  
 
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
    
    # Utworzenie obiektu TSP
    tsp_prob <- TSP(dist_mat)
    tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')
    
    # Rozwiązanie problemu TSP
    tour <- solve_TSP(tsp_prob, method = input$method, control = list(input$iterations))
    
    total_distance <- tour_length(tour) # całkowita odległość trasy
#### 
    
    
    
### UTWORZENIE TSP Z WAGAMI
    # Wybór odpowiedniej kolumny z wagami i utworzenie skalowanej macierzy
    weight_column <- input$weight_column
    selected_data$weight <- selected_data[[weight_column]]
    
    scaled_dist_mat <- dist_mat * selected_data$weight
    symmetric_dist_mat <- (scaled_dist_mat + t(scaled_dist_mat)) / 2
    
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
      selected_data <- selected_data %>% mutate(id_order = order(as.integer(new_path)))
    }

    
    
    
    # Mapa
    output$map <- renderLeaflet({
        #arrange(id_order) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(
          data = selected_data,
          fillColor = 'red',
          fillOpacity = 0.5,
          stroke = FALSE,
          label = ~Nazwa
        ) %>% 
        addPolylines(
          data = selected_data %>% arrange(id_order),
          lng = ~longitude,
          lat = ~latitude,
          color = 'blue'
        )
    })
    
    
    # Wyświetlanie łącznej długości trasy
    output$total_distance_output <- renderText(paste("Długość trasy:", round(total_distance, 3), "kilometrów"))
    
    
    
    
    
    # Utworzenie wektora przechowującego długości tras w kolejnych iteracjach
    distances <- c()
    
    # Rozwiązanie problemu TSP
    for (i in 1:input$iterations) {
      tour <- solve_TSP(tsp_prob, method = input$method, control = list(rep = i))
      distance <- tour_length(tour)
      distances <- c(distances, distance)
    }
    
    # Wykres
    output$distance_plot <- renderPlot({
      plot(1:input$iterations, distances, type = "b", xlab = "Iteration", ylab = "Route length")
    })
    
    
  })
}

shinyApp(ui = ui, server = server)

