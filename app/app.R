

# Adrian Nowacki
# Aplikacja pozwala wygenerować najkrótszą trasę dla 66 polskich miast o największej liczbie ludności. 
# Umożliwia wybór miasta startowego oraz końcowego, wagę odwiedzanej trasy oraz wybór algorytmu i jego iteracji.

# https://adryanqe.shinyapps.io/TravelingSalesmanR/


#Sys.setlocale(category = "LC_ALL", locale = "pl_PL.UTF-8")
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
library(htmlwidgets)
library(leaflet.providers)
library(htmltools)
library(sf)
library(shinyjs)
library(shinycssloaders)
library(shinybusy)
#library(rvest)
#library(tidygeocoder)


    #data <- read.csv("data/wagi_miast.csv", encoding = "UTF-8")

# 1. dołączenie długości oraz szerokości geograficznej do miejscowosci
         #data <- data %>% geocode(Nazwa, method = 'osm', lat = latitude , long = longitude)

# 2. pobranie danych populacji
         #miasta_pop = read_html("https://pl.wikipedia.org/wiki/Dane_statystyczne_o_miastach_w_Polsce")
         #table = html_node(miasta_pop, ".wikitable")
         #table = html_table(table, fill = TRUE)

    # wyodrębnienie miejscowosci i przypisanie kolumny z populacją
         #a <- table[table$Miasto %in% data$Nazwa,]
         #a <- a[-33, ]
        
         #data <- data %>% arrange(Nazwa)
         #pop <- a$`Liczba ludności (01.01.2021)`
         #data$population <- pop
         #data <- data %>% arrange(desc(population))
         #write.csv(data, "data/cities_data.csv", row.names = FALSE)

# w celu poprawnej wizualizacji w aplikacji shiny na shinyapps.io usunięto polskie znaki z nazw miast
data <- read.csv("data/cities_data.csv")



ui <-  fluidPage(
  shinyjs::useShinyjs(), collapsible = TRUE,
  includeCSS(path = "style.css"),
  navbarPage(title = div("The Traveling Salesman Problem for Polish cities", style = "text-align:center; font-family: Candara; margin-left:auto;margin-right:auto;"),
             windowTitle = HTML("Traveling Salesman in R</title>"),
             
  tags$head(tags$meta(charset = "UTF-8"),
),
tabPanel(
  HTML("<div style='display: flex; margin-top:12px; margin-right:10px;'>
          <a href='https://github.com/Adrian-Nowacki/TravelingSalesmanR'>
            <img src='github-sign.png' style='width: 30px; margin-right: 10px;'/>
            <span style='font-size: 16px; color: white;display: inline-block; vertical-align: middle;'>GitHub</span>
          </a>
        </div>")
),
  sidebarLayout(
    sidebarPanel(id = "dt_sidebar", style = "width: 85%;",
                 p("The app allows to generate the shortest route for 66 Polish cities with the largest population."),
      sliderInput("num_cities", "Select cities (ranked by population):", min = 1, max = 66,value = c(1, 66), step = 1, width = "92%"),
      selectInput("way", "Select way:", choices = c("straight lines", "roads"), width = "92%"),
      selectInput("start_city", "Starting city:", choices = c("None", data$Nazwa), width = "92%"),
      selectInput("end_city", "End city:", choices = c("None", data$Nazwa), width = "92%"),
      selectInput("weight_column", "Weight column:", 
                  choices = c("None","Restaurant","History", "Long Journeys", "Cycling","Swimming",
                              "Crowds", "Paintings", "Theatres", "Heat","Concerts", "Spending Time in Nature",
                              "Big Cities", "Small Towns", "Highclass Hotels","Hostels",
                              "Internet Information About City", "Animals","Parks","Sea",
                              "Non Touristy Places", "Places", "Average Weight"), 
                  selected = "None", 
                  width = "92%"),
      fluidRow(
         column(7,  selectInput("method", "Algorithm:", 
                                choices = c("Identity-Based Heuristic", "Random Search", "Nearest Neighbor Insertion",
                                            "Cheapest Insertion","Farthest Insertion",  "Arbitrary Insertion",
                                            "Nearest Neighbor", "Repetitive Nearest Neighbor", "2-Opt"), 
                                selected = "Nearest Neighbor")),
         column(4, numericInput("iterations", "Iterations:", value = 50, min = 1, max = 1000))
      ),
      actionButton("generate_button", "Generate route")
    ),
    mainPanel(id = "mainpanel",
              tabsetPanel(id = "tabsetPanel",
                          tabPanel("Interative Map", style = "padding:20px;",
                                  leafletOutput("map", height = "500px"),
                                  textOutput("total_distance_output"),
                                  fluidRow(
                                    column(6),
                                    column(2,
                                           downloadButton('download_gpkg', 'Save cities as .gpkg')),
                                    column(2,
                                           downloadButton('download_gpkg_lines', 'Save route as .gpkg')),
                                    column(2,
                                           downloadButton('download_csv', 'Save data as .csv'))
                                  )
                          ),
                          tabPanel("Distance stats", style = "padding:20px;",
                                   splitLayout(
                                      plotlyOutput("distance_iter_plot", height = "30vh", width = "98%") %>% withSpinner(type = 5, color = "#306578", size = 0.5),
                                      plotlyOutput("time_plot", height = "30vh", width = "98%") %>% withSpinner(type = 5, color = "#306578", size = 0.5)),
                                   DT::dataTableOutput("table_route", height = "40vh") %>% withSpinner(type = 5, color = "#306578", size = 0.5)),
                          
                          tabPanel(id = "dist_tab", value = "dist_tab", "Duration stats", style = "padding:20px;",
                                     plotlyOutput("duration_plot", height = "40vh", width = "100%") %>% withSpinner(type = 5, color = "#306578", size = 0.5)
                                     )
                          )
              
                        )
                      )
))


server <- function(input, output, session) {

  
  # pusta mapa podkładowa
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(
        "Esri.WorldStreetMap",
        group = "Esri.WorldStreetMap"
      ) %>%
      setView(lng = 19.3900, lat = 52.1300, zoom = 6) %>%
      addProviderTiles(
        "Esri.WorldGrayCanvas",
        group = "Esri.WorldGrayCanvas"
      ) %>%
      addProviderTiles(
        "OpenStreetMap",
        group = "OpenStreetMap"
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Esri.WorldStreetMap", "Esri.WorldGrayCanvas", "OpenStreetMap"
        ),
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
  
  
  # zaktualizowanie listy miast na podstawie slidera
  observe({
    choices_start <- start_city_choices()
    updateSelectInput(session, "start_city", choices = c("None", choices_start))
    
    choices_end <- end_city_choices()
    updateSelectInput(session, "end_city", choices = c("None", choices_end))
  })
  
  

  ## switch nazw wag
  selected_weight <- eventReactive(input$generate_button,{
  switch(input$weight_column,
                  "None" = "None",
                  "Restaurant" = "likes_restaurants",
                  "History" = "interested_in_history",
                  "Long Journeys" = "likes_long_journeys",
                  "Cycling" = "likes_cycling",
                  "Swimming" = "likes_swimming",
                  "Crowds" = "likes_crowds",
                  "Paintings" = "likes_paintings",
                  "Theatres" = "likes_going_to_the_theatre",
                  "Heat" = "likes_heat",
                  "Concerts" = "likes_concerts",
                  "Spending Time in Nature" = "likes_spending_time_in_the_nature",
                  "Big Cities" = "likes_big_cities",
                  "Small Towns" = "likes_small_towns",
                  "Highclass Hotels" = "enjoys_sleeping_in_highclass_hotels",
                  "Hostels" = "enjoys_sleeping_in_hostels",
                  "Internet Information About City" = "looks_for_information_about_the_cities_on_the_internet",
                  "Animals" = "likes_animals",
                  "Parks" = "likes_parks",
                  "Sea" = "likes_the_sea",
                  "Non Touristy Places" = "likes_exploring_nontouristyplaces",
                  "Places" = "likes_mountains",
                  "Average Weight" = "srednia_waga"
  )
  })
  
  
  
  ## switch nazw algorytmów
  selected_algorithm <- eventReactive(input$generate_button,{
    switch(input$method,
           "Identity-Based Heuristic" = "identity", 
           "Random Search" = "random", 
           "Nearest Neighbor Insertion" = "nearest_insertion",
           "Cheapest Insertion" = "cheapest_insertion", 
           "Farthest Insertion" = "farthest_insertion", 
           "Arbitrary Insertion" = "arbitrary_insertion",
           "Nearest Neighbor" = "nn", 
           "Repetitive Nearest Neighbor" = "repetitive_nn", 
           "2-Opt" = "two_opt"
    )
  })
  
 
  
  observeEvent(input$generate_button, {
    show_modal_spinner(
      spin = "fading-circle",
      color = "#306578",
      text = "Calculating the most optimal route...",
    )
    # Utworzenie ramki danych tylko z wybranymi miastami
    selected_data <- data[input$num_cities[1]:input$num_cities[2], ]
    selected_data <- tibble::rowid_to_column(selected_data, "id")
    
    
     # Zdefiniowanie miasta startowego i końcowego
    start_city <- input$start_city
    start_city_row <- which(selected_data$Nazwa == start_city)
    
    end_city <- input$end_city
    end_city_row <- which(selected_data$Nazwa == end_city)
    
    
    
    
    # Utworzenie macierzy odległości
    if (input$way == "straight lines"){
        data_coords <- selected_data %>% select(longitude, latitude)
        dist_mat <- as.matrix(distm(data_coords, fun = distHaversine)) / 1000
    } else if (input$way == "roads"){
        dist_mat <- read.csv("data/road_dist_mat.csv")
        colnames(dist_mat) <- NULL
        dist_mat <- dist_mat[1:nrow(selected_data), 1:nrow(selected_data)]
        dist_mat <- as.dist(as.matrix(dist_mat))
    }
    
    

    
    ### UTWORZENIE TSP Z WAGAMI
    weight_column <- selected_weight()
    
    # Wybór odpowiedniej kolumny z wagami i utworzenie skalowanej macierzy
    if (weight_column != "None") {
        selected_data$weight <- selected_data[[weight_column]]
        
        scaled_dist_mat <- dist_mat * selected_data$weight
        symmetric_dist_mat <- (scaled_dist_mat + t(scaled_dist_mat)) / 2
    } else {
        symmetric_dist_mat <- dist_mat
    }
   
    
    tsp_prob <- TSP(symmetric_dist_mat)
    
    tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')
    
    algorithm <- selected_algorithm()

    tour <- solve_TSP(tsp_prob, method = algorithm, control = list(rep = input$iterations))
    #### 
    
    
    
    
    
    # Wyznaczenie optymalnej trasy
    if (start_city == "None" || (start_city == "None" && end_city != "None")) {
        path <- cut_tour(tour, 'dummy')
    } else {
        path <- cut_tour(tour, cut = start_city_row, exclude_cut = FALSE)
        
        dummy_index <- which(names(path) == "dummy")
        path <- path[-dummy_index]
    }
    
    
    
    # Utworzenie nowej trasy, zaktualizowanej o punkt startowy lub końcowy
    if (start_city == "None" && end_city == "None") {
        new_path <- path
    } else if (start_city != "None" && end_city == "None") {
        new_path <- append(start_city_row, path[path != start_city_row])
    } else if (start_city == "None" && end_city != "None") {
        new_path <- append(path[path != end_city_row], end_city_row)
    } else if (start_city != "None" && end_city != "None") {
        index <- which(names(path) == end_city_row)
        new_path <- path[-index]
        new_path <- append(new_path, end_city_row)
    } else {
        new_path <- NULL
    }
    
    
    #&& end_city != start_city
    if (!is.null(new_path)) {
      if (end_city != "None") {
        names(new_path)[length(new_path)] <- end_city_row
      }
    }
    
  
    
    
selected_data <- selected_data %>% mutate(id_order = order(as.integer(new_path)))
    

# warunek przypisujący pierwsze miasto jako ostatnie
if ((start_city == "None" && end_city == "None") || start_city == end_city) {
  
  first_city_row <- selected_data[selected_data$id_order == 1, ]
  first_city_row$id = selected_data$id[max(selected_data$id)] + 1
  first_city_row$id_order = selected_data$id[max(selected_data$id_order)] + 1
  
  selected_data <- rbind(selected_data, first_city_row)
  new_path <- append(new_path, first_city_row$id_order)
}


    
    
##### Przygotowanie danych osm do trasy obejmującej drogi

      # osrm_route <- function(from, to) {
      #   route <- osrm::osrmRoute(src = from, dst = to)
      #   geometry <- route$geometry
      #   distance <- route$distance
      #   
      #   return(list(geometry = geometry, distance = distance))
      # }
      # 
      # dist_mat <- matrix(0, nrow = nrow(selected_data), ncol = nrow(selected_data), 
      #                    dimnames = list(selected_data$Nazwa, selected_data$Nazwa))
      # 
      # for (i in 1:nrow(selected_data)) {
      #   for (j in 1:nrow(selected_data)) {
      #     if (i != j) {
      #       from <- selected_data[i, c("longitude", "latitude")]
      #       to <- selected_data[j, c("longitude", "latitude")]
      #       result <- osrm_route(from, to)
      #       dist_mat[i, j] <- result$distance
      #       dist_mat[j, i] <- result$distance
      #     }
      #   }
      # }

    route_data <- selected_data %>% filter(id_order %in% new_path) %>% arrange(id_order)
    
    distance_list <- c()
    duration_list <- c()
    if (input$way == "straight lines"){
        # Obliczenie łącznej długości trasy
        coords <- route_data[, c("longitude", "latitude")]
        total_distance <- round(sum(geosphere::distHaversine(coords[1:nrow(coords),]))/1000, 3)
        
        # Wyświetlanie łącznej długości trasy
        output$total_distance_output <- renderText(
          paste(total_distance, "km")
          )
    } else if (input$way == "roads"){
      #distance_list <- c()
      for (i in 1:(nrow(route_data) - 1)){
        from <- route_data[i, c("longitude", "latitude")]
        to <- route_data[i + 1, c("longitude", "latitude")]
        
        route <- osrm::osrmRoute(src = from, dst = to, overview = "full")
        distance <- route$distance
        distance_list[i] <- distance
        duration <- round(route$duration, 0)
        duration_list[i] <- duration
      }
      total_duration <- sum(distance_list)
      hours <- round(total_duration %/% 60, 0)
      minutes <- round(total_duration %% 60, 0)
      # Wyświetlanie łącznej długości trasy i czasu
      output$total_distance_output <- renderText(
        
        paste0(hours, " h ", minutes, " min  /  ", round(sum(distance_list), 3), " km")
      )
    }
        
#####
    
    
    
    
    
    
    # Utworzenie wektora przechowującego długości tras w kolejnych iteracjach
    distances <- c()
    
    # Rozwiązanie problemu TSP
    for (i in 1:input$iterations) {
      tour <- solve_TSP(tsp_prob, method = algorithm, control = list(rep = i))
      distance <- tour_length(round(tour, 3))
      distances <- c(distances, distance)
    }
   
    ##styl wykresu
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
    
    
    
    
    
    # wykres przedstawiający ogólną odległość dla każdej iteracji
    output$distance_iter_plot <- renderPlotly({

      g <- ggplot(data_plots, aes(x = Iteration, y = Distance)) +
        geom_line() +
        geom_point() +
        xlab("Iteration") +
        ggtitle("Estimated total distance for each iteration (without the same end city as starting city)") +
        ylab("Route length") + plot_theme
      
      ggplotly(g)%>% config(displayModeBar = F)
      
    })
    
    
    
     #wykres przedstawiający skumulowany czas dla każdej iteracji
     output$time_plot <- renderPlotly({
      
      g2 <- ggplot(data_plots, aes(x = Iteration, y = CumulativeTime)) +
        geom_line() +
        geom_point() +
        xlab("Iteration") +
        ggtitle("Cumulative time for each iteration") +
        ylab("Iteration time (ms)") + plot_theme
      
      
      ggplotly(g2)%>% config(displayModeBar = F)
      
    })
    

     
     
     

    selected_data <- selected_data %>%
      arrange(id_order)
    
    #przypisanie długości dróg i linii prostych
    if (input$way == "straight lines"){
        ## obliczenie ogólnej odległości oraz odległości pomiędzy miastami w kolejności trasy
        distances <- round(distHaversine(selected_data[, c("longitude", "latitude")])/1000, 3)
        selected_data$distance <- c(0, distances)
        selected_data$ov_distance <- round(cumsum(selected_data$distance), 3)
    } else if (input$way == "roads"){
        selected_data$distance <- c(0, distance_list)
        selected_data$ov_distance <- round(cumsum(selected_data$distance), 3)
    }
        
        
    
    
    
    
 ##### WYKRESY CZASU DROGI ORAZ ODLEGŁOŚCI
    
    plot_data <- selected_data %>% arrange(id_order)
    duration_list <- duration_list[-length(duration_list)]
    duration_list <- c(0, duration_list)
    plot_data <- plot_data[ -nrow(plot_data),]
    plot_data$duration <- duration_list
    
    City <- reorder(plot_data$Nazwa, plot_data$id_order)
    
    
    ## wykres czasu drogi dla wszystkich miast
     output$duration_plot <- renderPlotly({
    
      g3 <- ggplot(plot_data, aes(x = City, y = duration, group=1)) +
        geom_line() +
        geom_point() +
        xlab("") +
        ggtitle("Duration from previous city") +
        ylab("Duration in minutes") + plot_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
       ggplotly(g3)%>% config(displayModeBar = F)
       })
    

 ######  

    
    
    #wyszczególnienie kolumn do data table
    datatable <- selected_data[, c("id_order", "Nazwa", "distance", "ov_distance")]
    
    colnames(datatable) <- c("Route order",
                             "City",
                             "Distance from the previous city",
                             "Cumulative distance")
    
    
    
    ## data frame prezentująca wynikowe miasta
    output$table_route <- DT::renderDataTable(DT::datatable(datatable, options = list(
      rownames = FALSE,
      pageLength = 8,
      scrollX = TRUE,
      rownames = TRUE,
      lengthMenu = c(6, 8)) 
    ))
    
    
    # przypisanie wartości w popupie
    labels <- paste(
      "Route order: <strong>", selected_data$id_order, "</strong><br/>",
      "City: <strong>", selected_data$Nazwa, "</strong><br/>",
      "Distance from the previous city: <strong>", selected_data$distance, " km</strong><br/>",
      "Cumulative distance: <strong>", selected_data$ov_distance, " km</strong>") %>%
      lapply(htmltools::HTML)
    
    
    
    # pozyskanie geometrii wszystkich rekordów osm w celu wizualizacji
    geometry_list <- vector("list", nrow(route_data) - 1)
    
    for (i in 1:(nrow(route_data) - 1)){
      from <- route_data[i, c("longitude", "latitude")]
      to <- route_data[i + 1, c("longitude", "latitude")]
      
      route <- osrm::osrmRoute(src = from, dst = to, overview = "full")
      geometry <- route$geometry
      geometry_list[i] <- geometry
      
    }
    roads_geom_list <- lapply(geometry_list, function(geom) st_linestring(geom))
    roads_geom_list <- st_sfc(roads_geom_list)
    
    
    
    leaflet_text <- leaflet() %>% addProviderTiles(
          "Esri.WorldStreetMap",
          group = "Esri.WorldStreetMap"
        ) %>%
          setView(lng = 19.3900, lat = 52.1300, zoom = 6) %>%
          addProviderTiles(
            "Esri.WorldGrayCanvas",
            group = "Esri.WorldGrayCanvas"
          ) %>%
          addProviderTiles(
            "OpenStreetMap",
            group = "OpenStreetMap"
          ) %>%
          addLayersControl(
            baseGroups = c(
              "Esri.WorldStreetMap", "Esri.WorldGrayCanvas", "OpenStreetMap"
            ),
            position = "topleft"
          ) 
    
    
    
    # Wygenerowanie mapy
    if (input$way == "straight lines"){
        output$map <- renderLeaflet({
          leaflet_text %>%
            addPolylines(
              data = selected_data %>% arrange(id_order),
              lng = ~longitude,
              lat = ~latitude,
              color = '#222222'
            )%>% addControl(
              html = tags$h6(paste('Optimized route generated from straight lines for', input$num_cities[2], 'cities')),
              position = "topright"
            ) %>%
            addCircleMarkers(
              data = selected_data,
              radius = 5,
              fillColor = '#050505',
              fillOpacity = 0.7,
              stroke = FALSE,
              label = ~labels,
              labelOptions = labelOptions(
                  textsize = "12px") 
            )
        })
    } else if (input$way == "roads"){
      output$map <- renderLeaflet({
        leaflet_text %>%
          addCircleMarkers(
            data = selected_data,
            radius = 5,
            fillColor = '#050505',
            fillOpacity = 0.7,
            stroke = FALSE,
            label = ~labels,
            labelOptions = labelOptions(textsize = "12px")
          ) %>%
          addPolylines(
            data = roads_geom_list,
            color = '#27677c',
            opacity = 1,
            weight = 3,
            dashArray = "8, 5",
          ) %>% addControl(
                     html = tags$h6(paste('Optimized route generated from roads for', input$num_cities[2], 'cities')),
                     position = "topright"
          ) 
      })
    }
    remove_modal_spinner()
    
    ###  przypisanie opcji pobrania tabeli jako .csv
    output$download_csv <- downloadHandler(
      filename = function() {
          paste0("TSP_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(selected_data, file, row.names = FALSE)
      }
    )

    
    
    # utworzenie warstwy punktowej miast w celu jej pobrania
    selected_data_sf <- st_as_sf(selected_data, coords = c("longitude", "latitude"), crs = 4326)
    selected_data_sf <- st_transform(selected_data_sf, crs = 2180)
    
    
   
  
    
#### utworzenie warstwy liniowej trasy w celu jej pobrania
    linie <- list()
    if (input$way == "straight lines"){
          for (i in 1:(nrow(selected_data)-1)) {
            linie[i] <- st_sfc(st_linestring(matrix(c(selected_data$longitude[i], selected_data$latitude[i], 
                                                   selected_data$longitude[i+1], selected_data$latitude[i+1]), ncol = 2, byrow = TRUE)))
            
            }
    } else if (input$way == "roads"){
        linie <- roads_geom_list
        
    }
    
    
    #Konwersja listy geometrii na obiekt sf
    lines <- st_sfc(linie)
    selected_data_lines <- st_as_sf(data.frame(id_order = selected_data$id_order[2:nrow(selected_data)] - 1,
                                                Target_city = selected_data$Nazwa[2:nrow(selected_data)],
                                                Distance = selected_data$distance[2:nrow(selected_data)],
                                                Overall_distance = selected_data$ov_distance[2:nrow(selected_data)],
                                                lines))
    st_crs(selected_data_lines) <- st_crs(4326)
    selected_data_lines <- st_transform(selected_data_lines, crs = 2180)
####
    
    
    
    
    ## pobieranie warstwy punktowej miast
    output$download_gpkg <- downloadHandler(
      filename = function() {
          paste0("TSP_cities", "", sep = "")
      },
      content = function(file) {
        st_write(selected_data_sf, file, driver = "GPKG")
      }
    )
    
    
    ## pobieranie warstwy liniowej trasy
    output$download_gpkg_lines <- downloadHandler(
      filename = function() {
        paste0("TSP_route", "", sep = "")
      },
      content = function(file) {
        st_write(selected_data_lines, file, driver = "GPKG")
      }
    )
    
  })
  
  
  ## ustawienie widocznosci przyciskow
  observeEvent(input$generate_button, {
      runjs('document.getElementById("download_gpkg_lines").style.visibility = "visible"')
      runjs('document.getElementById("download_gpkg").style.visibility = "visible"')
      runjs('document.getElementById("download_csv").style.visibility = "visible"')
      if (input$way == "straight lines"){
        hideTab(inputId = "tabsetPanel", target = "dist_tab")
      } else if (input$way == "roads"){
        showTab(inputId = "tabsetPanel", target = "dist_tab")
      }
  })
}

shinyApp(ui = ui, server = server)

