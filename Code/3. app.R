library(shiny)
library(tidyverse)
library(ggmap)
library(proj4)
library(sf)
library(rgdal)
library(sp)
library(leaflet)
library(shinydashboard)

ui <- dashboardPage(
   
   dashboardHeader(title = "NYC Stop and Frisk"),
   
   dashboardSidebar(h3("Map Filters"),
                    
                    selectInput(inputId = "map_year", label =  "Year:", choices = c(2012:2016), selected = 2016),
                    
                    br(),
                    
                    checkboxGroupInput("check_city", label = "Borough:",
                                       choices = c("Bronx" = "BRONX",
                                                   "Brooklyn" = "BROOKLYN",
                                                   "Manhattan" = "MANHATTAN",
                                                   "Queens" = "QUEENS",
                                                   "Staten Island" = "STATEN ISLAND"),
                                       selected = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")),
                    br(),
                    
                    checkboxGroupInput("check_race", label = "Race:",
                                       choices = c("Asian" = "asian",
                                                   "Black" = "black",
                                                   "Hispanic" = "hispanic",
                                                   "Native American" = "native.american",
                                                   "Other" = "other",
                                                   "White" = "white"),
                                       selected = c("asian","black","hispanic","native.american","other","white"))),               
   
   dashboardBody(fluidRow(
      box(leafletOutput(outputId = "mymap", height = '525px'), width = 8),
      tabBox(title = "Time Trends", side = "right", selected = "Counts", id = "tab1", width = 4,
         tabPanel("Rates", "Frisked & Arrested Rates (# / Total Stopped)", 
                  selectInput(inputId = "rate_type", label = "Rate Type", 
                              choices = c("Frisked" = "frisked_rate", "Arrested" = "arrested_rate"), 
                              selected = "Frisked"), 
                  plotOutput("plot_rates"), style='height:500px'),
         tabPanel("Counts", "Stopped Counts by Year",
                  selectInput(inputId = "count_bor", label = "Borough",
                              choices = c("All" = "all", "Bronx" = "BRONX", "Brooklyn" = "BROOKLYN", 
                                          "Manhattan" = "MANHATTAN", "Queens" = "QUEENS", 
                                          "Staten Island" = "STATEN ISLAND"),
                              selected = "all"),
                  plotOutput("plot_counts"),
                  style='height:500px')
      )),
      fluidRow(
         box(plotOutput(outputId = "prob.plot"), title = "Probability of being stopped (# Stopped / Census Population)"),
         box(title = "Stopped Probability Inputs",
             selectInput(inputId = "plot_year", label =  "Year:", choices = c(2012:2016), selected = 2016),
             br(),
             checkboxGroupInput("plot_check_race", label = "Race:",
                                choices = c("Asian" = "asian",
                                            "Black" = "black",
                                            "Hispanic" = "hispanic",
                                            "Native American" = "native.american",
                                            "Other" = "other",
                                            "White" = "white"),
                                selected = c("asian","black","hispanic","white")),
         width = 2),
         box(selectInput(inputId = "force_year", label =  "Year:", choices = c(2012:2016), selected = 2016),
             selectInput(inputId = "force_fill", label =  "Fill (Bars):", 
                         choices = c("Race" = "suspect.race", "Reason Stopped" = "why.stopped"), selected = "Race"),
            plotOutput(outputId = "force_plot"),
            width = 4)
      ),
      fluidRow(
         box(title = "Predicted Probability of Finding a Weapon",
             textOutput(outputId = "formula"),
             textOutput(outputId = "formula2"),
             br(),
            plotOutput(outputId = "lr.prob.frisked"))
      )
   )
)


server <- function(input, output){
  
   df <- reactive({data %>% 
                   group_by(year) %>%
                   filter(year == input$map_year,
                          suspect.race %in% input$check_race,
                          city %in% input$check_city)})
   
   pal <- colorFactor(
      palette = c('dark orange', 'red', 'dark green', 'blue', 'black', 'purple'),
      domain = data$suspect.race)
   
   output$mymap <- renderLeaflet({
     leaflet(data) %>% 
       setView(lng = -74, lat = 40.7, zoom = 11) %>%
       addTiles() %>% 
       addCircleMarkers(data = df(), lat = ~ lat, lng = ~ lon, weight = 2, 
                  color = ~pal(suspect.race), fillOpacity = 0.5, radius = ~(1 + 2*contraband_count)) %>%
       addLegend("topleft", pal = pal, values = ~ data$suspect.race,
                 title = "Race",
                 opacity = 1)
   })
   
   output$prob.plot <- renderPlot({
      testdata = data_census %>% 
         select(city, year, precinct, suspect.race, population) %>% 
         filter(year == input$plot_year,
                suspect.race %in% input$plot_check_race) %>% 
         arrange(city) %>%
         group_by(city, suspect.race) %>% 
         summarize(probability = n() / sum(population))
      
      ggplot(testdata, aes(x = city, y = probability, fill = suspect.race)) +
         geom_bar(stat = 'identity', position = 'dodge') +
         scale_y_continuous(labels = percent) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
   })
   
   output$plot_counts <- renderPlot({
      
      data_counts <- reactive({
          if(input$count_bor == "all") { data } else {
               data %>%
               filter(city %in% input$count_bor)}
      })
      
      ggplot(data_counts(), aes(fill = suspect.race, x = year), width = 4)+
         geom_bar(position = 'stack', stat = 'count') + 
         labs(x = "Year", y = "Number Stopped", fill = "Race") + 
         scale_y_continuous(labels = comma, limits = c(0, 520000))
   })
   
   output$plot_rates <- renderPlot({
      
      data_display <- reactive({if(input$rate_type == "frisked_rate"){ rate_frisk } else { rate_arrest }})
      
      ggplot(data_display(), aes(fill = suspect.race, x = year, y = data_display()[,4]), width = 4) +
         geom_bar(stat = 'identity', position = 'dodge') +
         labs(x = "Year", y = "Rate (%)", fill = "Race") + 
         scale_y_continuous(labels = scales::percent)

   })
   
   output$force_plot <- renderPlot({
      
      force_data <- reactive({data %>% filter(year == input$force_year)})
      
      ggplot(force_data(), aes(fill = suspect.race, x = force.used), width = 4)+
         geom_bar(position = 'stack', stat = 'count') + 
         labs(x = "Types of Force", y = "Force Used (Count)", fill = "Race") + 
         scale_y_continuous(labels = comma) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
   })
   
   output$lr.prob.frisked <- renderPlot({
      
      ggplot(glm_data, aes(x=year, y=predicted, color = suspect.race)) +
         stat_smooth(method = "glm", formula = y~x,
                     alpha = 0.2, size = 2, aes(fill = suspect.race)) +
         ggtitle("Probability of finding a weapon") +
         labs(x = "Year", y = "Predicted Probability")
   })
   
   output$formula <- renderText("Data subset on suspected crime = criminal possession of a weapon")
   
   output$formula2 <- renderText("Found Weapon ~ Age + Height + Weight + Build + Sex + Race * Year")
   
}


shinyApp(ui = ui, server = server)
