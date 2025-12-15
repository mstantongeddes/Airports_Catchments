##load packages
library(airportr)
library(tidyverse)
library(readxl)
library(gt)
library(dplyr)
library(purrr)
library(leaflet)
library(shiny)
library(renv)

#Prepare data

#Vector of EU countries to filter
eu_alpha2 <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE")

##load data extracted from Cirium
df <- read_excel("Cirium_Airport2Airport_capacity_2025.xlsx", col_types = c("numeric", "text", "text", 
                                                                                                                                                                                                       "text", "text", "text", "text", "numeric", 
                                                                                                                                                                                                       "numeric"))
str(df)

colnames(df)

df <- df %>%
    rename_with(~ .x |>
                    tolower() |>
                    str_replace_all("/ ", "") |>
                    str_replace_all(" ", "_") |>
                    str_replace_all("\\.", ""))

glimpse(df)

#Read in Total Passengers (TPAX) data for airports, from the ACI WORLD ihub.aci.aero website. 
##Note that seats are scheduled airline seats, while Passengers are actual passengers departing + arriving at an airport. 
#Seat data may not capture un-scheduled or charter or private aviation flights, not will it capture empty seats / aircraft load factor 

df_world_tpax <- read_excel("World Airport Traffic Data.xlsx") |>
    select(Code, Country, Airport, TPAX)

glimpse(df_world_tpax)

df_world_tpax2 <- df_world_tpax |> mutate(group = 
                                              case_when(
                                                  TPAX >= 40000000 ~ "Group 1",
                                                  TPAX >= 25000000 ~ "Group 2",
                                                  TPAX >= 10000000 ~ "Group 3",
                                                  TPAX >= 1000000 ~ "Group 4",
                                                  TPAX < 1000000 ~ "Group 5" 
                                              ))

setdiff(df$destination_code, df_world_tpax$Code) #Check airports that are in the ACI TPAX dataset but not in the Cirium scheduled

glimpse(df_world_tpax2)

#Add size category of departure airport and destination airports and join CIRIUM seats and ACI TPAX data to have comprehensive dataset
df_2 <- left_join(df, 
                  df_world_tpax2 |> select(Code, TPAX, group),
                  join_by(origin_code == Code)) |>  #Join group of departure airport
    rename(origin_group = group) |> 
    left_join(
        df_world_tpax2 |> select(Airport, Code, group),
        join_by(destination_code == Code)    #Join group of departure airport
    ) |>
    rename(destination_group = group)

glimpse(df_2)

df_airports <- df_2 |>  
    group_by(origin_code, origin_country_subregion) |> 
    summarise(TPAX = max(TPAX),
              seats = sum(seats)) |>
    arrange(desc(TPAX))

glimpse(df_airports)

unique(df_airports$origin_country_subregion)


#_______Below using a leftjoin to combine airports with TPAX/Seats and airportR Lat Long data

#Load data from AirportR for all airports LAT and LONG for mapping in leaflet
df_airportR <- airportr::airports

df_airports_Wscheduled_traffic <- left_join(
    df_airports,
    df_airportR |> select(IATA, Country, `Country Code (Alpha-2)`, Latitude, Longitude),
    join_by(origin_code == IATA)
)

glimpse(df_airports_Wscheduled_traffic)

###????? Is the left_join above correct? Would a full_join be better and then filter to EU?

df_airports_Wscheduled_traffic_FULLJOIN <- full_join(
    df_airports,
    df_airportR |> select(IATA, Country, `Country Code (Alpha-2)`, Latitude, Longitude),
    join_by(origin_code == IATA)
) #|>
#filter(`Country Code (Alpha-2)` %in% eu_alpha2)

glimpse(df_airports_Wscheduled_traffic_FULLJOIN)

df_airports_Wscheduled_traffic_FULLJOIN |> 
    filter(is.na(TPAX)) #Many airports which have scheduled seats in Cirium data, but for which PAX data is missing

df_airports_Wscheduled_traffic_FULLJOIN |> 
    filter(is.na(seats) & TPAX > 0) #There are no airports for which there is missing seat data, but there is TPAX data. 
#This means that OK to use the left_join dataframe with the basic dataframe coming from Cirium, as it captures all airports for which there is also PAX data

#Back to the analysis with the dataframe centred on Cirium Seats by airports dataframe

#Explore the airports with scheduled traffic data
df_airports_Wscheduled_traffic |> filter(is.na(TPAX))
df_airports_Wscheduled_traffic |> filter(is.na(seats))
df_airports_Wscheduled_traffic |> filter(TPAX <1000000 & seats > 692000)


#Estimate TPAX for airports where there is CIRIUM Seats data

df_airports_Wscheduled_traffic <- df_airports_Wscheduled_traffic |> mutate(
    TPAX = case_when(is.na(TPAX) ~ seats * 1.6,
                     !is.na(TPAX) ~ TPAX)
)


#Filter to create a dataset of only airports in the scope for State Aid - NOTE that filtering on TPAX misses airports that are no in the ACI databaes

df_focus_airports <- df_airports_Wscheduled_traffic |>
    filter(seats < 692000) |>  #Filtering on Seats allows to include airports for which there is no TPAX data
    #filter(TPAX < 1000000) |>
    arrange((desc(TPAX)))

#Review data
df_focus_airports |> arrange(desc(TPAX)) #Captures some airports >1mppa, but as they are in Russia and Turkey, ok to proceed


#Mutate new concatenated field for the label  NOTE decided instead to create label of nearby airports
#df_focus_airports <- df_focus_airports |>
#  mutate(Label = paste(origin_code, TPAX))

glimpse(df_focus_airports)

#Limit to EU airports
df_focus_airports_EU <- df_focus_airports |> filter(`Country Code (Alpha-2)` %in% eu_alpha2) |> arrange(TPAX) 

df_focus_airports_EU$origin_code
unique(df_focus_airports_EU$origin_country_subregion)
glimpse(df_focus_airports_EU)



##Identify airports nearby the focus airports and save as as new dataframe.
###First write function to ensure to pull all data, then map airports_nearby
find_nearby <-  possibly(
    ~ airports_near_airport(.x, distance = 100) %>% mutate(source_airport = .x),
    otherwise = NULL
)

df_nearby <- map_dfr(df_focus_airports_EU$origin_code, find_nearby)

glimpse(df_nearby)


#Filter dataset to remove irrelevant airports lacking IATA codes
df_nearby_removeDromes <- df_nearby |> 
    filter(`Country Code (Alpha-2)` %in% eu_alpha2) |> #limit to EU airports
    filter(IATA != "\\N", !is.na(IATA))

glimpse(df_nearby_removeDromes)

##Add a label o the focus airport, for the list of nearby airports
###First create labels feature and then join to focus airports dataframe
labels <- df_nearby_removeDromes |> group_by(source_airport)  |>
    summarise(All_Airports_in_Catchment = paste0(IATA, collapse = ", "),
              .groups = 'drop'
    )

glimpse(labels)  

###Add labels to nearby airports dataframe

df_focus_airports_EU <- left_join(df_focus_airports_EU, labels, join_by(origin_code == source_airport))

df_focus_airports_EU$popups <- paste0(
    "<b>Focus Airport:</b> ", df_focus_airports_EU$origin_code, "<br>",
    "<b>Focus Airport Size (PAX):</b> ", df_focus_airports_EU$TPAX, "<br>",
    "<b>Nearby Airports:</b> ", df_focus_airports_EU$All_Airports_in_Catchment, "<br>"
)

glimpse(df_focus_airports_EU)

############APP#############

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Airport Catchment Areas - Direct Radius"),
    
    #Application description
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("This app shows which other EU airports are within the selected radius of EU airports <1 million passengers per year. Each blue circle is an EU airport with <1 million passengers in 2024."),
            numericInput("radius",
                         "Radius in Kilometers:",
                         min = 10,
                         max = 250,
                         value = 100) #100km is 100,000m
        ),
      
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map", width = "100%", height = 600)
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$map <- renderLeaflet({
        
        df_focus_airports_EU_with_radius <- df_focus_airports_EU |>
            dplyr::mutate(
                radius = input$radius
            )
        
        leaflet() |>
            addTiles() |>
            addCircles(
                data = df_focus_airports_EU_with_radius,
                lng = ~Longitude,
                lat = ~Latitude,
                color = "blue",
                popup = ~popups,
                radius = ~radius * 1000
            ) |>
            addCircles(
                data = df_nearby_removeDromes,
                lng = ~Longitude,
                lat = ~Latitude,
                color = "yellow",
                label = ~IATA
            )
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
