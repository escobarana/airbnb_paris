# * Install and load the Shiny package *
# install.packages("shinyjs", dependencies=TRUE)
# devtools::install_github("rstudio/EDAWR")
library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinycssloaders)
library(dplyr)
library(EDAWR)
library(tidyr)
library(stringr)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(ggpubr)
library(RColorBrewer)
 
# 1. Load the AirBnB data ------------------------------------------------------
load("AirBnB.RData")

# 2. Clean the AirBnB data -----------------------------------------------------
## 2.1 Select only the necessary variables and rename if necessary -------------
data <- select(L, listing_id = id, host_id, host_name, bathrooms, bedrooms, 
               beds, bed_type, equipments= amenities, type= property_type, room= room_type, 
               nb_of_guests= accommodates, price, guests_included, minimum_nights, 
               maximum_nights,availability_over_one_year= availability_365, instant_bookable, 
               cancellation_policy, city, address= street, neighbourhood=neighbourhood_cleansed, 
               city_quarter=zipcode, latitude, longitude, security_deposit, transit, 
               host_response_time, superhost= host_is_superhost, host_since, 
               listing_count= calculated_host_listings_count, host_score= review_scores_rating, 
               reviews_per_month, number_of_reviews)

## 2.2 Remove duplicates by listing_id , if any --------------------------------
data %>% distinct(listing_id, .keep_all = TRUE)

## 2.3 Removing the '$' character in the price to later convert it to numeric --
data$price <- substring(gsub(",", "", as.character(data$price)),2)

## 2.4 Convert the necessary data types as numeric -----------------------------
data$bathrooms <- as.numeric((data$bathrooms))
data$bedrooms <- as.numeric((data$bedrooms))
data$beds <- as.numeric((data$beds))
data$price <- as.numeric((data$price))
data$guests_included <- as.numeric((data$guests_included))
data$minimum_nights <- as.numeric((data$minimum_nights))
data$maximum_nights <- as.numeric((data$maximum_nights))
data$availability_over_one_year <- as.numeric((data$availability_over_one_year))
data$security_deposit <- as.numeric((data$security_deposit))
data$listing_count <- as.numeric((data$listing_count))
data$host_score <- as.numeric((data$host_score))
data$reviews_per_month <- as.numeric((data$reviews_per_month))
data$number_of_reviews <- as.numeric((data$number_of_reviews))

## 2.5 Convert the necessary data types as characters --------------------------
data$neighbourhood <- as.character(data$neighbourhood)

## 2.6 Rename the names of neighborhoods that are written incorrectly ----------
data[data == "PanthÃ©on"] <- "Panthéon"
data[data == "OpÃ©ra"] <- "Opéra"
data[data == "EntrepÃ´t"] <- "Entrepôt"
data[data == "Ã‰lysÃ©e"] <- "Elysée"
data[data == "MÃ©nilmontant"] <- "Mesnilmontant"
data[data == "HÃ´tel-de-Ville"] <- "Hôtel-de-Ville"

## 2.7 Bathroom, bedrooms and beds have missing values,  ----------------------
## fill its missing values with the mean value
### 2.7.1 Bathrooms variable
temp = mean(data$bathrooms, na.rm = TRUE) 
val = is.na(data$bathrooms) 
data$bathrooms[val] = temp

### 2.7.2 Bedrooms variable
temp = mean(data$bedrooms, na.rm = TRUE)
val = is.na(data$bedrooms)
data$bedrooms[val] = temp

### 2.7.3 Beds variable
temp = mean(data$beds, na.rm = TRUE) 
val = is.na(data$beds)
data$beds[val] = temp

## 2.8 Build city quarters ("arrondissements") ---------------------------------
data$city = str_sub(data$city, 1, 5)
data$city_quarter = str_sub(data$city_quarter, -2)

## 2.9 Work only with the observations of the data that don't have an empty city_quarter
data <- subset(data, data$city_quarter != "" & data$city_quarter != '00' & data$city_quarter != ' ')

## 2.10 Compute visit frequency of the different quarters according to time ----
temp <- inner_join(data, R, by = "listing_id")
temp = mutate(temp, year = as.numeric(str_extract(temp$date, "^\\d{4}")))

## 2.11 Compute number of apartments per host ----------------------------------
count_by_host_1 <- data %>% 
    group_by(host_id) %>%
    summarise(number_apt_by_host = n()) %>%
    ungroup() %>%
    mutate(groups = case_when(
        number_apt_by_host == 1 ~ "001",
        between(number_apt_by_host, 2,10) ~ "002-010",
        number_apt_by_host > 10 ~ "011-153"))

count_by_host_2 <- count_by_host_1 %>%
    group_by(groups) %>%
    summarise(counting = n() %>%
    sort(number_apt_by_host,decreasing = T)) # order by nb of apt per host descending

count_by_host_3 <- data %>%
  group_by(host_id) %>%
  summarise(number_apt_by_host = n()) %>%
  arrange(desc(number_apt_by_host))

top_listings_by_host <- count_by_host_3 %>%
  top_n(n=20, wt = number_apt_by_host)

knit_print.data.frame <- top_listings_by_host


## 2.12 List by property type --------------------------------------------------
whole_property_type_count <- table(data$type)
property_types_counts <- table(data$type,exclude=names(whole_property_type_count[whole_property_type_count[] < 4000]))

count_of_others <- sum(as.vector(whole_property_type_count[whole_property_type_count[] < 4000]))
property_types_counts['Others'] <- count_of_others
property_types <- names(property_types_counts)
counts <- as.vector(property_types_counts)
percentages <- scales::percent(round(counts/sum(counts), 2))
property_types_percentages <- sprintf("%s (%s)", property_types, percentages)
property_types_counts_df <- data.frame(group = property_types, value = counts)


## 2.13 Average price per neighborhood -----------------------------------------

average_prices_per_arrond <- aggregate(cbind(data$price),
                                       by = list(arrond = data$city_quarter),
                                       FUN = function(x) mean(x))

# 3. Build maps ----------------------------------------------------------------
## Build dataframe from the dataset the necessary variables to build the map
df <- select(data,longitude, neighbourhood, latitude, price)
## Remove the 'metadata' from previous dataframe
df %>% select(longitude, neighbourhood, latitude, price)

## Superhost dataframe 
dfsuperhost <- select(data, longitude, neighbourhood, latitude, price)
dfsuperhost <- filter(data, superhost == "t")


# 4. Building the shiny app ---------------------------------------------------

## 4.1 Define the UI ----------------------------------------------------------
ui <- fluidPage(
    
    # 4.1.1 Add a title
    titlePanel("Exploring the AirBnB Data for Paris"),
    
    # 4.1.2 Add a sidebar with widgets for selecting features to explore
    dashboardSidebar(
        
        sidebarMenu(
            
            menuItem("Prices vs Apartments",tabName="prices_apartments", icon=icon("money-bill-1-wave")),
            
            menuItem("Apartments per Owner",tabName="apartments_owners", icon=icon("house-chimney-user")),
            
            menuItem("Price per Quarter",tabName="arrondissements", icon=icon("dashboard")),
            
            menuItem("Visit Frequency",tabName="visits", icon=icon("wave-square")),
            
            menuItem("Maps",tabName="map", icon=icon("map"))
        )),
    
    
    dashboardBody(
      
      # 4.1.3 A quick overview of the data
      fluidRow(tags$head(tags$style(HTML(".small-box {height: 100px}"))),
               valueBox("Paris", "France", icon = icon("location-pin"), width = 3),
               valueBoxOutput("mean_price", width = 3),
               valueBoxOutput("nb_superhosts", width = 3),
               valueBoxOutput("count_listings", width = 3)),
        
      # 4.1.4 Build the dashboards on the corresponding menu item
      tabItems(
          
          tabItem(tabName ="prices_apartments",
            fluidRow(
                box(title= "Listings by room type", 
                    width = 6,
                    plotOutput("room_type")%>% withSpinner(color="#971a4a"), 
                    status = "primary", 
                    solidHeader = FALSE, 
                    collapsible = TRUE),
                
                box(title= "Listings by property type",
                    width =6,
                    plotOutput("property_type")%>% withSpinner(color="#971a4a"), 
                    status = "primary", 
                    solidHeader = FALSE, 
                    collapsible = TRUE)),
            fluidRow(
                box(title="Avg. price according to room type", 
                    plotOutput("price_room")%>% withSpinner(color="#971a4a"), 
                    width=6, 
                    solidHeader = FALSE, 
                    collapsible = TRUE ),
                
                  box(title= "Top 10 neighborhoods in Paris",
                    width =6,
                    plotlyOutput("top10_nh")%>% withSpinner(color="#971a4a"), 
                    status = "primary", 
                    solidHeader = FALSE, 
                    collapsible = TRUE)
            )
          ),  
          
          tabItem(tabName ="apartments_owners",
            fluidRow(
                box(title= "Apartments per host", 
                    width = 6,  
                    plotOutput("nb_apartments")%>% withSpinner(color="#971a4a"), 
                    status = "primary", 
                    solidHeader = FALSE, 
                    collapsible = TRUE),
                
                box(title= "Hosts vs Superhosts",  
                    plotOutput("superhosts")%>% withSpinner(color="#971a4a"), 
                    status = "primary", 
                    solidHeader = FALSE, 
                    width = 6, 
                    collapsible = TRUE)
            ),

            fluidRow(
                box(title = "Top 20 hosts in Paris", 
                    width=12, 
                    status= "success",
                    solidHeader = FALSE, 
                    collapsible = TRUE,
                    DTOutput('top20_hosts')))
          ),
          
          tabItem(tabName ="arrondissements",
                  
            fluidRow(
              box(title= "Avg. price per neighborhood",
                  width = 12, 
                  plotlyOutput("avg_price_nh")%>% withSpinner(color="#971a4a"), 
                  status = "success", 
                  solidHeader = FALSE, 
                  collapsible = TRUE)),
            
            fluidRow(
              box(title= "Rented apartments in the past years",
                  width = 12, plotOutput("nb_rented")%>% withSpinner(color="#971a4a"), 
                  status = "success", 
                  solidHeader = FALSE, 
                  collapsible = TRUE)),
            
            fluidRow(
              box(title= "Map representing price range within Paris neighborhoods", 
                  width = 12, 
                  plotOutput("price_range_nh")%>% withSpinner(color="#971a4a"),
                  status = "success", 
                  solidHeader = FALSE, 
                  collapsible = TRUE),
            )),
          
          tabItem(tabName ="visits",
            fluidRow(
                box(title= "Listings by neighborhood and room type",
                    plotlyOutput("listings_nh")%>% withSpinner(color="#971a4a"), 
                    width = 12, 
                    status = "success", 
                    solidHeader = FALSE, 
                    collapsible = TRUE)),
            
            fluidRow(  
                box(title= "Frequency of visits in the past years", 
                    plotlyOutput("freq_visit")%>% withSpinner(color="#971a4a"), 
                    width = 12, 
                    status = "success", 
                    solidHeader = FALSE, 
                    collapsible = TRUE)
            ),
          ),
          
          tabItem(tabName ="map",
            fluidPage(
                box(title = "All listings", 
                    width = 12,
                    leafletOutput("all_map"), 
                    status = "success", 
                    solidHeader = FALSE, 
                    collapsible = TRUE),
                
                box(title = "Superhosts listings", 
                    width = 12,
                    leafletOutput("superhost_map"), 
                    status = "success", 
                    solidHeader = FALSE, 
                    collapsible = TRUE)
            ))
        ) 
    )  
)

## 4.2 Define the server  -----------------------------------------------------

server <- function(input, output) {
    
    # 4.2.1 Create a Shiny alert
    shinyalert("DSTI Project for Big Data Processing with R course",
               "by Ana Escobar Llamazares - S22")
    
    # 4.2.2 Output variables; define value boxes
    output$mean_price <- renderValueBox({
        valueBox(
            round(mean(data$price),0), 
            "Mean Price", 
            icon = icon("hand-holding-dollar")
        )
    })
    
    output$nb_superhosts <- renderValueBox({
        valueBox(
            sum(data$superhost == "t"), 
            "Superhosts", 
            icon = icon("user-check")
        )
    })
    
    output$count_listings <- renderValueBox({
        valueBox(
            nrow(data), 
            "Listings", 
            icon = icon("list")
        )
    })
    
    # 4.2.3 Define the graphs
    ## 4.2.3.1 Listings by room type 
    output$room_type <- renderPlot ({
      room_types_counts <- table(data$room)
      room_types <- names(room_types_counts)
      counts <- as.vector(room_types_counts)
      percentages <- scales::percent(round(counts/sum(counts), 2))
      room_types_percentages <- sprintf("%s (%s)", room_types, percentages)
      room_types_counts_df <- data.frame(group = room_types, value = counts)
      
      ggplot(room_types_counts_df, 
             aes(x = "", 
                 y = value, 
                 fill = room_types_percentages)
             )+
        geom_bar(width = 1, stat = "identity")+
        coord_polar("y", start = 0)+
        scale_fill_brewer("Room types", palette ="BuPu")+
        ylab("")+
        xlab("")+
        labs(fill="")+
        geom_text(aes(label = percentages), 
                  size = 4, 
                  position = position_stack(vjust = 0.5))+
        theme_void()
    })
    
    ## 4.2.3.2 Listings by property type
    output$property_type <- renderPlot ({
      ggplot(property_types_counts_df, aes(x="",y = value, fill=property_types_percentages))+
        geom_bar(width = 1,stat = "identity")+
        coord_polar("y",start = 0)+
        scale_fill_brewer("Property types", palette ="BuPu")+
        ylab("")+
        xlab("")+
        labs(fill="")+
        geom_text(aes(label = percentages),size= 4 ,position = position_stack(vjust = 0.5))+
        theme_void()
      
    })
    
    ## 4.2.3.3 Mean price by room type
    output$price_room <- renderPlot ({
      data %>% 
        
        group_by(room) %>% 
        summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
        ggplot(aes(x = reorder(room, mean_price), y = mean_price, fill = room)) +
        geom_col(stat ="identity", fill="#971a4a") +
        coord_flip() +
        theme_minimal()+
        labs(x = "Room type", y = "Price") +
        geom_text(aes(label = round(mean_price,digit = 2)), hjust = 1.0, color = "white", size = 3.5) +
        
        xlab("Room type") + 
        ylab("Mean price")
    })
    
    ## 4.2.3.4 Top 10 neighborhoods
    output$top10_nh <- renderPlotly ({
      p30<- data %>%
        group_by(neighbourhood) %>%
        dplyr::summarize(num_listings = n(), 
                         borough = unique(neighbourhood)) %>%
        top_n(n = 10, wt = num_listings) %>%
        ggplot(aes(x = fct_reorder(neighbourhood, num_listings), 
                   y = num_listings, fill = borough)) +
        scale_fill_brewer(palette ="BuPu")+
        geom_col() +
        coord_flip() +
        theme(legend.position = "none") +
        labs(x = "Neighborhood", y = "Nb. of listings")
      
      ggplotly(p30)
    })
    
    ## 4.2.3.5 Nb. of Apartments by host
    output$nb_apartments <- renderPlot ({
      ggplot(count_by_host_2, aes(x = "", y = counting)) +  
        geom_col(aes(fill = factor(groups)),color = "white")+
        geom_text(aes(y = counting / 1.23, label = counting),
                  size = 3)+
        labs(x = "", y = "", fill = "Number of apartments per host")+
        scale_fill_brewer(palette ="BuPu") +
        coord_polar(theta = "y")+
        theme_void()
    })
    
    ## 4.2.3.6 Nb. of superhosts
    output$superhosts <- renderPlot ({
      ggplot(data) +
        geom_bar(aes(x='' , fill=superhost)) +
        coord_polar(theta='y') +
        scale_fill_brewer(palette ="BuPu")+
        theme_void()
      
    })
    
    ## 4.2.3.7 Top 20 hosts
    output$top20_hosts <- renderDT(
      top_listings_by_host, 
      options = list(searching = FALSE,
                     pageLength = 10
      ))
    
    ## 4.2.3.8 Avg. price by Neighborhood 
    output$avg_price_nh <- renderPlotly ({
      p3 <- ggplot(data = average_prices_per_arrond, aes(x = arrond, y = V1))+
        geom_bar(stat = "identity", fill = "#971a4a", width = 0.7)+
        geom_text(aes(label = round(V1, 2)), size=4)+
        coord_flip()+
        labs(
          x = "City quarters", y = "Average daily price")+
        theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))+
        scale_fill_brewer(palette ="BuPu") 
      ggplotly(p3)
    })
    
    ## 4.2.3.9 Nb. of rented apartments 
    output$nb_rented <- renderPlot ({
      temp["date"] <- temp["date"] %>% map(., as.Date)

      longitudinal  <- temp %>% 
        group_by(date, neighbourhood) %>% 
        summarise(count_obs = n())
      
      ggplot(longitudinal,aes(x = date,y = count_obs,group = 1))+ 
        geom_line(size = 0.5,colour = "#FF5AAC") +  
        stat_smooth(color = "#971a4a",method = "loess")+  
        scale_x_date(date_labels = "%Y")+  
        labs(x = "Year",y = "Nb. rented apartment")+  
        facet_wrap(~ neighbourhood)
      
    })
    
    
    ## 4.2.3.10 Price range by neighborhood 
    output$price_range_nh <- renderPlot({
      height <- max(data$latitude) - min(data$latitude)
      width <- max(data$longitude) - min(data$longitude)
      Paris_borders <- c(bottom  = min(data$latitude)  - 0.1 * height, 
                         top     = max(data$latitude)  + 0.1 * height,
                         left    = min(data$longitude) - 0.1 * width,
                         right   = max(data$longitude) + 0.1 * width)
      map <- get_stamenmap(Paris_borders, zoom = 12)
      p8<- ggmap(map) +
        geom_point(data = data, 
                   mapping = aes(x = longitude, y = latitude, col = log(price))) +
        scale_color_distiller(palette ="BuPu", direction = 1)
      p8
    })
    
    ## 4.2.3.11 Nb. of listings by neighborhood
    output$listings_nh <- renderPlotly({
      x <- ggplot(data, aes(x = fct_infreq(neighbourhood), fill = room)) +
        geom_bar() +
        labs(x = "Neighborhood", y = "Nb. of listings")+
        scale_fill_brewer(palette ="BuPu") +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
      ggplotly(x)
    })
    
    ## 4.2.3.12 Visit frequency
    output$freq_visit <- renderPlotly ({
      p6 <- ggplot(temp) +
        geom_bar(aes(y =city_quarter,
                     fill=factor(year)))+
        scale_size_area() +
        labs( x="Frequency", y="City Quarter", fill="Year")+
        scale_fill_brewer(palette ="BuPu")
      
      ggplotly(p6)
    })
    
    ## 4.2.3.12 All listings map
    output$all_map <- renderLeaflet ({
        leaflet(df) %>%  
            setView(lng = 2.3488, lat = 48.8534, zoom = 10) %>%
            addTiles() %>% 
            addMarkers(clusterOptions = markerClusterOptions()) %>%
            addMiniMap()
    })
    
    ## 4.2.3.13 Superhost listings map
    output$superhost_map <- renderLeaflet ({
        leaflet(dfsuperhost %>% select(longitude,neighbourhood,
                                       latitude,price))%>%
            setView(lng = 2.3488, lat = 48.8534, zoom = 10) %>%
            addTiles() %>% 
            addMarkers(clusterOptions = markerClusterOptions()) %>%
            addMiniMap()
    })
}

## 4.3 Run the Shiny App ------------------------------------------------------
shinyApp(ui, server)
