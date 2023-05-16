## ----setup, include=FALSE-----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(cache = FALSE, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE)

## -----------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(shiny)
library(maps)
library(scales)
library(tsibble)
library(dplyr)
library(shinythemes)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(lubridate)
library(ggplot2)
library(plotly)
library(forcats)
library(mapproj)


## -----------------------------------------------------------------------------------------------------------------------
# Create data frame of ID, STATE NAME pairs from: https://eric.clst.org/tech/usgeojson/ -> U.S. States geojson
us_states_state_id_mapping = read_sf("https://uwmadison.box.com/shared/static/vnih15dvap3ic7c1ddqdbouhazjruqhp.json") %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  select(STATE, NAME) %>% 
  filter(NAME != "Hawaii" & NAME != "Alaska" & NAME != "Puerto Rico")

# Join the state name on each county ID
us_counties = read_sf("https://uwmadison.box.com/shared/static/qis14d5xz0s9q8jvyqrlxmzdh5k8wzl9.json") %>% 
  as.data.frame() %>% 
  rename(COUNTY_NAME = NAME) %>% 
  left_join(us_states_state_id_mapping, by="STATE") 

# Preprocess/Clean U.S. counties geojson
# us_counties DF columns: 
# OBJECTID (chr), STATE (chr), COUNTY (chr), COUNTY_NAME (chr), STATE_NAME (chr), geometry (sfc_MULTIPOLYGON)
us_counties = us_counties %>% 
  rename(STATE_NAME = NAME) %>% 
  select(-LSAD, -CENSUSAREA) %>% 
  rename(OBJECTID = GEO_ID) %>%
  filter(STATE_NAME != "Hawaii" & STATE_NAME != "Alaska" & STATE_NAME != "Puerto Rico") %>% 
  st_as_sf()

# Read in csv file of all (COUNTY, STATE) rental data: scraped with Python Selenium script from rentdata.org
# Script located here: <paste box link to script>
# all_rental_data DF columns:
# STATE_NAME (chr), Year (num), COUNTY_NAME (chr), 0BR (num), 1BR (num), 2BR (num), 3BR (num), 4BR (num)   
all_rental_data <- read_csv("https://uwmadison.box.com/shared/static/143sr11feqgi5pqahlv9pn00ku7swsna.csv") %>% 
  mutate_at(vars(`0BR`: `4BR`), as.numeric) %>% 
  rename(COUNTY_NAME = County,
         STATE_NAME = State)

# Save U.S. County Data in sf format to DF
us_counties_df <- as.data.frame(us_counties)

# us_counties_all is a DF object with columns:
# STATE_NAME (chr), COUNTY_NAME (chr), OBJECTID (chr), STATE (chr), COUNTY (chr), geometry (sfc_MULTIPOLYGON), 
# Year (num), 0BR (num), 1BR (num), 2BR (num), 3BR (num), 4BR (num)
us_counties_all <- merge(us_counties_df, all_rental_data, 
                            by.x = c("STATE_NAME", "COUNTY_NAME"), 
                            by.y = c("STATE_NAME", "COUNTY_NAME"))

# Create a DF of only rental data for 2021, used for County Analysis interactive Leaflet plot
us_counties_2021 <- us_counties_all %>% 
  filter(Year == 2021)
  
# Save 2021 U.S. County Rental Data as sf
us_counties_2021_geojson <- st_as_sf(us_counties_2021)

# Create a DF of mainland U.S. States (not DC) for the "Select a state" selectInput
available_states <- us_states_state_id_mapping %>% 
  arrange(NAME) %>% 
  filter(NAME != "District of Columbia") %>% 
  select(NAME) %>% 
  rename(`Available States` = NAME)


## -----------------------------------------------------------------------------------------------------------------------
# Function to compute normalized column vector (max value = 100, min value = 1)
normalize <- function(vec) { return((vec-min(vec))/(max(vec)-min(vec))*100) }


## -----------------------------------------------------------------------------------------------------------------------
# State population data
state_pop_2022 = read_csv("https://uwmadison.box.com/shared/static/z4zms2iesc8rr6lov8x3p74or2q7syyp.csv") %>% 
  mutate(`Population` = `Population`*1e6)

# Number of hospitals/state
hospitals <- read_csv("https://uwmadison.box.com/shared/static/pywujigfcjq83ka272igh6j1kn03ssqv.csv") %>%
  mutate(State = str_remove(State, "^\\w+\\s+-\\s+")) %>% # remove "abbrev -" from State col
  left_join(state_pop_2022) %>%                           # join state population data set
  mutate(`People Per Hospital` = `Population` / `Number Hospitals`) %>% # create new people per hospital col
  mutate(State = tolower(State)) %>%  # make the state name lower case
  drop_na() %>%                       #remove missing values
  select(`State`, `People Per Hospital`) %>% # change ordering
  filter(`State` != "hawaii" & `State` != "alaska") %>% 
  mutate(scaled_count = normalize(`People Per Hospital`) * -1)


## -----------------------------------------------------------------------------------------------------------------------
# education PPCS per state
education_spending_ppcs <- read_csv("https://uwmadison.box.com/shared/static/abtigdtpzkckdh4ivcx5vftck9uyov30.csv") %>% 
  rename(State = `Geographic area`) %>% 
  select(`State`, `PPCS`) %>% 
  drop_na() %>% 
  mutate(State = gsub("\\.", "", State)) %>%
  mutate(State = tolower(State)) %>% 
  filter(`State` != "hawaii" & `State` != "alaska") %>% 
  mutate(scaled_ppc = normalize(PPCS))


## -----------------------------------------------------------------------------------------------------------------------
# Realtor.com per state data set
realty_data <- read_csv("https://uwmadison.box.com/shared/static/r9kw6nyro7n0p3dr40q0zn7g3eyaw490.csv") 

# Preprocess realty data
realty_data <- realty_data %>% 
  mutate(month = as.Date(paste0(realty_data$month, "01"), format = "%Y%m%d")) %>%
  mutate(date = as.Date(paste0(realty_data$month_date_yyyymm, "01"), format = "%Y%m%d")) %>% # new
  mutate(year = format(realty_data$date, "%Y")) %>% # new
  mutate(format(realty_data$date, "%Y")) %>%
  rename(State= state) %>% 
  mutate(State = tolower(State)) %>% 
  filter(`State` != "hawaii" & `State` != "alaska")

# Initialize DF with State median listing price in Jan 2023
median_listing_price <- realty_data %>% 
  filter(month_date_yyyymm == 202301) %>% 
  select(State, median_listing_price) %>% 
  mutate(median_listing_price_scaled = normalize(median_listing_price))

# Initialize DF with State median days on the market in Jan 2023
median_listing_days <- realty_data %>% 
  filter(month_date_yyyymm == 202301) %>% 
  select(State, median_days_on_market) %>% 
  mutate(median_days_on_market_scaled = normalize(median_days_on_market) * -1)


## -----------------------------------------------------------------------------------------------------------------------
# Function to compute normalized weighted sum of all input features. Ta
compute_weighted_sum <- function(education_spending_weight, citizens_per_hospital_weight, median_home_prices_weight, median_listing_days_weight){
  education_weighted_data <- education_spending_ppcs %>%
      mutate(education_weighted = as.numeric(scaled_ppc) * education_spending_weight) %>% 
      select(`State`, education_weighted)
    
    hospital_weighted_data <- hospitals %>%
      mutate(hospital_weighted = as.numeric(scaled_count) * citizens_per_hospital_weight) %>% 
      select(`State`, hospital_weighted)

    median_home_prices_weighted_data <- median_listing_price %>%
      mutate(median_home_prices_weighted = as.numeric(median_listing_price_scaled) * median_home_prices_weight) %>% 
      select(`State`, median_home_prices_weighted)
    
    median_listing_days_weighted_data <- median_listing_days %>%
      mutate(median_listing_days_weighted = as.numeric(median_days_on_market_scaled) * median_listing_days_weight) %>% 
      select(`State`, median_listing_days_weighted)
    
    weighted_sum_data <- education_weighted_data %>%
      filter(`State` != "district of columbia") %>% 
      left_join(hospital_weighted_data, by = "State") %>% 
      left_join(median_home_prices_weighted_data, by = "State") %>%
      left_join(median_listing_days_weighted_data, by = "State")
  
    weighted_sum_data <- weighted_sum_data %>%
      mutate(weighted_sum = education_weighted + hospital_weighted + median_home_prices_weighted + median_listing_days_weighted)
    
    normalized_weighted_data <- weighted_sum_data %>%
      mutate(total_normalized_weight = (weighted_sum - min(weighted_sum)) / (max(weighted_sum) - min(weighted_sum))) %>% 
      select(`State`, total_normalized_weight)

    return(normalized_weighted_data)
}

color_scale <- c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59")

# Function for creating map plot
create_map_plot <- function(map_data, fill_var, title, low_is_good = TRUE) {
  par(mar = c(0, 0, 0, 0))
  ggplot(data=map_data, aes(x = long, y = lat, group = group, fill = !!sym(fill_var))) +
    geom_polygon(color = "black") +
    scale_fill_gradientn(
      colors = if (low_is_good) color_scale else rev(color_scale),
      guide = guide_colorbar(
        title = title,
        ticks = FALSE,
        barwidth = 3,
        title.theme = element_text(color = "white")
      )
    ) +
    coord_map() +
    theme_void() +
    theme(panel.background = element_rect(fill = "#272B30", colour = NA),
          plot.background = element_rect(fill = "#272B30", colour = NA),
          legend.text = element_text(color = "white")
    )
}

# Function for creating median price plot
create_median_price_plot <- function(data){
  data$month_date_yyyymm <- ym(data$month_date_yyyymm)
  data = data %>% 
    filter(is.finite(median_listing_price))
  ggplot(data, aes(x = date, y = median_listing_price / 1000, group = 1, color = median_listing_price)) +
    geom_line(size = 1.2, alpha = 0.7) + 
    scale_color_gradientn(colors = color_scale) +
    scale_x_date(breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, max(data$median_listing_price / 1000) + 50, by = 50)) +
    facet_wrap(~reorder(year, -year), ncol = 1, scales = "free_x") +
    ggtitle(paste0("Median Listing Price in ", data$State[1])) +
    labs(x = "Year",
         y = "Median Listing Price (in Thousands)") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = "white"),
          plot.background = element_rect(fill = "#272B30", colour = NA),
          plot.title = element_text(size = 17),
          legend.position = "none",
          text = element_text(color = "snow"),
          axis.text = element_text(color = "snow3", size = 10),
          axis.title = element_text(size = 14),
          strip.background =element_rect(fill="#272B30"),
          strip.text = element_text(colour = "snow"))
}

# Function for creating trhe median days plot
create_median_days_plot <- function(data){
  data$month_date_yyyymm <- ym(data$month_date_yyyymm)
  ggplot(data, aes(x = month_date_yyyymm, y = median_days_on_market, group = 1, color = median_days_on_market)) +
    geom_line(size = 1.5, alpha = 0.7) +
    scale_color_gradientn(colors = color_scale) +
    scale_x_date(breaks = "1 month", date_labels = "%b") +
    facet_wrap(~reorder(year, -year), ncol = 1, scales = "free_x") +
    labs(x = "Month", y = "Median Days on Market") +
    ggtitle(paste0("Seasonal Median Days on the Market in ", data$State[1])) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = "white"),
          plot.background = element_rect(fill = "#272B30", colour = NA),
          plot.title = element_text(size = 17),
          legend.position = "none",
          text = element_text(color = "snow"),
          axis.text = element_text(color = "snow3", size = 10),
          axis.title = element_text(size = 14),
          strip.background =element_rect(fill="#272B30"),
          strip.text = element_text(colour = "snow"))
}

create_median_days_boxplot <- function(data){
  data$month_date_yyyymm <- as.Date(paste0(data$month_date_yyyymm, "01"), format = "%Y%m%d")
  data$year <- as.numeric(substr(data$month_date_yyyymm, 1, 4))
  data$month <- factor(month(data$month_date_yyyymm, label = TRUE), levels = month.abb)
  p <- ggplot(data, aes(x = month, y = median_days_on_market)) +
    geom_boxplot(fill = "#272B30", color = "snow3", whisker.color = "white") +
    labs(x = "Month", y = "Median Days on Market") +
    ggtitle(paste0("Box Plot of Median Days on the Market in ", data$State[1]), " (2016-2022)") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = "white"),
          plot.background = element_rect(fill = "#272B30", colour = NA),
          plot.title = element_text(size = 17),
          legend.position = "none",
          text = element_text(color = "snow"),
          axis.text = element_text(color = "snow3", size = 10),
          axis.title = element_text(size = 14),
          strip.background =element_rect(fill="#272B30"),
          strip.text = element_text(colour = "snow"))
  
  ggplotly(p, tooltip = "all")
  
}

create_median_price_boxplot <- function(data){
  data$month_date_yyyymm <- as.Date(paste0(data$month_date_yyyymm, "01"), format = "%Y%m%d")
  data$year <- as.numeric(substr(data$month_date_yyyymm, 1, 4))
  data$month <- factor(month(data$month_date_yyyymm, label = TRUE), levels = month.abb)
  p <- ggplot(data, aes(x = month, y = median_listing_price / 1000)) +
    geom_boxplot(fill = "#272B30", color = "snow3", whisker.color = "white") +
    labs(x = "Month", y = "Median Listing Price (Thousands)") +
    ggtitle(paste0("Box Plot of Median Listing Price in ", data$State[1]), " (2016-2022)") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = "white"),
          plot.background = element_rect(fill = "#272B30", colour = NA),
          plot.title = element_text(size = 17),
          legend.position = "none",
          text = element_text(color = "snow"),
          axis.text = element_text(color = "snow3", size = 10),
          axis.title = element_text(size = 14),
          strip.background =element_rect(fill="#272B30"),
          strip.text = element_text(colour = "snow"))
  
  ggplotly(p, tooltip = "all")
  
}

create_median_price_scatterplot <- function(data){
  data$month_date_yyyymm <- as.Date(paste0(data$month_date_yyyymm, "01"), format = "%Y%m%d")
  data$year <- as.numeric(substr(data$month_date_yyyymm, 1, 4))
  data$month <- factor(month(data$month_date_yyyymm, label = TRUE), levels = month.abb)
  data$month_year <- format(data$month_date_yyyymm, "%b %Y")
  
  # Calculate the linear regression model
  model <- lm(median_listing_price ~ month_date_yyyymm, data = data)
  slope <- round(coef(model)[2], 3)
  
  p <- ggplot(data, aes(x = month_date_yyyymm, y = median_listing_price / 1000)) +
    geom_point(aes(text = paste("Observed Median Listing Price (Thousands):", round(median_listing_price / 1000, 2))), color = "snow3", size = 2) +
    geom_smooth(method = "lm", se = FALSE, linetype = "solid", color = "lightblue", aes(text = paste("Median Listing Price Change Per Month: $", slope))) +
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y", expand = c(0, 0)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Month and Year", y = "Median Listing Price (Thousands)") +
    ggtitle(paste0("Scatterplot of Median Listing Price in ", data$State[1]), " (2016-2022)") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = "white"),
          plot.background = element_rect(fill = "#272B30", colour = NA),
          plot.title = element_text(size = 17),
          legend.position = "none",
          text = element_text(color = "snow"),
          axis.text = element_text(color = "snow3", size = 10),
          axis.title = element_text(size = 14),
          strip.background =element_rect(fill="#272B30"),
          strip.text = element_text(colour = "snow"))
  
  ggplotly(p, tooltip = "text")
}



create_bar_plot <- function(data, var, var_name) {
  p <- ggplot(data, aes(x = State, y = !!sym(var), fill = !!sym(var))) +
    geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.2), show.legend = FALSE) + # adjust width and position
    scale_y_continuous(limits = c(0, max(data[[var]], na.rm = TRUE) * 1.1), expand = c(0, 0)) +
    coord_flip() + # Flip the x and y axis
    scale_x_discrete(limits = data$State[order(data[[var]])]) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14), # increase text size
          axis.text = element_text(color = "white", size = 10),
          axis.title = element_text(color = "white", size = 14),
          axis.text.x = element_text(color = "white", size = 12, hjust = 1), # increase text size and adjust spacing
          axis.text.y = element_text(color = "white", size = 12), # increase text size
          plot.title = element_text(color = "white", size = 20)) + # increase text size
    labs(title = paste("Top States by", var_name), x = "State", y = var_name) +
    scale_fill_gradientn(colors = color_scale)
  
  ggplotly(p, tooltip = c("y")) %>%
    layout(plot_bgcolor = 'rgba(0,0,0,0)', 
           paper_bgcolor = 'rgba(0,0,0,0)')
}


# CALI
cali_leaflet_plot <- function(bedrooms) {
  # Convert the character data to numeric
  california_geojson[[bedrooms]] <- california_geojson[[bedrooms]]
  
  # Create a color palette based on the numeric values
  color_palette <- colorNumeric(palette = "viridis", domain = california_geojson[[bedrooms]])
  
  map <- leaflet(california_geojson) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~color_palette(california_geojson[[bedrooms]]),
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.5,
      layerId = ~OBJECTID
    )
  
  # Add a legend for the color scale
  map %>% addLegend(
    "bottomright",
    title = bedrooms,
    pal = color_palette,
    values = ~as.numeric(california_geojson[[bedrooms]]),
    opacity = 1
  )
}

# ALL STATES
state_leaflet_plot <- function(bedrooms, optional_state_selection) {
  color_palette <- colorNumeric(palette = "viridis", domain = us_counties_2021_geojson[[bedrooms]])
  
  us_counties_2021_geojson_copy <- st_as_sf(us_counties_2021_geojson)
  
  if (optional_state_selection != "All") {
    us_counties_2021_geojson_copy = us_counties_2021_geojson_copy %>% 
      filter(STATE_NAME == optional_state_selection)
  }
  
  # Convert the character data to numeric
  map <- leaflet(data = us_counties_2021_geojson_copy, options = leafletOptions(zoomControl = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>% 
    addTiles() %>%
    addPolygons(
      fillColor = ~color_palette(us_counties_2021_geojson_copy[[bedrooms]]),
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.5,
      layerId = ~OBJECTID
    )
  
  # Add a legend for the color scale
  map %>% addLegend(
    "bottomright",
    title = bedrooms,
    pal = color_palette,
    values = ~as.numeric(us_counties_2021_geojson_copy[[bedrooms]]),
    opacity = 1
  )
}

create_bedrooms_line_plot <- function(county_state_name, bedrooms) {
  # Subset the data by county and select the bedrooms column
  county_data <- us_counties_all %>%
    filter(COUNTY_NAME == county_state_name[1]$COUNTY_NAME & STATE_NAME == county_state_name[2]$STATE_NAME) %>% 
    pivot_longer(
      cols = contains(c("0", "1", "2", "3", "4", "5")),
      names_to = "bedrooms",
      values_to = "rent_prices"
    ) %>%
    mutate(bedrooms = factor(bedrooms, levels = rev(unique(bedrooms))))
  
  plot <- ggplot(county_data, aes(x = Year, y = rent_prices, color = bedrooms)) +
    geom_line(size = 0.5) +
    xlab("Year") +
    ylab("Rent ($)") +
    ggtitle(paste0("Rent Prices for ", county_state_name[1]$COUNTY_NAME))+
    scale_color_discrete(name = "# of Bedrooms") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = "white"),
          plot.background = element_rect(fill = "#272B30", colour = NA),
          plot.title = element_text(size = 12),
          text = element_text(color = "snow"),
          axis.text = element_text(color = "snow3", size = 10),
          axis.title = element_text(size = 14),
          strip.background = element_rect(fill="#272B30"),
          strip.text = element_text(colour = "snow"),
          legend.background = element_rect(fill = "transparent")) 
  
  return(ggplotly(plot))
}

## -----------------------------------------------------------------------------------------------------------------------
# Page Paragraphs
us_analysis_pg = "This interactive visualization allows you to analyze key factors and assist you to make informed, low-risk investments. To start, click a variable of interest from the dropdown menu. The plot below maps your variable of interest on a US map, and if you click on a state the value the state's color is encoding will be presented at the top. Below the state map is a barplot of top states for each variable. For median house prices and median days on the market it will also display supplemental plots to the right and below once a state is clicked. The weighted sum component allows you to rank your importance for the variables presented, and a new map encoding and top states barplot should be presented."

county_analysis_pg = "This interactive visualization allows you to analyze specific counties for high yield investments. To start, select the number of bedrooms and the state of interest. The plot below maps the state at the county level and and colors by the rental price per county. When you click on a county, a line plot of rental prices over time for each number of bedrooms appears on the right."


## -----------------------------------------------------------------------------------------------------------------------
ui <- navbarPage(
  theme = shinytheme("slate"),
  title = "BREG",
  
  # U.S. Analysis (Main Visualization) Tab+Page
  tabPanel(
    "State Analysis",
    mainPanel(
      tags$h2("Badger Real Estate Group", 
        tags$span("Investment Dashboard", style = "color: #BF050C; font-weight: bold;")
      ),
      p(us_analysis_pg),
      uiOutput("selected_state_header"), # Display selected state in header
      textOutput("selected_state_data"), # Display selected state data
      br(),
      uiOutput("map_var_input"),
      # conditional weighted sum sliders
      conditionalPanel(
        condition = "input.map_var == 'Weighted Sum'",
          fluidRow(
            column(
              width = 12,
              div(style = "display:inline-block; width:22%; margin-right:2%",
                  sliderInput("education_spending_weight", "Education Spending Weight:", min = 0, max = 1, step = 0.01, value = 0.25)
              ),
              div(style = "display:inline-block; width:22%; margin-right:2%",
                  sliderInput("citizens_per_hospital_weight", "# Citizens Per Hospital Weight:", min = 0, max = 1, step = 0.01, value = 0.25)
              ),
              div(style = "display:inline-block; width:22%; margin-right:2%",
                  sliderInput("median_home_prices_weight", "Median Listing Prices Weight:", min = 0, max = 1, step = 0.01, value = 0.25)
              ),
              div(style = "display:inline-block; width:22%;",
                  sliderInput("median_listing_days_weight", "Median Listing Days Weight:", min = 0, max = 1, step = 0.01, value = 0.25)
              )
            )
          )
      ),
      textOutput("description"),
      br(),
      actionButton("clear_selection_button", "Clear Selection")
    ),

    fluidRow(
      column(width = 8,
             plotOutput("us_map", click = "plot_click", width = "100%", height = "550px"),
             conditionalPanel(condition = "input.map_var == 'Median Days on the Market' && $('#selected_state_header').text() !== 'Please click on a state.'",
                 plotlyOutput("median_days_box_plot", width = "100%", height = "400px")
              ),
             conditionalPanel(condition = "input.map_var == 'Median Listing Prices' && $('#selected_state_header').text() !== 'Please click on a state.'",
                  tabsetPanel(
                      id = "myTabsetPanel",
                      type = "tabs",
                      tabPanel(
                          "Box",
                          plotlyOutput("median_price_box_plot", width = "100%", height = "400px")
                      ),
                      tabPanel(
                          "Scatter",
                          plotlyOutput("median_price_scatter_plot", width = "100%", height = "400px")
                      )
                  )
              )
      ),
      column(width = 4,
             div(
               style = "text-align: center;",
                conditionalPanel(
                   condition = "input.map_var == 'Median Days on the Market' && $('#selected_state_header').text() !== 'Please click on a state.'",
                   plotOutput("median_days_plot", width = "100%", height = "1000px")
                ),
                conditionalPanel(
                 condition = "input.map_var == 'Median Listing Prices' && $('#selected_state_header').text() !== 'Please click on a state.'", 
                 plotOutput("median_price_plot", width = "100%", height = "1000px")
                ),
             )
      )
    ),
    fluidRow(
      plotlyOutput("state_bar_plot", height = "1000px", width = "90%")
    )
  ), 
  # California Analysis Tab+Page
  tabPanel(
    "County Analysis",
     mainPanel(
    tags$h2("U.S. County", 
      tags$span("Rental Property Analysis", style = "color: #BF050C; font-weight: bold;")
    ),
    p(county_analysis_pg),
      uiOutput("selected_state_county_header"),
    fluidRow(
      column(
        width = 3,
        selectInput("bedrooms", "Select number of bedrooms:",
          choices = c("0BR", "1BR", "2BR", "3BR", "4BR"),
          selected = "1BR")
      ),
      column(
        width = 3,
        selectInput("state_optional_select", "Select a state (Optional):",
          choices = c("All", available_states), 
          selected = "All")
      )
    ),
    fluidRow(
splitLayout(cellWidths = c(900, 550), 
            leafletOutput("us_county_map", width = "100%", height = "500px"),
            conditionalPanel(condition = "$('#selected_state_county_header').text() !== 'Showing All U.S. Counties'", 
                             plotlyOutput("bedrooms_line_plot", width = "100%", height = "600px")
            )
)
,
  ),
     ),
  ),

  # Link to Code + Links to Data
  tabPanel(
    "About",
    mainPanel(
    includeHTML("about.html")
    )
  )
)

server <- function(input, output, session) {
  
  # U.S. ANALYSIS
  # reactive expression initializing weighted sum of all features per state data 
  weighted_sum_data <- reactive({ compute_weighted_sum(input$education_spending_weight, input$citizens_per_hospital_weight, input$median_home_prices_weight, input$median_listing_days_weight)})
  
  # look up table that maps input values to data sets and column names
  data_lookup <- list(
    "Education Spending" = list(data = education_spending_ppcs, var = "PPCS", join_data = education_spending_ppcs, legend = "PPCS (Thousands)"),
    "# Citizens Per Hospital" = list(data = hospitals, var = "People Per Hospital", join_data = hospitals, legend = "# of Citizens (Thousands)"),
    "Median Listing Prices" = list(data = realty_data, var = "median_listing_price", join_data = median_listing_price, legend = "Median Listing Prices (Thousands)"),
    "Median Days on the Market" = list(data = realty_data, var = "median_days_on_market", join_data = median_listing_days, legend = "Median Days on the Market"))
  
  # update the data_lookup list
  data_lookup[["Weighted Sum"]] <- list(data = NULL, var = "weighted_sum", join_data = weighted_sum_data)

  # reactive value to store the selected (clicked) state and its data
  selected_state <- reactiveValues(state = NULL, data = NULL)
  
  # observe event sets the selected state to NULL when the variable select input is changed
  observeEvent(input$map_var, {
    selected_state$state <- NULL
    selected_state$data <- NULL
  })
  
  # observe event sets the selected state to NULL when the "Clear Selection" button is pressed
  observeEvent(input$clear_selection_button, {
    selected_state$state <- NULL
    selected_state$data <- NULL
    updateSelectInput(session, "plot_click", selected = NULL)
  })
  
  # observe event that sets the  selected state when user clicks on map by x and y coordinates
  observeEvent(input$plot_click, {
    click_loc <- input$plot_click
    state_name <- map.where("state", click_loc$x, click_loc$y)
    state_name <- gsub(":.*", "", state_name) # remove any characters after and including ":"
    selected_state$state <- state_name

    if (!is.null(selected_state$state)) {
      data_info <- data_lookup[[input$map_var]]
      if(input$map_var != "Weighted Sum"){
        data_info$join_data %>%
          filter(State == selected_state$state) %>%
          select(data_info$var) %>%
          pull() %>%
          as.character() -> selected_state$data
      } else {
        weighted_sum_data() %>% 
          filter(State == selected_state$state) %>% 
          select(`total_normalized_weight`) %>% 
          pull() %>%
          as.character() -> selected_state$data
      }
    }
  })
  
  # renderUI context to update the selected state in the sidebar panel
  output$selected_state_header <- renderUI({
    if (!is.null(selected_state$state)) {
      tags$h3("Selected State: ",str_to_title(selected_state$state), br())
    } else {
      tags$h4("Please click on a state.")
    }
  })
  
  # renderPlot context to render the map plot for each select input variable using the look up table
  output$us_map <- renderPlot({
    if (input$map_var == "Weighted Sum") {
      weighted_data <- weighted_sum_data()
      join_data <- map_data("state", exact = FALSE) %>%
        left_join(weighted_data, by = c("region" = "State")) %>%
        mutate(fill_color = total_normalized_weight, region = tolower(region))
      create_map_plot(join_data, "fill_color", "Weighted Sum (Normalized)", TRUE)
    } else {
      data_info <- data_lookup[[input$map_var]]
      map_data <- map_data("state", exact = FALSE) %>%
        left_join(data_info$join_data, by = c("region" = "State"))
      if (input$map_var %in% c("Education Spending", "# Citizens Per Hospital", "Median Listing Prices")) {
        map_data = map_data %>% mutate(fill_color = !!sym(data_info$var) / 1e3, region = tolower(region))
        create_map_plot(map_data, "fill_color", data_info$legend, TRUE)
      } else {
        map_data = map_data %>% mutate(fill_color = !!sym(data_info$var), region = tolower(region))
        create_map_plot(map_data, "fill_color", data_info$legend, TRUE)
      }
    }
  }, bg="transparent")
    
  # renderPlot context to render "Median House Prices Over Time" time series plot
  output$median_price_plot <- renderPlot({
    if (input$map_var == "Median Listing Prices") {
      #  str(realty_data)
        filtered_data <- realty_data %>%
          filter(State == selected_state$state & 
                 as.numeric(substr(month_date_yyyymm, 1, 4)) != 2023) %>% 
          mutate(State = str_to_title(State))
    
        filtered_data$month <- as.Date(paste0(filtered_data$month_date_yyyymm, "01"), format = "%Y%m%d") 
        filtered_data$year <- as.numeric(substr(filtered_data$month_date_yyyymm, 1, 4))
        create_median_price_plot(filtered_data)
    } else {
        NULL
      }
  })
  
  # renderPlotly context to render the state bar plot
  output$state_bar_plot <- renderPlotly({
    if (input$map_var != "Weighted Sum") {
      data_info <- data_lookup[[input$map_var]]
      data <- data_info$join_data %>%
        select(State, !!sym(data_info$var)) %>%
        arrange(desc(!!sym(data_info$var))) %>% # Sort data in descending order
        mutate(State = fct_reorder(State, !!sym(data_info$var), .desc = TRUE)) %>%  # Reorder states in descending order
        mutate(State = str_to_title(State))
      create_bar_plot(data, data_info$var, input$map_var)
    } else {
      data <- weighted_sum_data() %>% 
        select(State, total_normalized_weight) %>%
        arrange(desc(total_normalized_weight)) %>%
        mutate(State = factor(State, levels = unique(State))) %>% 
        mutate(State = str_to_title(State))
      create_bar_plot(data, "total_normalized_weight", input$map_var)
    }
    
  })

  # renderPlot context to render "Median Days on the Market" seasonal plot
  output$median_days_plot <- renderPlot({
    if (input$map_var == "Median Days on the Market") {
      if (!is.null(selected_state$state)) {
        filtered_data <- realty_data %>%
          group_by(State) %>% 
          filter(State == selected_state$state & 
                 as.numeric(substr(month_date_yyyymm, 1, 4)) != 2023) %>% 
          mutate(State = str_to_title(State))
          
        filtered_data$month <- as.Date(paste0(filtered_data$month_date_yyyymm, "01"), format = "%Y%m%d") 
        filtered_data$year <- as.numeric(substr(filtered_data$month_date_yyyymm, 1, 4))
      

        create_median_days_plot(filtered_data)
      } else {
        NULL 
      }
        
    }else{
        NULL
      }
  })
  
    # renderPlot context to render "Median Days on the Market" seasonal plot
  output$median_days_box_plot <- renderPlotly({
    if (input$map_var == "Median Days on the Market") {
      if (!is.null(selected_state$state)) {
        filtered_data <- realty_data %>%
          group_by(State) %>% 
          filter(State == selected_state$state & 
                 as.numeric(substr(month_date_yyyymm, 1, 4)) != 2023) %>% 
          mutate(State = str_to_title(State))
          
        filtered_data$month <- as.Date(paste0(filtered_data$month_date_yyyymm, "01"), format = "%Y%m%d") 
        filtered_data$year <- as.numeric(substr(filtered_data$month_date_yyyymm, 1, 4))
      

        create_median_days_boxplot(filtered_data)
      } else {
        NULL 
      }
        
    }else{
        NULL
      }
  })
  
  # renderPlot context to render "Median Listing Prices" seasonal plot
  output$median_price_box_plot <- renderPlotly({
    if (input$map_var == "Median Listing Prices") {
      if (!is.null(selected_state$state)) {
        filtered_data <- realty_data %>%
          group_by(State) %>% 
          filter(State == selected_state$state & 
                 as.numeric(substr(month_date_yyyymm, 1, 4)) != 2023) %>% 
          mutate(State = str_to_title(State))
        filtered_data$month <- as.Date(paste0(filtered_data$month_date_yyyymm, "01"), format = "%Y%m%d") 
        filtered_data$year <- as.numeric(substr(filtered_data$month_date_yyyymm, 1, 4))
      

        create_median_price_boxplot(filtered_data)
      } else {
        NULL 
      }
        
    }else{
        NULL
      }
  })
  
  # renderPlot context to render "Median Days on the Market" seasonal plot
  output$median_price_scatter_plot <- renderPlotly({
    if (input$map_var == "Median Listing Prices") {
      if (!is.null(selected_state$state)) {
        filtered_data <- realty_data %>%
          group_by(State) %>% 
          filter(State == selected_state$state & 
                 as.numeric(substr(month_date_yyyymm, 1, 4)) != 2023) %>% 
          mutate(State = str_to_title(State))
        filtered_data$month <- as.Date(paste0(filtered_data$month_date_yyyymm, "01"), format = "%Y%m%d") 
        filtered_data$year <- as.numeric(substr(filtered_data$month_date_yyyymm, 1, 4))
      

        create_median_price_scatterplot(filtered_data)
      } else {
        NULL 
      }
        
    }else{
        NULL
      }
  })
    
    
  # renderUI context for the "Select a variable" selectInput
  output$map_var_input <- renderUI({
    selectInput("map_var", "Select a variable:", choices = c("Education Spending", "# Citizens Per Hospital", "Median Listing Prices", "Median Days on the Market", "Weighted Sum"))
  })
  
  # renderText context to display selected state data
  output$selected_state_data <- renderText({
    if (!is.null(selected_state$data)) {
      data_info <- data_lookup[[input$map_var]]
      column_name <- data_info$var
      if (input$map_var == "Weighted Sum") {pretty_value <- prettyNum(round(as.numeric(selected_state$data), 2), big.mark = ",")} 
      else {pretty_value <- prettyNum(round(as.integer(selected_state$data)), big.mark = ",")}
      if (column_name == "PPCS") {
        pretty_column_name <- column_name
      } else {
        pretty_column_name <- gsub("_", " ", column_name)
        pretty_column_name <- str_to_title(pretty_column_name)
      }
      paste(pretty_column_name, ": ", pretty_value, "*")
    }
  })
  
  output$description <- renderText({
    if (input$map_var == "Median Listing Prices") {
      "*The median listing price for a given state in January 2021."
    } else if (input$map_var == "Median Days on the Market") {
      "*The median number of days property listings spend on the market for a given state in January 2021. Time spent on the market is defined as the time between the initial listing of a property and either its closing date or the date it is taken off the market."
    } else if (input$map_var == "Weighted Sum") {
    "*The normalized weighted sum of Education Spending, # Citizens Per Hospital, Median Listing Prices, and Median Days on the Market, according to the provided weights."
    }else if(input$map_var == "Education Spending"){
    "*The Per Pupil amounts for Current Spending by state government for a given state in 2020." 
    }else if(input$map_var == "# Citizens Per Hospital"){
    "*The number of citizens per hospital for a given state in 2022." 
    }
  })
  
  # U.S. County ANALYSIS
  output$us_county_map <- renderLeaflet({state_leaflet_plot(input$bedrooms, input$state_optional_select)})
  
  selected_county <- reactiveVal()

  observeEvent(input$us_county_map_shape_click, {click_data <- input$us_county_map_shape_click
    selected_county(click_data$id)
  })
  
  selected_county_info <- reactive({
    if (!is.null(selected_county())) {
      county_info <- us_counties_2021[us_counties_2021$OBJECTID == selected_county(), c("COUNTY_NAME", "STATE_NAME")]
      return(county_info)
    }
  })

  output$county_info <- renderDT({
    if (is.null(selected_county())) {
      return(NULL)
    }

    county_data <- us_counties_all[us_counties_all$OBJECTID == selected_county(), ] %>% 
      dplyr::select(-OBJECTID, -geometry, -STATE, -COUNTY) %>% 
      arrange(desc(Year)) %>% 
      select(-STATE_NAME, -COUNTY_NAME)
    datatable(county_data, rownames = FALSE)
  })
  
  output$bedrooms_line_plot <- renderPlotly({
    if (!is.null(selected_county())) {
      create_bedrooms_line_plot(selected_county_info(), input$bedrooms)
    }
  })
  
    # renderUI context to update the selected state in the sidebar panel
  output$selected_state_county_header <- renderUI({
    if (!is.null(selected_county())) {
      tags$h3(paste(selected_county_info()[1], selected_county_info()[2], sep = ", "), br())

    } else {
      tags$h4("Showing All U.S. Counties")
    }
  })
  
}

shinyApp(ui = ui, server = server)

