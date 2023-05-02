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
  data = data %>% 
    filter(is.finite(median_listing_price))
  ggplot(data, aes(x = date, y = median_listing_price, group = State, color = median_listing_price)) +
    geom_line(size = 1.2, alpha = 0.7) + 
    scale_color_gradientn(colors = color_scale) +
    scale_y_continuous(breaks = seq(0, max(data$median_listing_price) + 5e4, by = 5e4)) +
    labs(title = "Median Home Prices Over Time by State",
         x = "Year",
         y = "Median Home Price") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = "white"),
          plot.background = element_rect(fill = "#272B30", colour = NA),
          legend.position = "none",
          text = element_text(color = "snow"),
          axis.text = element_text(color = "snow3", size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 17))
}

create_median_days_plot <- function(data){
  ggplot(data, aes(x = month, y = median_days_on_market, color = median_days_on_market)) +
    geom_line(size = 1.5, alpha = 0.7) +
    scale_color_gradientn(colors = color_scale) +
    scale_x_date(breaks = "1 year", date_labels = "%Y") +
    facet_wrap(~State) +
    labs(x = "Year", y = "Median Days on Market") +
    ggtitle("Seasonal Plot of Median Days on the Market")+
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
