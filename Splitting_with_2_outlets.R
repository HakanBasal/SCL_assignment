


##############################################################################
## RETAIL STORE SPLITTING BETWEEN TWO OUTLETS
## This implementation allows retail stores to send products to their
## two nearest outlets based on distance criteria
##############################################################################

library(tidyverse)
library(geosphere)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(ggplot2)
library(readxl)

France_Outlet <- read_excel("France_Outlet.xlsx")
France_Retail_ <- read_excel("France_Retail.xlsx")
  
# Load data and prepare base map (assuming these are already done)
stores_raw <- bind_rows(France_Outlet, France_Retail_) 
# Convert to sf objects
stores_sf <- st_as_sf(
  stores_raw,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
)

# Split into retail and outlets
retail_sf <- filter(stores_sf, StoreType == "Retail")
outlets_sf <- filter(stores_sf, StoreType == "Outlet")

# Get France map for plotting
fr_metropole <- ne_countries(
  scale = "medium",
  country = "France",
  returnclass = "sf"
) %>%
  st_crop(xmin = -5.5, xmax = 9.5, ymin = 41, ymax = 51.5)

##############################################################################
## 1 ▸  Find Two Nearest Outlets for Each Retail Store
##############################################################################

# Function to find the two nearest outlets and calculate distances
find_two_nearest_outlets <- function(retail_coords, outlet_coords, outlet_numbers) {
  # Calculate distances from one retail point to all outlets
  distances_km <- distHaversine(retail_coords, outlet_coords) / 1000
  # Find the two nearest
  sorted_indices <- order(distances_km)
  nearest_two <- sorted_indices[1:2]
  
  return(c(
    outlet1 = outlet_numbers[nearest_two[1]],
    dist1 = distances_km[nearest_two[1]],
    outlet2 = outlet_numbers[nearest_two[2]],
    dist2 = distances_km[nearest_two[2]]
  ))
}

# Apply the function to each retail store
nearest_outlets <- t(apply(
  st_coordinates(retail_sf),
  1,
  function(pt) find_two_nearest_outlets(
    pt,
    st_coordinates(outlets_sf),
    outlets_sf$storeNumber
  )
))

# Convert to dataframe and ensure proper types
nearest_df <- as_tibble(nearest_outlets) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(c(outlet1, outlet2), as.integer))

# Combine with retail data
retail_with_nearest <- bind_cols(retail_sf, nearest_df)

##############################################################################
## 2 ▸  Apply Distance Cutoff for Second Outlet
##############################################################################

# Maximum distance threshold for second outlet (in km)
max_distance_km <- 200  # Adjust this threshold as needed

# Apply the cutoff - only use second outlet if within threshold
retail_with_cutoff <- retail_with_nearest %>%
  mutate(
    # If second outlet is too far, set to NA
    use_second = dist2 <= max_distance_km,
    outlet2_final = if_else(use_second, outlet2, NA_integer_),
    dist2_final = if_else(use_second, dist2, NA_real_)
  )

##############################################################################
## 3 ▸  Calculate Shipment Splits
##############################################################################

# Calculate split ratios using inverse distance weighting
retail_final <- retail_with_cutoff %>%
  mutate(
    # Equal split (50/50) if second outlet is used
    split_equal_outlet1 = if_else(use_second, 0.5, 1.0),
    split_equal_outlet2 = if_else(use_second, 0.5, 0.0),
    
    # Inverse distance weighted split
    # Weight is inversely proportional to distance
    inv_dist1 = 1 / dist1,
    inv_dist2 = if_else(use_second, 1 / dist2, 0),
    total_inv_dist = inv_dist1 + inv_dist2,
    
    # Calculate final weighted proportions
    split_weighted_outlet1 = inv_dist1 / total_inv_dist,
    split_weighted_outlet2 = inv_dist2 / total_inv_dist
  )



##############################################################################
## 5 ▸  Visualize the Results
##############################################################################

# Helper functions for nice axis labels
lon_lab <- function(x) paste0(abs(x), "°", ifelse(x < 0, " W", ifelse(x == 0, "", " E")))
lat_lab <- function(x) paste0(abs(x), "° N")

# Create split lines data for visualization
create_split_lines <- function(retail_data) {
  # Create lines only for stores with two outlets
  split_stores <- retail_data %>%
    filter(!is.na(outlet2_final))
  
  # Get coordinates
  retail_coords <- st_coordinates(split_stores)
  
  # Create line data for primary outlet connections
  primary_lines <- map_dfr(1:nrow(split_stores), function(i) {
    store <- split_stores[i,]
    outlet_idx <- which(outlets_sf$storeNumber == store$outlet1)
    if(length(outlet_idx) == 0) return(NULL)
    
    outlet_coords <- st_coordinates(outlets_sf[outlet_idx,])
    
    tibble(
      store_num = store$storeNumber,
      x_start = retail_coords[i,1],
      y_start = retail_coords[i,2],
      x_end = outlet_coords[1,1],
      y_end = outlet_coords[1,2],
      connection = "primary",
      weight = store$split_weighted_outlet1
    )
  })
  
  # Create line data for secondary outlet connections
  secondary_lines <- map_dfr(1:nrow(split_stores), function(i) {
    store <- split_stores[i,]
    outlet_idx <- which(outlets_sf$storeNumber == store$outlet2_final)
    if(length(outlet_idx) == 0) return(NULL)
    
    outlet_coords <- st_coordinates(outlets_sf[outlet_idx,])
    
    tibble(
      store_num = store$storeNumber,
      x_start = retail_coords[i,1],
      y_start = retail_coords[i,2],
      x_end = outlet_coords[1,1],
      y_end = outlet_coords[1,2],
      connection = "secondary",
      weight = store$split_weighted_outlet2
    )
  })
  
  bind_rows(primary_lines, secondary_lines)
}

# Generate connection lines
connection_lines <- create_split_lines(retail_final)

# Create primary outlet connections for stores with only one outlet
single_outlet_stores <- retail_final %>%
  filter(is.na(outlet2_final))

single_outlet_lines <- map_dfr(1:nrow(single_outlet_stores), function(i) {
  store <- single_outlet_stores[i,]
  outlet_idx <- which(outlets_sf$storeNumber == store$outlet1)
  if(length(outlet_idx) == 0) return(NULL)
  
  store_coords <- st_coordinates(store)
  outlet_coords <- st_coordinates(outlets_sf[outlet_idx,])
  
  tibble(
    store_num = store$storeNumber,
    x_start = store_coords[1],
    y_start = store_coords[2],
    x_end = outlet_coords[1,1],
    y_end = outlet_coords[1,2],
    connection = "single",
    weight = 1
  )
})

# Combine all lines
all_lines <- bind_rows(connection_lines, single_outlet_lines)

# Plot with split connections
ggplot() +
  # Base map
  geom_sf(data = fr_metropole, fill = "grey92", colour = NA) +
  
  # Connection lines with varying thickness based on proportion
  geom_segment(
    data = all_lines,
    aes(
      x = x_start, y = y_start,
      xend = x_end, yend = y_end,
      colour = connection,
      size = weight,
      alpha = weight
    ),
    lineend = "round"
  ) +
  
  # Retail stores
  geom_sf(
    data = retail_final,
    shape = 21,
    fill = "#2F6FA7",
    colour = "#2F6FA7",
    size = 1.5,
    alpha = 0.6
  ) +
  
  # Outlet stores
  geom_sf(
    data = outlets_sf,
    shape = 21,
    fill = "#D53838",
    colour = "white",
    size = 4,
    stroke = 0.7
  ) +
  
  # Outlet labels
  geom_sf_text(
    data = outlets_sf,
    aes(label = storeNumber),
    nudge_y = 0.15,
    size = 3.4,
    fontface = "bold",
    colour = "black",
    stroke = 0.2,
    bg.colour = "white",
    bg.r = 0.15
  ) +
  
  # Scales and styling
  scale_colour_manual(
    values = c("primary" = "#0072B2", "secondary" = "#009E73", "single" = "#999999"),
    name = "Connection Type",
    labels = c("primary" = "Primary Outlet", "secondary" = "Secondary Outlet", "single" = "Single Outlet")
  ) +
  scale_size_continuous(range = c(0.2, 1), name = "Proportion") +
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none") +
  
  scale_x_continuous(breaks = seq(-5, 9, by = 2), labels = lon_lab, expand = expansion(0)) +
  scale_y_continuous(breaks = seq(42, 50, by = 2), labels = lat_lab, expand = expansion(0)) +
  
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_minimal,
    text_size = 7
  ) +
  
  coord_sf(xlim = c(-5.5, 9.5), ylim = c(41, 51.5), expand = FALSE) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11)
  ) +
  
  labs(
    title = "Retail Stores with Split Shipments to Multiple Outlets",
    subtitle = paste0(
      "Retail stores can ship to two outlets if the second outlet is within ", 
      max_distance_km, " km.\n",
      "Line thickness represents the proportion of shipments to each outlet."
    )
  )

##############################################################################
## 6 ▸  Summary Statistics
##############################################################################

# Calculate summary statistics
summary_stats <- retail_final %>%
  st_drop_geometry() %>%
  summarise(
    total_retail_stores = n(),
    stores_with_split_shipments = sum(!is.na(outlet2_final)),
    avg_dist_to_primary_km = mean(dist1, na.rm = TRUE),
    avg_dist_to_secondary_km = mean(dist2_final, na.rm = TRUE)
  )

print("Summary Statistics for Split Shipment Model:")
print(summary_stats)


