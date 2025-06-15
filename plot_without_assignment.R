# ─── 0 ─── Packages ─────────────────────────────────────────────────────────
library(tidyverse)
library(sf)
library(readxl)
library(rnaturalearth)
library(ggspatial)
library(purrr)

# ─── 1 ─── Read data ─────────────────────────────────────────────────────────
pallets <- read_excel("pallets_by_route.xlsx")   # cols: Store, Outlet, Pallets
outlets <- read_excel("France_Outlet.xlsx")      # cols: storeNumber, longitude, latitude
retail  <- read_excel("France_Retail.xlsx")      # cols: storeNumber, longitude, latitude

# ─── 2 ─── Debug: Check data structure ─────────────────────────────────────

print(head(pallets))

print(head(retail))

print(head(outlets))

# ─── 3 ─── Make sf point layers ──────────────────────────────────────────────
stores_sf  <- st_as_sf(retail  %>% mutate(storeNumber = as.character(storeNumber)),
                       coords = c("longitude","latitude"), crs = 4326)
outlets_sf <- st_as_sf(outlets %>% mutate(storeNumber = as.character(storeNumber)),
                       coords = c("longitude","latitude"), crs = 4326)

# ─── 4 ─── Create flows with proper LINESTRING geometry ─────────────────────
flows <- pallets %>%
  # join store (origin) point
  left_join(retail %>% select(storeNumber, longitude, latitude) %>%
              mutate(storeNumber = as.character(storeNumber)),
            by = c("Store" = "storeNumber")) %>% 
  rename(from_lon = longitude, from_lat = latitude) %>%
  # join outlet (destination) point
  left_join(outlets %>% select(storeNumber, longitude, latitude) %>%
              mutate(storeNumber = as.character(storeNumber)),
            by = c("Outlet" = "storeNumber")) %>% 
  rename(to_lon = longitude, to_lat = latitude) %>%
  # filter out missing coordinates
  filter(!is.na(from_lon) & !is.na(from_lat) & !is.na(to_lon) & !is.na(to_lat))

# ─── 5 ─── Debug: Check flows data ─────────────────────────────────────────

print(head(flows))
print(paste("Number of flows:", nrow(flows)))

# ─── 6 ─── Create LINESTRING geometries ────────────────────────────────────
flows_sf <- flows %>%
  rowwise() %>%
  mutate(
    geometry = list(st_linestring(matrix(c(from_lon, from_lat, to_lon, to_lat), 
                                         nrow = 2, byrow = TRUE)))
  ) %>%
  ungroup() %>%
  st_as_sf(crs = 4326)

# ─── 7 ─── Base map of mainland France ───────────────────────────────────────
france <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
france_main <- st_crop(france,
                       xmin = -5.5, xmax = 9.5,
                       ymin = 41,   ymax = 51.5)

# ─── 8 ─── Pretty axis labels ───────────────────────────────────────────────
lon_lab <- function(x) paste0(abs(x), "°", ifelse(x < 0, " W", " E"))
lat_lab <- function(x) paste0(abs(x), "° N")

# ─── 9 ─── Plot with Flow Lines ─────────────────────────────────────────────
ggplot() +
  # country fill
  geom_sf(data = france_main, fill = "grey95", colour = NA) +
  
  # flow lines with thickness and color based on pallet volume
  geom_sf(
    data = flows_sf,
    aes(size = Pallets, colour = Pallets),
    alpha = 0.8
  ) +
  
  # store points (blue)
  geom_sf(data = stores_sf,
          shape = 21, fill = "#2F6FA7", colour = "white", size = 3, 
          alpha = 0.9, stroke = 1) +
  
  # outlet points (red)
  geom_sf(data = outlets_sf,
          shape = 21, fill = "#D53838", colour = "white", size = 4, 
          alpha = 0.9, stroke = 1) +
  
  # legends for pallet volume
  scale_size_continuous(range = c(0.5, 3), name = "Pallets") +
  scale_color_viridis_c(name = "Pallets", option = "plasma") +
  
  # cartographic extras
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_minimal) +
  
  # degree axes
  scale_x_continuous(breaks = seq(-5, 9, by = 2),
                     labels = lon_lab, expand = expansion(0.01)) +
  scale_y_continuous(breaks = seq(42, 50, by = 2),
                     labels = lat_lab, expand = expansion(0.01)) +
  coord_sf(xlim = c(-5.5, 9.5), ylim = c(41, 51.5), expand = FALSE) +
  
  # theme & titles
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11)
  ) +
  labs(
    title = "Store → Outlet Pallet Flows — France",
    subtitle = "Line thickness & color proportional to pallet volume | Blue = Stores, Red = Outlets"
  )




# Function to calculate distance between two lat/lon points
haversine_distance <- function(lon1, lat1, lon2, lat2) {
  # Convert to radians
  lon1 <- lon1 * pi / 180
  lat1 <- lat1 * pi / 180
  lon2 <- lon2 * pi / 180
  lat2 <- lat2 * pi / 180
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  # Earth's radius in km
  R <- 6371
  distance <- R * c
  return(distance)
}

# Add distance calculations to flows
flows_with_distance <- flows %>%
  mutate(
    distance_km = haversine_distance(from_lon, from_lat, to_lon, to_lat)
  )

# Create LINESTRING geometries
flows_sf <- flows_with_distance %>%
  rowwise() %>%
  mutate(
    geometry = list(st_linestring(matrix(c(from_lon, from_lat, to_lon, to_lat), 
                                         nrow = 2, byrow = TRUE)))
  ) %>%
  ungroup() %>%
  st_as_sf(crs = 4326)

# Calculate distance statistics
distance_stats <- flows_sf %>%
  st_drop_geometry() %>%
  summarise(
    total_routes = n(),
    total_pallets = sum(Pallets),
    avg_distance_simple = mean(distance_km),
    avg_distance_weighted = sum(distance_km * Pallets) / sum(Pallets),
    min_distance = min(distance_km),
    max_distance = max(distance_km),
    median_distance = median(distance_km),
    total_pallet_km = sum(distance_km * Pallets)
  )


cat("Total number of routes:", distance_stats$total_routes, "\n")
cat("Total pallets shipped:", distance_stats$total_pallets, "\n")
cat("Average distance per route:", round(distance_stats$avg_distance_simple, 1), "km\n")
cat("Shortest route:", round(distance_stats$min_distance, 1), "km\n")
cat("Longest route:", round(distance_stats$max_distance, 1), "km\n")
cat("Median route distance:", round(distance_stats$median_distance, 1), "km\n")
cat("Total pallet-kilometers:", round(distance_stats$total_pallet_km, 0), "\n")

# Distance by store analysis
store_distance_analysis <- flows_sf %>%
  st_drop_geometry() %>%
  group_by(Store) %>%
  summarise(
    routes = n(),
    total_pallets = sum(Pallets),
    avg_distance = round(mean(distance_km), 1),
    avg_distance_weighted = round(sum(distance_km * Pallets) / sum(Pallets), 1),
    min_distance = round(min(distance_km), 1),
    max_distance = round(max(distance_km), 1),
    total_pallet_km = round(sum(distance_km * Pallets), 0),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_distance_weighted))

print("DISTANCE ANALYSIS BY STORE:")
print(store_distance_analysis)

# Base map of mainland France
france <- ne_countries(scale = "medium", country = "France", returnclass = "sf")
france_main <- st_crop(france,
                       xmin = -5.5, xmax = 9.5,
                       ymin = 41,   ymax = 51.5)

# Pretty axis labels
lon_lab <- function(x) paste0(abs(x), "°", ifelse(x < 0, " W", " E"))
lat_lab <- function(x) paste0(abs(x), "° N")

# Enhanced plot with distance information
ggplot() +
  # country fill
  geom_sf(data = france_main, fill = "grey95", colour = NA) +
  
  # flow lines with thickness based on pallets and color based on distance
  geom_sf(
    data = flows_sf,
    aes(size = Pallets, colour = distance_km),
    alpha = 0.8
  ) +
  
  # store points (blue)
  geom_sf(data = stores_sf,
          shape = 21, fill = "#2F6FA7", colour = "white", size = 3, 
          alpha = 0.9, stroke = 1) +
  
  # outlet points (red)
  geom_sf(data = outlets_sf,
          shape = 21, fill = "#D53838", colour = "white", size = 4, 
          alpha = 0.9, stroke = 1) +
  
  # legends
  scale_size_continuous(range = c(0.5, 3), name = "Pallets") +
  scale_color_viridis_c(name = "Distance (km)", option = "plasma") +
  
  # cartographic extras
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_minimal) +
  
  # degree axes
  scale_x_continuous(breaks = seq(-5, 9, by = 2),
                     labels = lon_lab, expand = expansion(0.01)) +
  scale_y_continuous(breaks = seq(42, 50, by = 2),
                     labels = lat_lab, expand = expansion(0.01)) +
  coord_sf(xlim = c(-5.5, 9.5), ylim = c(41, 51.5), expand = FALSE) +
  
  # theme & titles
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11)
  ) +
  labs(
    title = "Store to Outlet Pallet Flows with Distance Analysis",
    subtitle = paste("Line thickness = pallets | Line color = distance | Avg distance:", 
                     round(distance_stats$avg_distance_simple, 1), "km (weighted:", 
                     round(distance_stats$avg_distance_weighted, 1), "km)"),
    caption = paste("Total:", distance_stats$total_pallets, "pallets across", 
                    distance_stats$total_routes, "routes | Total pallet-km:", 
                    round(distance_stats$total_pallet_km, 0))
  )
