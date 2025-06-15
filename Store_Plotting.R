library(tidyverse)
library(geosphere)
library(ggspatial)
library(terra)           # must load before rnaturalearth on some systems
library(rnaturalearth)
library(sf)
library(readxl)
library(cluster)       # silhouette()
library(clue)          # solve_LSAP()  – Hungarian assignment           # map (optional)
library(ggplot2)
library(dplyr)
library(grid)
library(readxl)

France_Outlet <- read_excel("France_Outlet.xlsx")
France_Retail_ <- read_excel("France_Retail .xlsx")

stores_raw <- bind_rows(France_Outlet, France_Retail_) 

# Plot 1

stores_sf <- st_as_sf(
  stores_raw,
  coords = c("longitude", "latitude"),   # (x = lon, y = lat)
  crs    = 4326,
  remove = FALSE
)
france <- ne_countries(scale = "medium",
                       country = "France",
                       returnclass = "sf")

ggplot() +
  geom_sf(data = france, fill = "grey95", colour = NA) +
  geom_sf(data = filter(stores_sf, StoreType == "Retail"),
          colour = "steelblue", size = 1.7, alpha = .6) +
  geom_sf(data = filter(stores_sf, StoreType == "Outlet"),
          colour = "red", size = 3) +
  annotation_scale(location = "bl", width_hint = .25) +
  annotation_north_arrow(location = "bl") +
  coord_sf(xlim = c(-5.5, 9.5), ylim = c(41, 51.5), expand = FALSE) +
  theme_minimal() +
  labs(title    = "Retail vs. Outlet stores – France",
       subtitle = "Red = Outlet    Blue = Retail")



# ─── 2  the plot -----------------------------------------------------------

# ─── 0  libraries are already loaded in your session ──────────────────────
# (tidyverse, sf, ggspatial, rnaturalearth, …)

# ─── 1  make an sf of France and crop to metropole only  ──────────────────
france <- ne_countries(scale = "medium", country = "France",
                       returnclass = "sf")

france_main <- st_crop(            # bounding box around mainland
  france,
  xmin = -5.5, xmax = 9.5,
  ymin = 41,   ymax = 51.5
)

# ─── 2  helper label functions for nice axes  ─────────────────────────────
lon_lab <- function(x) paste0(abs(x), "°", ifelse(x < 0, " W", " E"))
lat_lab <- function(x) paste0(abs(x), "°", " N")

# ─── 3  plot  ─────────────────────────────────────────────────────────────
ggplot() +
  # France polygon
  geom_sf(data = france_main, fill = "grey92", colour = NA) +
  # retail
  geom_sf(
    data   = filter(stores_sf, StoreType == "Retail"),
    shape  = 21, size = 2,
    fill   = "#2F6FA7", colour = "#2F6FA7", alpha = .7
  ) +
  # outlets
  geom_sf(
    data   = filter(stores_sf, StoreType == "Outlet"),
    shape  = 21, size = 4,
    fill   = "#D53838", colour = "white", stroke = .5
  ) +
  # cartographic helpers
  annotation_scale(location = "br", width_hint = .25, text_cex = .65) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_minimal, text_size = 7
  ) +
  # axes & grid
  scale_x_continuous(
    name   = NULL,
    breaks = c(-5, -2, 0, 2, 4, 6, 8),
    labels = lon_lab,
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    name   = NULL,
    breaks = seq(42, 50, by = 2),
    labels = lat_lab,
    expand = expansion(mult = 0)
  ) +
  coord_sf(xlim = c(-5.5, 9.5), ylim = c(41, 51.5), expand = FALSE) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey85", linewidth = .3),
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle    = element_text(size = 11, hjust = 0, margin = margin(b = 6))
  ) +
  labs(
    title    = "Retail vs. Outlet Stores — France",
    subtitle = "Red = Outlet • Blue = Retail"
  )






##############################################################################
## 1 ▸  Build one sf object per layer (still in WGS‑84)
##############################################################################
stores_raw <- bind_rows(France_Outlet, France_Retail_)

stores_sf <- st_as_sf(
  stores_raw,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
)

# split
retail     <- filter(stores_sf, StoreType == "Retail")
outlets_sf <- filter(stores_sf, StoreType == "Outlet")

##############################################################################
## 2 ▸  k‑means on retail coordinates  (k = 6)
##############################################################################
coords <- st_coordinates(retail)          # matrix: lon  lat

set.seed(42)
km6 <- kmeans(coords, centers = 6, nstart = 50)

retail$cluster_km <- km6$cluster          # add cluster ID 1…6

##############################################################################
## 3 ▸  Map each cluster to the *nearest* outlet
##############################################################################
centres <- as_tibble(km6$centers)         # lon, lat of each cluster centre

# distance matrix: 6 centres × 6 outlet points
dist_mat <- geosphere::distm(
  as.matrix(centres),
  as.matrix(outlets_sf %>% st_coordinates())
)

# one‑to‑one Hungarian assignment = minimal total distance
assignment <- solve_LSAP(dist_mat) |> as.integer()

centres$storeNumber <- outlets_sf$storeNumber[assignment]

centres_out <- centres %>%
  mutate(cluster_km = row_number(),
         assignedOutlet = storeNumber) %>%   # rename here
  select(cluster_km, assignedOutlet)

retail <- retail %>%
  left_join(centres_out, by = "cluster_km")

##############################################################################
## 4 ▸  Quick workload table (optional)
##############################################################################
retail %>% 
  st_drop_geometry() %>%
  count(assignedOutlet, name = "n_retail") %>%
  arrange(desc(n_retail))
# → tells you how many retail stores each outlet will handle

##############################################################################
## 5 ▸  Draw the map
##############################################################################
# Get France map and crop to mainland
france <- ne_countries(scale = "medium", country = "France", returnclass = "sf")

france_main <- st_crop(            # bounding box around mainland
  france,
  xmin = -5.5, xmax = 9.5,
  ymin = 41,   ymax = 51.5
)

# helper functions
lon_lab <- function(x) paste0(abs(x), "°", ifelse(x < 0, " W", ifelse(x == 0, "", " E")))
lat_lab <- function(x) paste0(abs(x), "° N")

ggplot() +
  geom_sf(data = france_main,
          fill   = "grey92", colour = NA) +
  
  geom_sf(data = retail,
          aes(colour = factor(assignedOutlet)),
          size  = 2, alpha = .8) +
  
  geom_sf(data = outlets_sf,
          shape  = 21, fill   = "#E31A1C",
          colour = "black", size = 4, stroke = .7) +
  
  geom_sf_text(data    = outlets_sf,
               aes(label = storeNumber),
               nudge_y   = 0.15,
               size      = 3.4,
               fontface  = "bold",
               colour    = "black",
               stroke    = .2,
               bg.colour = "white",
               bg.r      = .15) +
  
  # clean degree axes
  scale_x_continuous(breaks = seq(-5, 9, by = 2), labels = lon_lab, expand = expansion(0)) +
  scale_y_continuous(breaks = seq(42, 50, by = 2), labels = lat_lab, expand = expansion(0)) +
  
  scale_colour_brewer(palette = "Set2", name = "Outlet\n(storeNumber)") +
  annotation_scale(location = "bl", width_hint = .25, text_cex = .7) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_minimal, text_size = 7) +
  
  coord_sf(xlim = c(-5.5, 9.5), ylim = c(41, 51.5), expand = FALSE) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(colour = "grey85", linewidth = .25),
    panel.grid.minor = element_blank(),
    axis.title       = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(size = 10),
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(size = 11)
  ) +
  
  labs(
    title    = "K means Clustering: Assigning Retail Stores to 6 Outlet Hubs",
    subtitle = paste0(
      "Each retail store is clustered (k=6) and then linked to the nearest outlet.\n",
      "Red circles = outlet locations (labelled)"
    )
  )



# Create a clean assignment table
assignment_table <- retail %>%
  st_drop_geometry() %>%
  select(storeNumber, assignedOutlet) %>%
  arrange(assignedOutlet, storeNumber)

# Print summary stats
summary_stats <- retail %>%
  st_drop_geometry() %>%
  count(assignedOutlet, name = "n_retail") %>%
  arrange(assignedOutlet)

# Print the summary table
print("Summary of Store Assignments:")
print(summary_stats)

# Print detailed assignments by outlet
for(outlet in unique(assignment_table$assignedOutlet)) {
  cat("\n\nOutlet #", outlet, "is assigned these retail stores:\n")
  assigned_stores <- assignment_table %>% 
    filter(assignedOutlet == outlet) %>%
    pull(storeNumber)
  cat(paste(assigned_stores, collapse = ", "))
}

# Optional: Create a formatted table for visualization/export
library(knitr)
library(gt)

# Group by outlet and list all store numbers as comma-separated values
pretty_table <- retail %>%
  st_drop_geometry() %>%
  group_by(assignedOutlet) %>%
  summarize(
    `Number of Retail Stores` = n(),
    `Assigned Retail Store Numbers` = paste(sort(storeNumber), collapse = ", ")
  ) %>%
  arrange(assignedOutlet)

# Print as formatted table
gt(pretty_table) %>%
  tab_header(
    title = "Outlet to Retail Store Assignment",
    subtitle = paste0("Generated on: ", Sys.Date(), " by ", Sys.getenv("USER"))
  ) %>%
  fmt_number(columns = vars(`Number of Retail Stores`), decimals = 0) %>%
  cols_label(
    assignedOutlet = "Outlet Store Number",
    `Number of Retail Stores` = "Number of Assigned Retail Stores",
    `Assigned Retail Store Numbers` = "Retail Store Numbers"
  )


##############################################################################
## 6 ▸  Assign each retail store to its 2 nearest outlets
##############################################################################

# How many neighbours & cutoff (km)
k             <- 2
threshold_km  <- 80     # only keep the 2nd outlet if ≤ 80 km away

# 6a) Extract coordinate matrices
ret_coords <- st_coordinates(retail_sf)            # [n_retail × 2]
out_coords <- st_coordinates(outlets_sf)        # [n_outlets × 2]

# 6b) Distance matrix (km)
dist_mat <- geosphere::distm(ret_coords, out_coords) / 1000

# 6c) Nearest‑neighbour indices & distances (two per retail store)
nn_idx  <- t(apply(dist_mat, 1, function(x) order(x)[1:k]))
nn_dist <- t(apply(dist_mat, 1, function(x) sort(x)[1:k]))

# 6d) Wide table: one row per retail, outlets/distances in columns
retail_nn_wide <- retail %>%
  st_drop_geometry() %>%
  mutate(
    outlet1 = outlets_sf$storeNumber[nn_idx[,1]],
    dist1   = nn_dist[,1],
    outlet2 = outlets_sf$storeNumber[nn_idx[,2]],
    dist2   = nn_dist[,2]
  ) %>%
  # apply cutoff to second neighbour
  mutate(
    outlet2 = if_else(dist2 <= threshold_km, outlet2, NA_integer_),
    dist2   = if_else(dist2 <= threshold_km, dist2,   NA_real_)
  )

# 6e) Long format: one row per (store, outlet) pair
library(tidyr)
retail_nn_long <- retail_nn_wide %>%
  select(storeNumber, outlet1, dist1, outlet2, dist2) %>%
  pivot_longer(
    cols           = c(outlet1, outlet2, dist1, dist2),
    names_to       = c(".value", "rank"),
    names_pattern  = "(outlet|dist)([12])"
  ) %>%
  filter(!is.na(outlet))   # drop missing 2nd neighbour if beyond cutoff

# Inspect the first few assignments
retail_nn_long %>%
  slice_head(n = 10) %>%
  print()

