library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(openxlsx)
library(officer)
library(flextable)

# =========================
# 2. Load your data
# =========================
df <- read_excel("metadados_with_latlon_decimal.xlsx")

# Make sure coordinates are numeric
df$Latitude_decimal <- as.numeric(gsub(",", ".", df$Latitude_decimal))
df$Longitude_decimal <- as.numeric(gsub(",", ".", df$Longitude_decimal))

# =========================
# 3. Convert to sf points
# =========================
df_sf <- st_as_sf(df, coords = c("Longitude_decimal", "Latitude_decimal"), crs = 4326)

# =========================
# 4. Load world countries
# =========================
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# =========================
# 5. Spatial join: assign countries to points
# =========================
df_with_country <- st_join(df_sf, world_sf["name"], left = TRUE)

# =========================
# 6. Assign nearest country to points still missing
# =========================
missing_points <- df_with_country %>% filter(is.na(name))

if(nrow(missing_points) > 0){
  nearest_country <- st_nearest_feature(missing_points, world_sf)
  missing_points$country_assigned <- world_sf$name[nearest_country]
}

# Combine back
df_with_country <- df_with_country %>%
  mutate(country_assigned = ifelse(is.na(name),
                                   missing_points$country_assigned[match(row_number(), which(is.na(name)))],
                                   name))

# =========================
# 7. Count samples per country
# =========================
country_counts <- df_with_country %>%
  st_drop_geometry() %>%
  count(country_assigned, sort = TRUE)

# =========================
# 8. Merge counts with world map
# =========================
world_samples <- left_join(world_sf, country_counts, by = c("name" = "country_assigned"))

# =========================
# 9. Plot choropleth
# =========================
ggplot(world_samples) +
  geom_sf(aes(fill = n), color = "black", linewidth = 0.2) +
  scale_fill_gradient(low = "#FFE4E1", high = "#C71585", na.value = "gray90") +
  labs(title = "", fill = "Samples") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#ADD8E6"),
    plot.background = element_rect(fill = "#ADD8E6")
  )



