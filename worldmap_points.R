# Load libraries
library(readxl)    # to read Excel files
library(ggplot2)   # for plotting
library(maps)      # world map data

# Load your data
df <- read_excel("metadados_with_latlon_decimal.xlsx")

library(readxl)

df <- read_excel("metadados_with_latlon_decimal.xlsx")

# Check column names
colnames(df)
# If your coordinates have comma as decimal, convert to numeric
df$Latitude_decimal <- as.numeric(gsub(",", ".", df$Latitude_decimal))
df$Longitude_decimal <- as.numeric(gsub(",", ".", df$Longitude_decimal))
# Get world map data
world <- map_data("world")
df[is.na(df$Latitude_decimal) | is.na(df$Longitude_decimal), ]

ggplot() +
  geom_map(data = world, map = world,
           aes(map_id = region),
           color = "black", fill = "#D2B48C", linewidth = 0.3) +
  geom_point(data = df, 
             aes(x = Longitude_decimal, y = Latitude_decimal),
             color = "#DB7093", size = 2) +
  coord_fixed(1.3) +
  labs(title = "",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue"),   # light blue background
    plot.background = element_rect(fill = "lightblue"),    # light blue outside panel
    )



