library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Keep only unique BioSamples per year
df_runinfo_unique <- df_runinfo %>%
  mutate(year = year(as.Date(ReleaseDate))) %>%
  select(BioSample, year) %>%
  distinct()  # ensures one BioSample per year

# 2. Keep only unique BioSamples with assigned country
df_country_unique <- df_with_country %>%
  st_drop_geometry() %>%
  select(BioSample, country_assigned) %>%
  distinct()

# 3. Merge by BioSample
df_merged <- df_country_unique %>%
  inner_join(df_runinfo_unique, by = "BioSample")

# 4. Count samples per year and country
samples_year_country <- df_merged %>%
  group_by(year, country_assigned) %>%
  summarise(samples = n(), .groups = "drop")

# 5. Ensure all years appear on x-axis
all_years <- seq(min(samples_year_country$year), max(samples_year_country$year))
samples_year_country <- samples_year_country %>%
  complete(year = all_years, country_assigned, fill = list(samples = 0))

# Install if needed
install.packages("randomcoloR")
library(randomcoloR)

# Get unique countries
countries <- unique(samples_year_country$country_assigned)

# Generate distinct colors
set.seed(123)
other_colors <- distinctColorPalette(length(countries) - 1)

# Assign China a fixed color
country_colors <- setNames(c(other_colors), countries[countries != "China"])
country_colors <- c("China" = "#C71585", country_colors)

# Plot
ggplot(samples_year_country, aes(x = factor(year), y = samples, fill = country_assigned)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = country_colors, na.value = "gray90") +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Year", y = "Number of Samples", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(dplyr)
library(flextable)
library(officer)

# 1. Count unique BioSamples per year and country
samples_table <- df_merged %>%
  st_drop_geometry() %>%
  group_by(year, country_assigned) %>%
  summarise(samples = n_distinct(BioSample), .groups = "drop") %>%
  arrange(year, desc(samples))

# 2. Create a Word document
doc <- read_docx()

# Add title
doc <- body_add_par(doc, "Samples per Year and Country", style = "heading 1")

# Convert to flextable
ft <- flextable(samples_table)
ft <- autofit(ft)

# Add table to document
doc <- body_add_flextable(doc, value = ft)

# Save Word document
print(doc, target = "samples_per_year_country.docx")


