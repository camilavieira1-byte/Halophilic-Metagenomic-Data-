library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(viridis)
library(readr)
# Load your spreadsheet
df <- read_csv("filtered_runinfo.csv")
# Extract year
df <- df %>%
  mutate(Year = year(ReleaseDate))

# Summarize
df_summary <- df %>%
  group_by(Year, Model) %>%
  summarise(n_biosamples = n_distinct(BioSample), .groups = "drop")

# Ensure all years and models are present
all_years <- seq(min(df_summary$Year), max(df_summary$Year))
all_models <- unique(df_summary$Model)
df_summary <- df_summary %>%
  complete(Year = all_years, Model = all_models, fill = list(n_biosamples = 0))

# Prepare colors: two fixed colors + viridis for the rest
special_colors <- c(
  "Illumina NovaSeq 6000" = "#DB7093",
  "Illumina MiSeq" = "#C71585",
  "HiSeq X Ten" = "yellow",
  "Ion Torrent Proton" = "grey",
  "MinION" = "pink",
  "Illumina HiSeq 2000" = "green",
  "Ion Torrent PGM" = "darkgreen",
  "Illumina NovaSeq X" = "#FF00FF",
  "Illumina HiSeq 1000"= "#800080",
  "Illumina HiSeq 2500" = "#808000"
)
other_models <- setdiff(all_models, names(special_colors))
colors <- c(special_colors,
            setNames(viridis(length(other_models), option = "turbo"), other_models))

# Plot
ggplot(df_summary, aes(x = Year, y = n_biosamples, fill = Model)) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(
    title = "",
    x = "Year",
    y = "Number of Samples",
    fill = "Sequencing Platform"
  ) +
  scale_fill_manual(values = colors)

install.packages(c("officer", "flextable"))
library(officer)
library(flextable)
# If you want, you can sort by Year and then by number of samples
df_summary_word <- df_summary %>%
  arrange(Year, desc(n_biosamples))
# Create a new Word document
doc <- read_docx()

# Add a title
doc <- doc %>%
  body_add_par("Number of BioSamples per Year by Sequencing Platform", style = "heading 1")

# Add the table
ft <- flextable(df_summary_word)
ft <- autofit(ft)  # adjust column widths automatically

doc <- doc %>%
  body_add_flextable(ft)

# Save Word document
print(doc, target = "BioSamples_per_platform.docx")


