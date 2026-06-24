setwd("/home/usuario/Documentos/databasehalophiles/spreadsheets/")
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

# 1. Extract year and GROUP PLATFORMS
df <- df %>%
  mutate(
    Year = year(ReleaseDate),
    Tech_Group = case_when(
      grepl("NovaSeq", Model, ignore.case = TRUE) ~ "Illumina NovaSeq",
      grepl("HiSeq", Model, ignore.case = TRUE) ~ "Illumina HiSeq",
      grepl("MiSeq", Model, ignore.case = TRUE) ~ "Illumina MiSeq",
      grepl("NextSeq", Model, ignore.case = TRUE) ~ "Illumina NextSeq",
      grepl("Illumina", Model, ignore.case = TRUE) ~ "Illumina Other", # Pega o Genome Analyzer IIx
      grepl("Ion Torrent", Model, ignore.case = TRUE) ~ "Ion Torrent",
      grepl("MinION", Model, ignore.case = TRUE) ~ "Oxford Nanopore",
      grepl("DNBSEQ", Model, ignore.case = TRUE) ~ "BGI/MGI",
      grepl("454", Model) ~ "Roche 454", # Adicionado para os modelos antigos
      grepl("3730xL", Model) ~ "Sanger / Capillary", # Adicionado para o AB 3730xL
      TRUE ~ "Other / Unspecified"
    )
  )

# 2. Summarize
df_summary <- df %>%
  group_by(Year, Tech_Group) %>%
  summarise(n_biosamples = n_distinct(BioSample), .groups = "drop")

# 3. Ensure all years and tech groups are present
all_years <- seq(min(df_summary$Year, na.rm = TRUE), max(df_summary$Year, na.rm = TRUE))
all_techs <- unique(df_summary$Tech_Group)
df_summary <- df_summary %>%
  complete(Year = all_years, Tech_Group = all_techs, fill = list(n_biosamples = 0))

# 4. Paleta de cores atualizada (PacBio removido, 454 e Sanger adicionados)
tech_colors <- c(
  "Illumina NovaSeq"   = "#FF00FF",   # Magenta
  "Illumina HiSeq"     = "#C71585",   # MediumVioletRed
  "Illumina MiSeq"     = "#DB7093",   # PaleVioletRed
  "Illumina NextSeq"   = "pink",      # Rosa claro
  "Illumina Other"     = "#800080",   # Roxo escuro
  "Oxford Nanopore"    = "#4B0082",   # Indigo
  "Ion Torrent"        = "grey",      # Cinza
  "BGI/MGI"            = "orange",    # Laranja
  "Roche 454"          = "lightblue", # Azul claro para tech antiga
  "Sanger / Capillary" = "gold",      # Amarelo para Sanger
  "Other / Unspecified"= "lightgrey"
)

# 5. Plotar
ggplot(df_summary, aes(x = Year, y = n_biosamples, fill = Tech_Group)) +
  geom_col(position = "stack") +
  theme_minimal() +
  scale_fill_manual(values = tech_colors) +
  labs(
    title = "",
    x = "Year",
    y = "Number of Samples",
    fill = "Sequencing Platform"
  )