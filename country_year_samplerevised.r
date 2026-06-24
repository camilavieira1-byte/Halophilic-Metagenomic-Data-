setwd("/home/usuario/Documentos/databasehalophiles/spreadsheets/")
library(dplyr)
library(ggplot2)
library(readr)

library(dplyr)
library(ggplot2)
library(readr)

# 1. Dados brutos do documento
dados_texto <- 'year,country_assigned,samples
"2,013",Antarctica,2
"2,013",Spain,2
"2,013",United States of America,1
"2,014",India,6
"2,014",Australia,1
"2,015",Italy,4
"2,015",Portugal,3
"2,015",Japan,2
"2,015",New Caledonia,2
"2,015",Canada,1
"2,015",Costa Rica,1
"2,016",Israel,6
"2,016",Italy,4
"2,016",Canada,2
"2,016",Jordan,2
"2,016",Palestine,2
"2,016",Sweden,2
"2,016",United Kingdom,2
"2,016",Chile,1
"2,017",United States of America,75
"2,017",Cayman Is.,25
"2,017",Australia,5
"2,017",Iran,3
"2,017",United Kingdom,3
"2,017",Spain,2
"2,017",Argentina,1
"2,018",Chile,64
"2,018",Japan,21
"2,018",China,8
"2,018",United Kingdom,1
"2,019",China,37
"2,019",United States of America,25
"2,019",Sweden,12
"2,019",Spain,11
"2,019",Russia,7
"2,019",Puerto Rico,3
"2,019",Germany,2
"2,019",Iceland,2
"2,019",Chile,1
"2,019",Portugal,1
"2,019",United Kingdom,1
"2,020",United States of America,28
"2,020",China,12
"2,020",South Africa,4
"2,020",Austria,1
"2,021",China,51
"2,021",United States of America,28
"2,021",Spain,12
"2,021",Norway,8
"2,021",Cayman Is.,5
"2,021",Canada,2
"2,021",Portugal,1
"2,022",Sweden,36
"2,022",United States of America,25
"2,022",Ethiopia,22
"2,022",Australia,15
"2,022",Chile,14
"2,022",Greece,12
"2,022",Mexico,12
"2,022",Saudi Arabia,11
"2,022",United Arab Emirates,7
"2,022",Brazil,6
"2,022",China,6
"2,022",Canada,3
"2,022",Denmark,1
"2,022",Iran,1
"2,023",China,183
"2,023",Brazil,98
"2,023",United States of America,83
"2,023",Iran,10
"2,023",Namibia,10
"2,023",Spain,9
"2,023",Chile,3
"2,023",Poland,3
"2,023",Australia,2
"2,023",Botswana,2
"2,024",China,54
"2,024",Chile,18
"2,024",Antarctica,12
"2,024",Australia,10
"2,024",Hong Kong,10
"2,024",Ethiopia,7
"2,024",France,7
"2,024",United States of America,7
"2,024",Egypt,4
"2,024",India,2
"2,024",Macao,2
"2,025",France,50
"2,025",China,9
"2,025",Chile,1
"2,025",United States of America,1'

samples_table <- read_csv(dados_texto)

# 2. Limpeza e conversão numérica dos dados (conforme o documento)
samples_table <- samples_table %>%
  mutate(
    year = as.numeric(gsub(",", "", as.character(year))),
    samples = as.numeric(samples)
  ) [cite: 2]

# 3. Vetor de países europeus para o agrupamento regional
paises_europeus <- c(
  "Spain", "Italy", "Portugal", "Sweden", "United Kingdom", 
  "Russia", "Germany", "Iceland", "Austria", "Norway", 
  "Greece", "Denmark", "Poland", "France"
) [cite: 2]

# Criar coluna mapeando a Europa
samples_table <- samples_table %>%
  mutate(country_mapped = if_else(country_assigned %in% paises_europeus, "Europe", country_assigned)) [cite: 2]

# 4. Identificar os top 6 países NÃO-europeus com mais amostras totais
top_6_nao_europeus <- samples_table %>%
  filter(country_mapped != "Europe") %>%
  group_by(country_mapped) %>%
  summarise(total_amostras = sum(samples)) %>%
  arrange(desc(total_amostras)) %>%
  slice_head(n = 6) %>%
  pull(country_mapped)

# 5. Agrupar os restantes em "Other"
samples_plot_data <- samples_table %>%
  mutate(
    country_grouped = case_when(
      country_mapped == "Europe" ~ "Europe",
      country_mapped %in% top_6_nao_europeus ~ country_mapped,
      TRUE ~ "Other"
    )
  ) %>%
  group_by(year, country_grouped) %>%
  summarise(samples = sum(samples), .groups = "drop")

# 6. Ordenar os níveis dos fatores para organizar o empilhamento do gráfico
samples_plot_data$country_grouped <- factor(
  samples_plot_data$country_grouped,
  levels = c(top_6_nao_europeus, "Europe", "Other")
)

# 7. PALETA DE CORES IDÊNTICA À FIGURA 4 (Plataformas de Sequenciamento)
# Mapeamento direto dos mesmos códigos HEX e nomes de cores utilizados anteriormente
cores_plataformas <- c(
  "China"                    = "#FF00FF",   # Magenta (Idêntico ao NovaSeq)
  "United States of America" = "#C71585",   # MediumVioletRed (Idêntico ao HiSeq)
  "Brazil"                   = "#DB7093",   # PaleVioletRed (Idêntico ao MiSeq)
  "Chile"                    = "pink",      # Rosa claro (Idêntico ao NextSeq)
  "Europe"                   = "#800080",   # Roxo escuro (Idêntico ao Illumina Other)
  "Australia"                = "#4B0082",   # Indigo (Idêntico ao Oxford Nanopore)
  "Cayman Is."               = "orange",    # Laranja (Idêntico ao BGI/MGI)
  "Other"                    = "lightgrey"  # Cinza claro (Idêntico ao Other/Unspecified)
)

# 8. Gerar o gráfico da Figura 3 atualizado
ggplot(samples_plot_data, aes(x = factor(year), y = samples, fill = country_grouped)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = cores_plataformas) + 
  labs(
    x = "Year",
    y = "Number of Samples",
    fill = "Country / Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# 9. Salvar em alta resolução (300 DPI) para a submissão
ggsave("Figure3_Countries_PlataformPalette.png", width = 8, height = 6, dpi = 300)


