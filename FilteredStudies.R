library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

#the gene commonalties df
genedf <- read_excel("C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/GeneCommonalities2.xlsx")

# Filter by study name for gene df
filtered_df_gene <- genedf %>%
  filter(Study %in% c("Lohoff", "McCartney", "Bernabeu"))

# Group everything together again
df_summary_gene <- filtered_df_gene %>%
  group_by(UCSC_RefGene_Name) %>%
  summarise(
    Study = paste(sort(Study), collapse = "_"),
    Count = n(),
  )

write_xlsx(df_summary_gene,
           "C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\Code and Sheets\\FilteredGeneStudies.xlsx")

chr_pos_df <- read_excel("C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/ChrPosCommonalities2.xlsx")

# Filter by study name for chr_pos df
filtered_df_chr_pos <- chr_pos_df %>%
  filter(Study %in% c("Lohoff", "McCartney", "Bernabeu"))

# Group everything together again
df_summary_chr_pos <- filtered_df_chr_pos %>%
  group_by(chr_pos) %>%
  summarise(
    Study = paste(sort(Study), collapse = "_"),
    Count = n(),
  )

write_xlsx(df_summary_chr_pos,
           "C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\Code and Sheets\\FilteredChrPosStudies.xlsx")

