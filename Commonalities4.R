library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

# 1. Load your annotated Excel file
annotated <- read_excel("C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/AnnotatedResults6_23_25.xlsx")

# 2. Filter for studies of interest
filtered <- annotated %>%
  filter(str_detect(Study, "Lohoff|McCartney|Bernabeu"))

# 3. Process UCSC_RefGene_Name column
processed_genes <- filtered %>%
  mutate(UCSC_RefGene_Name = str_replace_all(UCSC_RefGene_Name, "\\s*;\\s*", ";")) %>% #gets rid of all white space
  mutate(UCSC_RefGene_Name = sapply(str_split(UCSC_RefGene_Name, ";"), 
                                    function(x) paste(unique(x), collapse = ";"))) %>%
  separate_rows(UCSC_RefGene_Name, sep = ";") 

# 4. Group by gene name
by_gene <- processed_genes %>%
  group_by(UCSC_RefGene_Name) %>%
  summarise(
    combined_studies = paste(unique(Study), collapse = "_"),
    study_count = n_distinct(Study),
    .groups = "drop"
  )


# 5. Group by chr_pos
by_chr_pos <- filtered %>%
 group_by(chr_pos) %>%
 summarise(
   combined_studies = paste(unique(Study), collapse = "_"),
   study_count = n_distinct(Study),
   .groups = "drop"
 )

# 6. Write to Excel
write_xlsx(
 list(
   "Grouped_by_Gene" = by_gene,
   "Grouped_by_chr_pos" = by_chr_pos
 ),
 "C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/GScommonalities4.xlsx"
)
