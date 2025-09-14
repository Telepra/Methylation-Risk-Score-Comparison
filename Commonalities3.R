library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# 1. Load annotated results file
annotated <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\Code and Sheets\\AnnotatedResults6_20_25.xlsx")

# 2. Filter for probes from Lohoff, McCartney, or Bernabeu in the Study column
# Assuming your study info is in a column called 'Study' (adjust if named differently)
filtered <- annotated %>%
  filter(str_detect(Study, "Lohoff|McCartney|Bernabeu"))

# Group by probe_id, fuse Study names, count how many, and keep chr_pos and UCSC_RefGene_Name
fused <- filtered %>%
  group_by(probe_id, chr_pos, UCSC_RefGene_Name) %>%  # group by extra columns you want to keep
  summarise(
    combined_studies = paste(unique(Study), collapse = "_"),
    study_count = n_distinct(Study),
    .groups = "drop"
  )

write_xlsx(fused,"C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\Code and Sheets\\GScommonalities2.xlsx" )
