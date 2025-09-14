library(readxl)
library(dplyr)
library(tidyr)

# Step 1: Load and filter data
df <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\Code and Sheets\\AnnotatedResults6_23_25.xlsx")
target_studies <- c("Lohoff", "McCartney", "Bernabeu", "Liu", "Xiaoyu")
filtered_df <- df %>% 
  filter(Study %in% target_studies) %>%
  select(UCSC_RefGene_Name, Study)

# Step 2: Split multi-gene rows into individual genes
split_df <- filtered_df %>%
  # Remove rows with NA or empty genes
  filter(!is.na(UCSC_RefGene_Name), UCSC_RefGene_Name != "") %>%
  # Split genes separated by ";" or ","
  separate_rows(UCSC_RefGene_Name, sep = ";\\s*") %>%  # Handles "; " or ";"
  # Trim whitespace from gene names
  mutate(UCSC_RefGene_Name = trimws(UCSC_RefGene_Name)) %>%
  # Remove empty gene names (if any remain)
  filter(UCSC_RefGene_Name != "")

# Step 3: Create binary presence matrix
result_df <- split_df %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = Study,
    values_from = value,
    values_fn = list(value = max),  # Use max to ensure 1 even if duplicates exist
    values_fill = list(value = 0)
  )

# View result
#print(result_df)

write_xlsx(result_df, "C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/Gene_CCA.xlsx")
