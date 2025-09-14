library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# Load your file
df <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\Code and Sheets\\AnnotatedResults6_23_25.xlsx")

# Filter for studies of interest
target_studies <- c("Liu", "Xiaoyu")
filtered_df <- df %>% 
  filter(Study %in% target_studies) %>%
  select(probe_id, Study)

# Get unique probe IDs and studies
unique_probes <- unique(filtered_df$probe_id)

# Create a presence matrix: 1 if that probe_id appears in that study, else 0
result_df <- filtered_df %>%
  mutate(value = 1) %>% # adds value column and fills it with 1's
  pivot_wider(
    names_from = Study, #columns from filtered_df's study
    values_from = value, #where study and probe_id match a 1 is added because that's how filtered_df was mutated
    values_fill = 0 #no match = fill with 0's
  )

# View the result
#print(result_df)

write_xlsx(result_df, "C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/Liu&XiaoyuOverlap.xlsx")

