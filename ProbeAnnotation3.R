library(tidyverse)
library(readxl)
library(writexl)

# read annotations to combine with CpG list 2
annotations <- read_csv(
  "C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\probe_annotations.csv",
  col_types = cols(
    .default = col_character(),  # First read everything as character
    pos = col_integer()          # Except position which should be integer
  )
) %>%
  mutate(across(where(is.character), ~na_if(., "")))#convert empty strings to NA

# load studies so which is what we are adding annotated information to
studies <- excel_sheets("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\CpG Site list 2.xlsx") %>% #read all excel sheets
  map(~read_excel(
    "C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\CpG Site list 2.xlsx",
    sheet = .x,
    col_types = "text"  # Just enforce CG# as text, let others be guessed
  )) %>%
  # Ensure consistent column names
  map(~rename_with(.x, ~str_replace_all(., " ", "_")))# replace spaces with _ for consistency

# Type-safe annotation function to avoid errors
annotate_study <- function(df) {
  # First ensure we have the CG# column
  if (!"CG#" %in% names(df)) {
    if ("Probe_ID" %in% names(df)) {
      df <- rename(df, "CG#" = "Probe_ID")
    } else {
      stop("No CG# or Probe_ID column found")
    }
  }
  
  df %>%
    left_join(annotations, by = c("CG#" = "probe_id")) %>% # join if columns match
    mutate(
      UCSC_RefGene_Name = as.character(UCSC_RefGene_Name),
      chr = as.character(chr),
      pos = as.integer(pos)
    ) %>%
    distinct()
}

# take the excel sheets of the the studies and annotate them with the annotate_study function without crashing
annotated_studies <- map(studies, safely(annotate_study))

# Check for errors in annotation
annotation_errors <- map(annotated_studies, "error") %>% compact()
if (length(annotation_errors) > 0) {
  warning("Annotation errors in: ", names(annotation_errors))
  print(annotation_errors)
}

# Extract successfully annotated studies
annotated_studies <- map(annotated_studies, "result") %>% compact()

# counting function for genes, chromosomes, and base pairs
# come back maybe to match chromosomes and base pairs

get_counts <- function(df) {
  # First ensure all columns are atomic vectors
  df <- df %>%
    mutate(across(everything(), ~as.vector(.x)))
  
  gene_counts <- df %>%
    separate_rows(UCSC_RefGene_Name, sep = ";") %>%
    mutate(UCSC_RefGene_Name = str_trim(UCSC_RefGene_Name)) %>%
    dplyr::count(UCSC_RefGene_Name, name = "n", sort = TRUE)
  
  pos_counts <- df %>%
    dplyr::count(pos, name = "n", sort = TRUE)
  
  chr_counts <- df %>%
    dplyr::count(chr, name = "n", sort = TRUE)
  
  list(
    genes = gene_counts,
    positions = pos_counts,
    chromosomes = chr_counts
  )
}

# Apply counting with error handling
count_results <- map(annotated_studies, safely(get_counts))

# Check for counting errors
counting_errors <- map(count_results, "error") %>% compact()
if (length(counting_errors) > 0) {
  warning("Counting errors in: ", names(counting_errors))
  print(counting_errors)
}

# Prepare and save results ----
output_list <- imap(annotated_studies, function(df, study_name) {
  counts <- count_results[[study_name]]$result
  
  list(
    annotated_data = df,
    gene_counts = counts$genes %||% tibble(UCSC_RefGene_Name = character(), n = integer()),#tibble() basically is a df and %||% is just choose whichever is not null
    position_counts = counts$positions %||% tibble(pos = integer(), n = integer()),
    chromosome_counts = counts$chromosomes %||% tibble(chr = character(), n = integer())
  )
})

write_xlsx(
  flatten(output_list),
  "C:/Users/denni/OneDrive/Desktop/Clark Lab/annotated_results_final.xlsx"
)