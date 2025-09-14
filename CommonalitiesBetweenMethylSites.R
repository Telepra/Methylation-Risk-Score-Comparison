
library(dplyr)
library(tidyr)

# Read data
probeAnnotations <- read.csv("C:/Users/denni/OneDrive/Desktop/Clark Lab/probe_annotations.csv")

# Split gene names into rows
gene_counts <- probeAnnotations %>%
  mutate(UCSC_RefGene_Name = sapply(  
    #mutate() plays with probe annotations and creates new columns with existing columns
    #sapply() applies transformation to all rows 
    strsplit(UCSC_RefGene_Name, ";"),
    #strsplit(splits a string on a certain character)
    function(x) paste(unique(x), collapse = ";")
    # function(x) is a small anonymous function where x is the input; in this case it is the gene
    # names in the cell
    # paste() takes the unique genes and combines them on a ; so A;B;A;C -> A;B;C
  )) %>%
  separate_rows(UCSC_RefGene_Name, sep = ";") %>% #gives multiple unique genes in a cell its own row
  filter(UCSC_RefGene_Name != "") %>% #removes rows without a gene
  count(UCSC_RefGene_Name, sort = TRUE) #count genes which are now rows

# View genes
head(gene_counts)

chr_counts <- probeAnnotations %>%
  count(chr,sort = TRUE)
head(chr_counts)

BP_counts <- probeAnnotations %>%
  count(pos, sort = TRUE)
head(BP_counts)

Island_status_counts <- probeAnnotations %>%
  count(Relation_to_Island, sort = TRUE)
head(Island_status_counts)

library(writexl)

listOfCommonalities <- list(gene_counts, chr_counts, Island_status_counts, BP_counts)
write_xlsx(listOfCommonalities, path = "C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\listOfCommonalities.xlsx")
