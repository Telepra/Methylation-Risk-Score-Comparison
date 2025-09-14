library(tidyverse)
library(readxl)
library(writexl)

dfGenes <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\GeneCommonalities2.xlsx")
dfGeneCounts <- dfGenes %>%
  group_by(UCSC_RefGene_Name) %>%
  summarise(
    Study = paste(Study, collapse="_"),
    Count = n(),
  )
#print(dfGeneCounts)

dfChrPos <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\ChrPosCommanilities2.xlsx")
dfChrPosCounts <- dfChrPos %>%
  group_by(chr_pos) %>%
  summarise(
    Study = paste(Study, collapse="_"),
    Count = n(),
  )
#print(dfChrPosCounts)


output_list <- list(
  "Gene_Counts" = dfGeneCounts,
  "ChrPos_Counts" = dfChrPosCounts
)

# Write to Excel with multiple sheets
write_xlsx(
  output_list,
  "C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\Commonalities2.xlsx"
)