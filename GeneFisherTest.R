library(readxl)
library(dplyr)
library(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)
library(minfi)
library(biomaRt)

# Load CCA file
cca_df <- read_excel("C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/Protein_Coding_Genes_CCA.xlsx")

# ===== Fetch all protein-coding genes from Ensembl =====
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

protein_coding_genes <- getBM(
  attributes = c("external_gene_name", "gene_biotype"),  # optional: include biotype to confirm
  filters = "biotype", 
  values = "protein_coding",
  mart = ensembl
)

# Extract gene names as the universe
all_protein_coding_genes <- protein_coding_genes$external_gene_name

run_fisher <- function(study1, study2, df, universe) {
  genes1 <- df$UCSC_RefGene_Name[df[[study1]] == 1]
  genes2 <- df$UCSC_RefGene_Name[df[[study2]] == 1]
  
  a <- length(intersect(genes1, genes2))                          # in both
  b <- length(setdiff(genes1, genes2))                            # in study1 only
  c <- length(setdiff(genes2, genes1))                            # in study2 only
  d <- length(setdiff(universe, union(genes1, genes2)))           # in neither
  
  # Build matrix
  contingency <- matrix(c(a, b, c, d), nrow = 2,
                        dimnames = list(study1 = c("In", "Not in"),
                                        study2 = c("In", "Not in")),byrow = TRUE)
  
  # Run Fisher’s Exact Test
  result <- fisher.test(contingency)
  
  list(table = contingency, p.value = result$p.value, odds.ratio = result$estimate)
}

# Assuming these are your study column names
studies <- c("Lohoff","Liu","McCartney", "Bernabeu", "Xiaoyu")

# Run pairwise tests
results <- list()
for (i in 1:(length(studies)-1)) {
  for (j in (i+1):length(studies)) {
    s1 <- studies[i]
    s2 <- studies[j]
    cat("\n=== Fisher's Test:", s1, "vs", s2, "===\n")
    res <- run_fisher(s1, s2, cca_df, all_protein_coding_genes)
    print(res$table)
    print(paste("p-value:", res$p.value))
    print(paste("odds ratio:", res$odds.ratio))
  }
}
