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

# Fisher's test function
run_fisher <- function(genes1,genes2, universe) {
  a <- length(intersect(genes1,genes2)) #top left
  b <- length(setdiff(genes1,genes2)) # top right
  c <- length(setdiff(genes2, genes1)) # bottom left
  d <- length(setdiff(universe, union(genes1,genes2))) #bottom right
  
  contingency <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE) #matrix
  fisher.test(contingency) #fisher's test
}

# Permutation test function
run_perm_test <- function(set1, set2, universe, n_perm = 10000) {
  observed_result <- run_fisher(set1, set2, universe) #observed result
  observed_or <- observed_result$estimate # observed Odds Ratio
  
  permuted_ors <- numeric(n_perm) # stores odd ratios from permutation test
  size1 <- length(set1) # just stores length so can grab random set of CpGs
  size2 <- length(set2)
  set.seed(42)  # for reproducibility
  
  for (k in 1:n_perm) {
    perm1 <- sample(universe, size1) # randomly choose a number of samples equal to size1 from the universe
    perm2 <- sample(universe, size2)
    result <- run_fisher(perm1, perm2, universe) #permuted result
    #print(results$estimate)
    #print(result$p.value)
    permuted_ors[k] <- result$estimate # each individual permuted odds ratio
  }
  
  # formula for empirical p-value is that you add up the number of times you got an odds ratio that
  #was greater than the observed odds ratio during the permutation
  # and then you divide by the number of times the permutation occurred
  empirical_p <- (sum(permuted_ors >= observed_or)) / (n_perm)
  list(observed_or = observed_or, empirical_p = empirical_p)
}

# Run analysis
studies <- c("Lohoff","Liu","McCartney", "Bernabeu", "Xiaoyu")

#loop through each combination of studies
for (i in 1:(length(studies)-1)) {
  for (j in (i+1):length(studies)) {
    s1 <- studies[i]
    s2 <- studies[j]
    cat("\n=== Fisher's Test:", s1, "vs", s2, "===\n")
    
    set1 <- cca_df$UCSC_RefGene_Name[cca_df[[s1]] == 1]# set1 is the corresponding gene extracted from the CCA file
    set2 <- cca_df$UCSC_RefGene_Name[cca_df[[s2]] == 1]
    
    # Run Fisher's Exact Test
    fisher_res <- run_fisher(set1, set2, all_protein_coding_genes)
    print(fisher_res)
    
    # Run Permutation Test
    perm_res <- run_perm_test(set1, set2, all_protein_coding_genes)
    cat("Observed odds ratio:", perm_res$observed_or, "\n")
    cat("Empirical p-value (permutation):", perm_res$empirical_p, "\n")
  }
}
