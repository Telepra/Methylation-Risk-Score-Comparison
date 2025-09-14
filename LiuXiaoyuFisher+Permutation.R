library(readxl)
library(dplyr)
library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
library(minfi)

# Load CCA file
cca_df <- read_excel("C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets//Liu&XiaoyuOverlap.xlsx")

# Load or define all 450K array probe IDs
annotationDF_450K <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)
all_450K_cpgs <- rownames(annotationDF_450K)

# Fisher's test function
run_fisher <- function(probes1, probes2, universe) {
  a <- length(intersect(probes1, probes2)) #top left
  b <- length(setdiff(probes1, probes2)) # bottom left
  c <- length(setdiff(probes2, probes1)) # top right
  d <- length(setdiff(universe, union(probes1, probes2))) #bottom right
  
  contingency <- matrix(c(a, b, c, d), nrow = 2) #matrix
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
studies <- c("Liu", "Xiaoyu")

#loop through each combination of studies
for (i in 1:(length(studies)-1)) {
  for (j in (i+1):length(studies)) {
    s1 <- studies[i]
    s2 <- studies[j]
    cat("\n=== Fisher's Test:", s1, "vs", s2, "===\n")
    
    set1 <- cca_df$probe_id[cca_df[[s1]] == 1]# set1 is the corresponding probe_id's extracted from the overlap file
    set2 <- cca_df$probe_id[cca_df[[s2]] == 1]
    
    # Run Fisher's Exact Test
    fisher_res <- run_fisher(set1, set2, all_450K_cpgs)
    print(fisher_res)
    
    # Run Permutation Test
    perm_res <- run_perm_test(set1, set2, all_450K_cpgs)
    cat("Observed odds ratio:", perm_res$observed_or, "\n")
    cat("Empirical p-value (permutation):", perm_res$empirical_p, "\n")
  }
}
