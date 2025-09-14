library(readxl)
library(dplyr)
library(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)
library(minfi)

# Load CCA file
cca_df <- read_excel("C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/CCA_file.xlsx")

# Load all EPIC array probe IDs
annotationDF_EPIC <- getAnnotation(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)
all_epic_cpgs <- rownames(annotationDF_EPIC)

# Fisher's test function
run_fisher <- function(probes1, probes2, universe) {
  a <- length(intersect(probes1, probes2))
  b <- length(setdiff(probes1, probes2))
  c <- length(setdiff(probes2, probes1))
  d <- length(setdiff(universe, union(probes1, probes2)))
  
  contingency <- matrix(c(a, b, c, d), nrow = 2)
  fisher.test(contingency)
}

# Permutation test function with histogram output
run_perm_test <- function(set1, set2, universe, n_perm = 1000, plot_title = "Permutation Test") {
  observed_result <- run_fisher(set1, set2, universe)
  observed_or <- observed_result$estimate
  
  permuted_ors <- numeric(n_perm)
  size1 <- length(set1)
  size2 <- length(set2)
  set.seed(42)
  
  for (k in 1:n_perm) {
    perm1 <- sample(universe, size1)
    perm2 <- sample(universe, size2)
    result <- run_fisher(perm1, perm2, universe)
    permuted_ors[k] <- result$estimate
  }
  
  empirical_p <- mean(permuted_ors >= observed_or)
  
  # Plot histogram of permuted odds ratios
  hist(permuted_ors, breaks = 50, main = plot_title,
       xlab = "Permuted Odds Ratios", col = "lightgray", border = "white")
  abline(v = observed_or, col = "red", lwd = 2)
  legend("topright", legend = paste("Observed OR =", round(observed_or, 2)), col = "red", lwd = 2)
  
  list(
    observed_or = observed_or,
    empirical_p = empirical_p,
    permuted_ors = permuted_ors
  )
}

# Study names to compare
studies <- c("Lohoff", "McCartney", "Bernabeu")

# Loop through study pairs
for (i in 1:(length(studies)-1)) {
  for (j in (i+1):length(studies)) {
    s1 <- studies[i]
    s2 <- studies[j]
    cat("\n=== Fisher's Test:", s1, "vs", s2, "===\n")
    
    set1 <- cca_df$probe_id[cca_df[[s1]] == 1]
    set2 <- cca_df$probe_id[cca_df[[s2]] == 1]
    
    # Fisher's test
    fisher_res <- run_fisher(set1, set2, all_epic_cpgs)
    print(fisher_res)
    
    # Permutation test + plot
    perm_res <- run_perm_test(set1, set2, all_epic_cpgs, plot_title = paste(s1, "vs", s2))
    cat("Observed odds ratio:", perm_res$observed_or, "\n")
    cat("Empirical p-value (permutation):", perm_res$empirical_p, "\n")
    
    # Optional: save plot to file
    # png(paste0("perm_test_", s1, "_vs_", s2, ".png"))
    # run_perm_test(set1, set2, all_epic_cpgs, plot_title = paste(s1, "vs", s2))
    # dev.off()
  }
}
