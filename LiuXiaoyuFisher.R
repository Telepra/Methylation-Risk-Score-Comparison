library(readxl)
library(dplyr)
library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
library(minfi)

# Load CCA file
cca_df <- read_excel("C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/Liu&XiaoyuOverlap.xlsx")

# Load or define all 450K array probe IDs
# For example, from annotation:
annotationDF_450K <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)
all_450K_cpgs <- rownames(annotationDF_450K)

run_fisher <- function(study1, study2, df, universe) {
  probes1 <- df$probe_id[df[[study1]] == 1]
  probes2 <- df$probe_id[df[[study2]] == 1]
  
  a <- length(intersect(probes1, probes2))                          # in both
  b <- length(setdiff(probes1, probes2))                            # in study1 only
  c <- length(setdiff(probes2, probes1))                            # in study2 only
  d <- length(setdiff(universe, union(probes1, probes2)))           # in neither
  
  # Build matrix
  contingency <- matrix(c(a, b, c, d), nrow = 2,
                        dimnames = list(study1 = c("In", "Not in"),
                                        study2 = c("In", "Not in")), byrow=TRUE)
  
  # Run Fisher’s Exact Test
  result <- fisher.test(contingency)
  
  list(table = contingency, p.value = result$p.value, odds.ratio = result$estimate)
}
# Assuming these are your study column names
studies <- c("Liu", "Xiaoyu")

# Run pairwise tests
results <- list()
for (i in 1:(length(studies)-1)) {
  for (j in (i+1):length(studies)) {
    s1 <- studies[i]
    s2 <- studies[j]
    cat("\n=== Fisher's Test:", s1, "vs", s2, "===\n")
    res <- run_fisher(s1, s2, cca_df, all_450K_cpgs)
    print(res$table)
    print(paste("p-value:", res$p.value))
    print(paste("odds ratio:", res$odds.ratio))
  }
}
