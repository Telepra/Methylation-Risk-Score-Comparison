library(readxl)
library(openxlsx)
library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
library(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

# Load data
probesDataLohoff <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\CpG Site list 2.xlsx", sheet = 'Lohoff Sites')
LohoffProbes <- probesDataLohoff
head(LohoffProbes)

# Get annotations
anno_450k <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)

# Subset anno_450k using LohoffProbes$`CG#` (assuming these are rownames in anno_450k)
Lohoff_annotations_450k <- anno_450k[LohoffProbes$`CG#`, c("chr", "pos", "UCSC_RefGene_Name", "Relation_to_Island")]

# Add array type
Lohoff_annotations_450k$array <- "450K"

# Merge with 'Weight' from LohoffProbes
Lohoff_annotations_450k <- merge(
  x = Lohoff_annotations_450k,
  y = LohoffProbes[, c("CG#", "Weight")],  # Keep 'CG#' and 'Weight'
  by.x = "row.names",                      # Match rownames (probe IDs) in anno_450k
  by.y = "CG#",                            # Match 'CG#' column in LohoffProbes
  all.x = TRUE                             # Keep all probes from anno_450k
)

# Reset rownames
rownames(Lohoff_annotations_450k) <- Lohoff_annotations_450k$Row.names
Lohoff_annotations_450k$Row.names <- NULL

# Check output
head(Lohoff_annotations_450k)

# Load data
probesDataMcCartney <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\CpG Site list 2.xlsx", sheet = 'McCartney Sites')
McCartneyProbes <- probesDataMcCartney
head(McCartneyProbes)

# Get annotations
anno_epic <- getAnnotation(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

# Subset anno_epic using McCartneyProbes$`CG#` (assuming these are rownames in EPIC)
McCartney_annotations_epic <- anno_epic[McCartneyProbes$`CG#`, c("chr", "pos", "UCSC_RefGene_Name", "Relation_to_Island")]

# Initialize a dataframe with ALL probes from McCartneyProbes
McCartney_annotations_epic <- data.frame(
  row.names = McCartneyProbes$`CG#`,  # Force all probes as rownames
  chr = NA,                           # Placeholder columns
  pos = NA,
  UCSC_RefGene_Name = NA,
  Relation_to_Island = NA,
  stringsAsFactors = FALSE
)

# Subset anno_epic for probes that DO exist in EPIC
epic_probes <- intersect(McCartneyProbes$`CG#`, rownames(anno_epic))
McCartney_annotations_epic[epic_probes, ] <- anno_epic[epic_probes, c("chr", "pos", "UCSC_RefGene_Name", "Relation_to_Island")]

# Add array type
McCartney_annotations_epic$array <- "EPIC"

# Merge with 'Weight' (for all probes, including non-EPIC)
McCartney_annotations_epic <- merge(
  x = McCartney_annotations_epic,
  y = McCartneyProbes[, c("CG#", "Weight")],
  by.x = "row.names",
  by.y = "CG#",
  all.x = TRUE
)

# Reset rownames
rownames(McCartney_annotations_epic) <- McCartney_annotations_epic$Row.names
McCartney_annotations_epic$Row.names <- NULL

# Check output (NA rows = non-EPIC probes)
head(McCartney_annotations_epic)