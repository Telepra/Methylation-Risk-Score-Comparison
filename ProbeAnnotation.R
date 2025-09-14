library(readxl)
library(openxlsx)
probesData <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\CpG Site list.xlsx", sheet ='Sheet1')
probes <- probesData$"Unique sites"
head(probes)# probes loaded successfully

# Already Installed
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("IlluminaHumanMethylation450kanno.ilmn12.hg19") #For 450K array

# Already installed
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("IlluminaHumanMethylationEPICanno.ilm10b4.hg19") #for EPIC array

library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
library(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

anno_450k <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)
anno_epic <- getAnnotation(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

probes_450k <- intersect(probes, rownames(anno_450k))
probes_epic <- intersect(probes, rownames(anno_epic))

annotations_450k <- anno_450k[probes_450k, c("chr", "pos", "UCSC_RefGene_Name", "Relation_to_Island")]
annotations_450k$array <- "450K"

annotations_epic <- anno_epic[probes_epic, c("chr", "pos", "UCSC_RefGene_Name", "Relation_to_Island")]
annotations_epic$array <- "EPIC"

probe_annotations <- rbind(annotations_450k, annotations_epic)
probe_annotations$probe_id <- rownames(probe_annotations)

missing_probes <- setdiff(probes, rownames(probe_annotations))
if (length(missing_probes) > 0) {
  warning("Probes not found in 450K or EPIC annotations: ", paste(missing_probes, collapse = ", "))
}





annotation <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)

probeAnnotation <- annotation[probes, c("chr", "pos", "UCSC_RefGene_Name", "Relation_to_Island")]
probeAnnotation$probe_id <- rownames(probeAnnotation)
head(probeAnnotation)
write.csv(probeAnnotation, "C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab", row.names = FALSE)