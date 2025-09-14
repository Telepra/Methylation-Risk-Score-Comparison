library(readxl)
library(openxlsx)
probesData <- read_excel("C:\\Users\\denni\\OneDrive\\Desktop\\Clark Lab\\CpG Site list.xlsx", sheet ='Sheet1')
probes <- probesData$"Unique sites"
head(probes)# probes loaded successfully

library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
library(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

#gets all annotation information for 450k and EPIC array
annotationDF_450k <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)
annotationDF_EPIC <- getAnnotation(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)

#find the probes of interest
probes450k <- intersect(probes, rownames(annotationDF_450k))
probesEPIC <- intersect(probes,rownames(annotationDF_EPIC))

#grabbing annotation information for my probes of interest, drops rownames which is annoying lol
annotations450k <-annotationDF_450k[probes450k, c('chr', 'pos', 'UCSC_RefGene_Name', "Relation_to_Island")]
annotationsEPIC <- annotationDF_EPIC[probesEPIC,c('chr', 'pos', 'UCSC_RefGene_Name', "Relation_to_Island")]

#row names added back and put where the methylation data came from
annotations450k$probe_id <- rownames(annotations450k)
annotationsEPIC$probe_id <- rownames(annotationsEPIC)
annotations450k$array <-"450K"
annotationsEPIC$array <- "EPIC"

#trying to reorder columns so that probe_id comes first
annotations450k <- annotations450k[, c("probe_id", setdiff(names(annotations450k), "probe_id"))]
annotationsEPIC <- annotationsEPIC[, c("probe_id", setdiff(names(annotationsEPIC), "probe_id"))]

#combine probe annotations
combinedProbeAnnotations <- rbind(annotations450k, annotationsEPIC)
head(combinedProbeAnnotations)
write.csv(combinedProbeAnnotations, "C:/Users/denni/OneDrive/Desktop/Clark Lab/probe_annotations.csv", row.names = FALSE)
