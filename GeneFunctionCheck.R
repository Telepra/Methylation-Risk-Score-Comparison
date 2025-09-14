library(biomaRt)
library(readxl)

# Connect to Ensembl gene database for humans
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

# Read your CCA file
cca_df <- read_excel("C:/Users/denni/OneDrive/Desktop/Clark Lab/Code and Sheets/Gene_CCA.xlsx")
my_genes <- cca_df$UCSC_RefGene_Name

# Fetch gene biotypes
gene_info <- getBM(
  attributes = c("external_gene_name", "gene_biotype"),
  filters = "external_gene_name",
  values = my_genes,
  mart = ensembl
)

# Get protein-coding genes
protein_coding_genes <- gene_info[gene_info$gene_biotype == "protein_coding", "external_gene_name"]

# Filter your original dataframe to keep only protein-coding genes
cca_df_filtered <- cca_df[cca_df$UCSC_RefGene_Name %in% protein_coding_genes, ]

# View the filtered dataframe
print("Filtered dataframe with only protein-coding genes:")
print(cca_df_filtered)

# You can also save the filtered dataframe to a new Excel file if needed
writexl::write_xlsx(cca_df_filtered, "Protein_Coding_Genes_CCA.xlsx")
