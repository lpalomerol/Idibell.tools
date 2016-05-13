gene_expression_file <- './data/expression.txt'
subset_genes_file <- './data/metab_synth_genes.csv'
gene_expression_filtered_file <- './output/expression.txt'

gene_expression <- read.csv(gene_expression_file, sep=' ')
subset_genes <- read.csv(subset_genes_file, sep=';')

subset_gene_expression <- gene_expression[(gene_expression$bcr_patient_barcode %in% subset_genes$GEN),]
write.table(subset_gene_expression, './output/expression.subset.txt', sep=' ', 
            row.names = FALSE, quote=FALSE  )
