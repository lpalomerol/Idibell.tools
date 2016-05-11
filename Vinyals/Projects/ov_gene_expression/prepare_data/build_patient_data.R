ROOT_DIRECTORY = '../../../../'

source(
  paste(ROOT_DIRECTORY, 'TCGA.analysis/tools/lib/clinical_data/loader.R', sep='')
)
source(
  paste(ROOT_DIRECTORY, 'TCGA.analysis/tools/lib/tumor_data/loader.R', sep='')
)
source(
  paste(ROOT_DIRECTORY, 'TCGA.analysis/tools/lib/somatic_mutations/loader.R', sep='')
)



setwd(
  paste(ROOT_DIRECTORY, 'Vinyals/Projects/ov_gene_expression/prepare_data', sep='')
)

#The objective of this script is merge CLINICAL, TUMOR and SOMATIC_MUTATIONS
#    data into a common file.
clinical_data_file = './data/clinical.txt'
clinical_data <- clinical_data.loader(clinical_data_file)

tumor_data_file =  './data/tumor.txt'
tumor_data <- tumor_data.loader(tumor_data_file)

somatic_mutations_file =  './data/somatic.maf'
somatic_mutations = somatic_mutations.loader(somatic_mutations_file)

clinical_data_expanded = merge(clinical_data, tumor_data, by="bcr_patient_barcode")
