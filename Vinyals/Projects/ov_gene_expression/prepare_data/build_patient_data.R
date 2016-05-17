ROOT_DIRECTORY = '../../../../'

source(
  paste(ROOT_DIRECTORY, 'TCGA.analysis/tools/lib/source/dir.R', sep='')
)


source(
  paste(ROOT_DIRECTORY, 'TCGA.analysis/tools/lib/clinical_data/loader.R', sep='')
)

source.dir(
  paste(ROOT_DIRECTORY, 'TCGA.analysis/tools/lib/followup_data/', sep='')
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

#This part of the script loads and prepares follow up data:
follow_up_data_file = './data/follow_up.txt'
follow_up_data_nte_file = './data/follow_up_nte.txt'

follow_up_data = followup.loader(follow_up_data_file, follow_up_data_nte_file)
follow_up_data = followup.normalizer(follow_up_data)
follow_up_data = followup.add_events(follow_up_data)

write.csv(follow_up_data, './output/follow_up.txt')
