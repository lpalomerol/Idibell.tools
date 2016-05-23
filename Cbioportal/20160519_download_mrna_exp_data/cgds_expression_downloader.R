install.packages('cgdsr')
library(cgdsr)
# Create CGDS object
mycgds = CGDS("http://www.cbioportal.org/public-portal/")

test(mycgds)

ID_FIELD=1


DATA_PATH = '../../Very.Common.Data'
GENE_SET_PATH = paste(DATA_PATH, '/Gene.Sets/all_genes.oql', sep = '')


cgds_tools.filter_studies <- function(studies, search_str){
  studies[ grep(search_str, studies$name, ignore.case=TRUE),]
}

cgds_tools.filter_cases  <- function(cases, search_str){
  cases[ grep(search_str, cases$case_list_name, ignore.case=TRUE),]
}

cgds_tools.find_mrna_profile <- function(profiles){
  profiles[grep('mrna$', profiles$genetic_profile_id, ignore.case = FALSE),]
}


cgds_tools.save_genomic_subset <- function(subset, name, subset_id ){
  tmp_file_id = paste(name, subset_id, sep='.subset.')
  write.table(subset, 
              paste('./output/',tmp_file_id, sep=''), 
              sep=' ', quote = FALSE)
}

cgds_tools.save_profile_data_subset <- function(profile_data, name, subset_id ){
  tmp_file_id = paste(name, subset_id, sep='.')
  write.table(profile_data, 
              paste('./output/',tmp_file_id, sep=''), 
              sep=' ', quote = FALSE)
}

cgds_tools.save_profile_data_non_retrieved <- function(non_retrieved, name, subset_id ){
  tmp_file_id = paste(name, subset_id, sep='.nonretrieved.')
  write.table(non_retrieved, 
              paste('./output/',tmp_file_id, sep=''), 
              sep=' ', quote = FALSE)
}

cgds_tools.save_clinical_data <- function(clinical_data, name){
  file_id = paste('clincal', name, 'txt', sep='.')
  write.table(clinical_data,
              paste('./output/',file_id, sep=''), 
              sep='\t', quote = FALSE)
}

cgds_tools.clean_subset <- function(subset){
  subset = subset[!is.na(subset)]
  subset[!grepl('#', subset)]
}

cgds_tools.get_profile_data_batches <- function(mycgds,gene_set, profile_id, case_id, batch_size=150){
  MAX_SEQ = length(gene_set)
  loop = 0
  all_profiles = NULL
  while(batch_size*loop < MAX_SEQ){
    print(paste (case_id, 'Loop', loop))
    first_id = batch_size*loop
    last_id = (batch_size*(loop+1))-1
    subset = gene_set[first_id:last_id]
    subset= cgds_tools.clean_subset(subset)
    
    profile_data = getProfileData(mycgds,subset,profile_id,case_id)
    non_retrieved_genes = setdiff(subset, gsub('\\.', '-', colnames(profile_data)))
    print(paste(case_id, 'Non-retrieved:', paste(non_retrieved_genes, collapse=',')))
    cgds_tools.save_genomic_subset(subset, profile_id, loop)
    cgds_tools.save_profile_data_non_retrieved(non_retrieved_genes, profile_id, loop)
    cgds_tools.save_profile_data_subset(profile_data, profile_id, loop)      
    loop = loop+1
  }
  all_profiles
}  


gene_set_file= file(GENE_SET_PATH)
lines = readLines(gene_set_file, n=1)
gene_set = strsplit(readLines(gene_set_file, n=1), ' ')[[1]]
close(gene_set_file)
length(gene_set)

# Get list of cancer studies at server
all_studies = getCancerStudies(mycgds)
prostate_studies = cgds_tools.filter_studies(all_studies, 'prostate')

BATCH_SIZE= 100

output_file = file('./script.output.Rout', open='wt')
sink(output_file)
sink(output_file, type='message')
for(i in 1:nrow(prostate_studies)){
  study_id = prostate_studies[i, ID_FIELD]
  print('RETRIEVING NEW STUDY')
  print(prostate_studies[i,]$name)
  case_lists = getCaseLists(mycgds, study_id)
  all_tumors_case_id = cgds_tools.filter_cases(case_lists , 'All Tumors')$case_list_id

  all_profiles = getGeneticProfiles(mycgds,study_id)[,1:2]

  print(all_profiles)
  mrna_profile_id = cgds_tools.find_mrna_profile(all_profiles)$genetic_profile_id

  if(length(mrna_profile_id) > 0){
    profile_data = cgds_tools.get_profile_data_batches(mycgds,gene_set,mrna_profile_id,all_tumors_case_id, BATCH_SIZE)
  }
  clinical_data = getClinicalData(mycgds, all_tumors_case_id)
  cgds_tools.save_clinical_data(clinical_data, study_id)
}
sink(type='message')
sink()
close(output_file)
