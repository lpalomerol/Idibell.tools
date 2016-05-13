ROOT_DIRECTORY = '../../../..'
DATA_FOLDER_DIRECTORY = '../prepare_data/output'

source(
  paste(ROOT_DIRECTORY, '/TCGA.analysis/tools/lib/clinical_data/loader.R', sep='')
)

source(
  paste(ROOT_DIRECTORY, '/TCGA.analysis/tools/survival_analysis_tools.R', sep='')
)



clinical_data.filter <- function(clinical_data){
  keep <- c('bcr_patient_barcode', 'tumor_grade', 'vascular_invasion_indicator', 
            'lymphovascular_invasion_indicator', 'clinical_stage'  )
  clinical_data[, colnames(clinical_data) %in% keep]
}


clinical_data_file = paste(DATA_FOLDER_DIRECTORY, '/clinical.txt', sep='')
clinical_data <- clinical_data.filter(
  clinical_data.loader(clinical_data_file)
)

expression_data_file <-   paste(DATA_FOLDER_DIRECTORY, '/expression.subset.txt', sep='')
expression_data <- load_gene_expression_data(expression_data_file,sep=' ')
rownames(expression_data) <- expression_data$bcr_patient_barcode

barcodes = expression_data$bcr_patient_barcode
expression_data = lapply(expression_data, function(row){as.numeric(as.character(row))})
expression_data$bcr_patient_barcode = barcodes 

all_data= merge(clinical_data, expression_data, by="bcr_patient_barcode")

all_data$BRCA2_tertile_group = cut(
  all_data$BRCA2,
  breaks = quantile(all_data$BRCA2, probs=seq(0,1, by=0.33), na.rm =TRUE), 
  labels=c('LOW','MED', 'HIGH'),
  include.lowest= TRUE
)

compare_groups <- function(groupA, groupB){
  t.test(groupA, groupB)
}

create_group_tertiles <- function(data, disriminating){
  groups = 
    cut(
      discriminating,
      breaks = quantile(all_data$BRCA1, probs=seq(0,1, by=0.33), na.rm =TRUE), 
      labels=c('LOW','MED', 'HIGH'),
      include.lowest= TRUE
    )
}

join_subgroups <- function(data, subgroups){
  merged_vector = c()
  GROUPS  = length(subgroups)
  for(i in 1:GROUPS){
    group_name = subgroups[i]
    merged_vector = as.vector(rbind (merged_vector, data[[group_name]]) )
  }
  merged_vector
}

create_groups <- function(data, discriminating,  groupA_labels, groupB_labels){
  splitted = split(data, discriminating)
  return(
    list(
      groupA = join_subgroups(splitted, groupA_labels),
      groupB = join_subgroups(splitted, groupB_labels) 
    )
  )
}

retrieve_p_values <- function(all_data){
  output <- data.frame(
    vascular_invasion_t = numeric(0),
    vascular_invasion_p = numeric(0),
    lypmhovascular_invasion_t = numeric(0),
    lypmhovascular_invasion_p = numeric(0),
        # tumor_grade = numeric(0),
    clinical_stage_t = numeric(0),
    clinical_stage_p = numeric(0),
    brca_tertile_t = numeric(0),
    brca_tertile_p = numeric(0)
  )
  
  expression_matrix = all_data[,
    !(colnames(all_data) %in% c('bcr_patient_barcode', 'tumor_grade', 
                      'vascular_invasion_indicator', 'clinical_stage',
                      'lymphovascular_invasion_indicator', 'BRCA2_tertile_group'))
  ]

  cols = colnames(expression_matrix)
  GENES_RANGE =1:nrow(expression_matrix)
  for(i in GENES_RANGE){
    name = cols[i]
    groups_vascular = create_groups(expression_matrix[,i],
                                    all_data[,'vascular_invasion_indicator'], 
                                    c('YES'), 
                                    c('NO'))


        
    groups_lymphovascular = create_groups(expression_matrix[,i],
                                          all_data[,'lymphovascular_invasion_indicator'], 
                                          c('YES'), 
                                          c('NO') )    
    groups_clinical_stage  = create_groups(expression_matrix[,i],
                                           all_data[,'clinical_stage'], 
                                           c('Stage IA', 'Stage IB', 'Stage IC'), 
                                           c('Stage IV')
    )
    # groups_tumor_grade = create_groups(all_data[,i],
    #                                    all_data[,'tumor_grade'], 
    #                                    'G1', 
    #                                    'G4')
    groups_brca_tertile  = create_groups(expression_matrix[,i],
                                           all_data[,'BRCA2_tertile_group'], 
                                           c('LOW'), 
                                           c('HIGH')
    )    
    
    vascular_invasion_test = compare_groups(groups_vascular$groupA, 
                                            groups_vascular$groupB)
    lymphovascular_invasion_test = compare_groups(groups_lymphovascular$groupA, 
                                                  groups_lymphovascular$groupB)
    clinical_stage_test = compare_groups(groups_clinical_stage$groupA, 
                                         groups_clinical_stage$groupB)
    brca_tertile_test = compare_groups(groups_brca_tertile$groupA, 
                                       groups_brca_tertile$groupB)
   
    output[name,] = c(
      vascular_invasion_t = vascular_invasion_test$statistic,
      vascular_invasion_p = vascular_invasion_test$p.value,
      lymphovascular_invasion_t =lymphovascular_invasion_test$statistic,
      lymphovascular_invasion_p =lymphovascular_invasion_test$p.value,
      clinical_stage_t =clinical_stage_test$statistic,
      clinical_stage_p =clinical_stage_test$p.value,
            # tumor_grade =
      brca_tertile_t = brca_tertile_test$statistic,
      brca_tertile_p = brca_tertile_test$p.value
      )

  }  
   output
}


head(retrieve_p_values(all_data))
