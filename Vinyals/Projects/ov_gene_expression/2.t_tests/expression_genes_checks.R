ROOT_DIRECTORY = '../../../..'
DATA_FOLDER_DIRECTORY = '../prepare_data/output'
install.packages('survival')
library(survival)

source(
  paste(ROOT_DIRECTORY, '/TCGA.analysis/tools/lib/clinical_data/loader.R', sep='')
)

source(
  paste(ROOT_DIRECTORY, '/TCGA.analysis/tools/survival_analysis_tools.R', sep='')
)

source(
  paste(ROOT_DIRECTORY, '/TCGA.analysis/tools/lib/followup/load.R', sep='')
)



clinical_data.filter <- function(clinical_data){
  keep <- c('bcr_patient_barcode', 'tumor_grade', 'vascular_invasion_indicator', 
            'lymphovascular_invasion_indicator', 'clinical_stage'  )
  clinical_data[, colnames(clinical_data) %in% keep]
}



compare_groups <- function(groupA, groupB){
  t.test(groupA, groupB)
}

check_cox <- function(surv, expressions){

  expression_median = median(expressions, na.rm=TRUE)
  median_group = ifelse(expressions < expression_median, 0,1 )
  cox = coxph(surv ~ median_group)
  return(
    list(
      model = cox,
      hazard.ratio =summary(cox)$coefficients[2],
      p.value =summary(cox)$coefficients[5]
    )
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
    shapiro = numeric(0),
    vascular_invasion_t = numeric(0),
    vascular_invasion_p = numeric(0),
    lypmhovascular_invasion_t = numeric(0),
    lypmhovascular_invasion_p = numeric(0),
    tumor_grade_t = numeric(0),
    tumor_grade_g = numeric(0),
    clinical_stage_t = numeric(0),
    clinical_stage_p = numeric(0),
    brca_tertile_t = numeric(0),
    brca_tertile_p = numeric(0),
    brca2_mutant_t = numeric(0),
    brca2_mutant_p = numeric(0),
    brca1_2_mutant_t = numeric(0),
    brca1_2_mutant_p = numeric(0),
    relapse_cox_hr = numeric(0),
    relapse_cox_p = numeric(0)
  )

  non_expression_columns = c(
    'bcr_patient_barcode', 'tumor_grade', 
    'vascular_invasion_indicator', 'clinical_stage',
    'lymphovascular_invasion_indicator', 
    'BRCA2_tertile_group', 'BRCA2_mutant',
    'BRCA1_2_mutant',"X",
    "last", "death", "recur", "bcr_followup_barcode",  "time.death",
    "event.death", "time.recur", "event.recur"
  )  

  expression_matrix = all_data[,
    !(colnames(all_data) %in% non_expression_columns)
  ]

  relapse_surv =  Surv(all_data$time.recur, all_data$event.recur==1) 
  
  cols = colnames(expression_matrix)
  GENES_RANGE =1:ncol(expression_matrix)
  for(i in GENES_RANGE){
    name = cols[i]
    groups_vascular = create_groups(expression_matrix[,i],
                                    all_data[,'vascular_invasion_indicator'], 
                                    c('YES'), 
                                    c('NO'))


        
    groups_lymphovascular = create_groups(expression_matrix[,i],
                                          all_data[,'lymphovascular_invasion_indicator'], 
                                          c('YES'), 
                                          c('NO') 
    )
    
    groups_clinical_stage  = create_groups(expression_matrix[,i],
                                           all_data[,'clinical_stage'], 
                                           c('Stage IA', 'Stage IB', 'Stage IC'), 
                                           c('Stage IV')
    )

    groups_tumor_grade = create_groups(expression_matrix[,i],
                                       all_data[,'tumor_grade'], 
                                       c('G1'), 
                                       c('G3','G4')
    )

    groups_brca_tertile  = create_groups(expression_matrix[,i],
                                         all_data[,'BRCA2_tertile_group'],
                                         c('LOW'),
                                         c('HIGH')
    )

    groups_brca_mutant  = create_groups(expression_matrix[,i],
                                        all_data[,'BRCA2_mutant'],
                                        c('NO'),
                                        c('YES')
    )

    groups_brca1_2_mutant  = create_groups(expression_matrix[,i],
                                        all_data[,'BRCA1_2_mutant'],
                                        c('NO'),
                                        c('YES')
    )
            
    shapiro = shapiro.test(expression_matrix[,i])
    vascular_invasion_test = compare_groups(groups_vascular$groupA, 
                                            groups_vascular$groupB)
    lymphovascular_invasion_test = compare_groups(groups_lymphovascular$groupA, 
                                                  groups_lymphovascular$groupB)
    clinical_stage_test = compare_groups(groups_clinical_stage$groupA, 
                                         groups_clinical_stage$groupB)
    tumor_grade_test = compare_groups(groups_tumor_grade$groupA, 
                                      groups_tumor_grade$groupB)
    brca_tertile_test = compare_groups(groups_brca_tertile$groupA,
                                       groups_brca_tertile$groupB)
    brca2_mutant_test = compare_groups(groups_brca_mutant$groupA,
                                       groups_brca_mutant$groupB)
    brca1_2_mutant_test = compare_groups(groups_brca1_2_mutant$groupA,
                                       groups_brca1_2_mutant$groupB)
    
    relapse_survival_test = check_cox(relapse_surv, expression_matrix[,i])
    
    output[name,] = c(
      shapiro = shapiro$p.value,
      vascular_invasion_t = vascular_invasion_test$statistic,
      vascular_invasion_p = vascular_invasion_test$p.value,
      lymphovascular_invasion_t =lymphovascular_invasion_test$statistic,
      lymphovascular_invasion_p =lymphovascular_invasion_test$p.value,
      clinical_stage_t =clinical_stage_test$statistic,
      clinical_stage_p =clinical_stage_test$p.value,
      tumor_grade_t =tumor_grade_test$statistic,
      tumor_grade_p =tumor_grade_test$p.value,
      brca_tertile_t = brca_tertile_test$statistic,
      brca_tertile_p = brca_tertile_test$p.value,
      brca2_mutant_t = brca2_mutant_test$statistic,
      brca2_mutant_p = brca2_mutant_test$p.value,
      brca1_2_mutant_t = brca1_2_mutant_test$statistic,
      brca1_2_mutant_p = brca1_2_mutant_test$p.value,
      relapse_cox_hr = relapse_survival_test$hazard.ratio,
      relapse_cox_p =  relapse_survival_test$p.value
      )

  }  
   output
}


#Read and prepare clinical_data file
clinical_data_file = paste(DATA_FOLDER_DIRECTORY, '/clinical.txt', sep='')
clinical_data <- clinical_data.filter(
  clinical_data.loader(clinical_data_file)
)

#Read and prepare expression data file
expression_data_file <-   paste(DATA_FOLDER_DIRECTORY, '/expression.subset.txt', sep='')
expression_data <- load_gene_expression_data(expression_data_file,sep=' ')
rownames(expression_data) <- expression_data$bcr_patient_barcode

barcodes = expression_data$bcr_patient_barcode
expression_data = lapply(expression_data, function(row){as.numeric(as.character(row))})
expression_data$bcr_patient_barcode = barcodes 

#Read and prepare BRCA2 mutated data file
mutated_brca2 <- read.csv(
  paste(DATA_FOLDER_DIRECTORY, '/brca2_mutated.txt', sep='')
)

#Read and prepare BRCA1+BRCA2 mutated data file
mutated_brca1_2 <- read.csv(
  paste(DATA_FOLDER_DIRECTORY, '/brca1_2_mutated.txt', sep='')
)

#Read and prepare FOLLOWUP 

followup <- followup.load(
    paste(DATA_FOLDER_DIRECTORY, '/follow_up.txt', sep='')
)

#Merge all data files and add BRCA2 expression columns
all_data= merge(clinical_data, expression_data, by="bcr_patient_barcode")
all_data = merge(all_data, followup, 
                 by.x='bcr_patient_barcode', by.y='barcode', all=TRUE)

all_data$BRCA2_tertile_group = cut(
  all_data$BRCA2,
  breaks = quantile(all_data$BRCA2, probs=seq(0,1, by=0.33), na.rm =TRUE), 
  labels=c('LOW','MED', 'HIGH'),
  include.lowest= TRUE
)

all_data$BRCA2_mutant = ifelse(
  all_data$bcr_patient_barcode %in% mutated_brca2$bcr_patient_barcode, 'YES', 'NO'
)

all_data$BRCA1_2_mutant = ifelse(
  all_data$bcr_patient_barcode %in% mutated_brca1_2$bcr_patient_barcode, 'YES', 'NO'
)

tests = (retrieve_p_values(all_data))

write.csv(tests, './output/t_tests.txt')