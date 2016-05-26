#!/usr/bin/env Rscript

#This script executes column correlations inside one file.
#  The input params are these ones:
#    - DATA CSV FILE
#    - CORRELATIONS CSV FILE
#    - OUTPUT FILE

# Correlations file is a CSV with the following columns
# COLUMN1;COLUMN2;GROUP

build_rule_name <- function(rule, subgroup ='NA'){
  return(paste(rule$COLUMN1, rule$COLUMN2, subgroup, sep=';'))
}

write_correlation <- function(name, correlation, file){
  line_to_write = paste(name, correlation$estimate, correlation$p.value, sep=';')
  write(line_to_write, file, append = TRUE)    
}

do_correlation <- function(column1, column2){
  correlation = list(
    p.value=NA,
    estimate=NA
  )
  tryCatch({
    correlation = cor.test(column1, column2)
  },error =function(err){
    print(paste('ERROR DOING THE CORRELATION!', err))
  }, finally={
    return(correlation)
  })
}

args = commandArgs(trailingOnly=TRUE)

DATA_FILE = args[1]
RULES_FILE = args[2]
OUTPUT_FILE = args[3]

data_file = read.csv(DATA_FILE, sep=';')
rules_file = read.csv(RULES_FILE, sep=';')
write('COLUMN1;COLUMN2;SUBGROUP;CORRELATION;P-VALUE', OUTPUT_FILE)

for(i in 1:nrow(rules_file)){

  rule = rules_file[i,]
  column1_lbl = as.character(rule$COLUMN1)
  column2_lbl = as.character(rule$COLUMN2)
  column1_data = as.numeric(data_file[,column1_lbl])
  column2_data = as.numeric(data_file[,column2_lbl])    
  if(is.na(rule$GROUP) == TRUE || rule$GROUP == '') {
    correlation = do_correlation(column1_data, column2_data)
    write_correlation(build_rule_name(rule), correlation, OUTPUT_FILE)
  } else {
    groups = levels(data_file[,as.character(rule$GROUP)])
    column_group = as.character(rule$GROUP)
     for(j in 1: length(groups)){

       curr_group = groups[j]
       
       curr_group_idxs = data_file[,column_group] == curr_group
       column1_subgroup_data = column1_data[curr_group_idxs] 
       column2_subgroup_data = column2_data[curr_group_idxs] 
       print(curr_group)
       correlation = do_correlation(column1_subgroup_data, column2_subgroup_data)
       write_correlation(build_rule_name(rule, curr_group), correlation, OUTPUT_FILE)
     }
  } 
}
