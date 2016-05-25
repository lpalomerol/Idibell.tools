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

args = commandArgs(trailingOnly=TRUE)

DATA_FILE = args[1]
RULES_FILE = args[2]
OUTPUT_FILE = args[3]

data_file = read.csv(DATA_FILE)
rules_file = read.csv(RULES_FILE, sep=';')
write('COLUMN1;COLUMN2;SUBGROUP;CORRELATION', OUTPUT_FILE)

 for(i in 1:nrow(rules_file)){
  rule = rules_file[i,]
  column1_lbl = as.character(rule$COLUMN1)
  column2_lbl = as.character(rule$COLUMN2)
  column1_data = as.numeric(data_file[,column1_lbl])
  column2_data = as.numeric(data_file[,column2_lbl])    
  
  if(is.na(rule$GROUP) == TRUE || rule$GROUP == '') {
    correlation = cor(
      column1_data, 
      column2_data,
      use='pairwise.complete.obs'
    )
    line_to_write = paste(build_rule_name(rule), correlation, sep=';')
    write(line_to_write, OUTPUT_FILE, append = TRUE)    
  } else {
    groups = levels(data_file[,as.character(rule$GROUP)])
    column_group = as.character(rule$GROUP)
    for(j in 1: length(groups)){
      curr_group = groups[j]
      curr_group_idxs = data_file[,column_group] == curr_group
      column1_subgroup_data = column1_data[curr_group_idxs] 
      column2_subgroup_data = column2_data[curr_group_idxs] 
      correlation = cor(
        column1_subgroup_data, 
        column2_subgroup_data,
        use='pairwise.complete.obs'
      )
      line_to_write = paste(build_rule_name(rule, curr_group), correlation, sep=';')
      write(line_to_write, OUTPUT_FILE, append = TRUE)
    }
  } 
}
