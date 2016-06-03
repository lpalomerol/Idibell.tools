#!/usr/bin/env Rscript

#install.packages('Kendall')
library(Kendall)

#This script executes column correlations inside one file.
#  The input params are these ones:
#    - DATA CSV FILE
#    - CORRELATIONS CSV FILE
#    - OUTPUT FILE

# Correlations file is a CSV with the following columns
# COLUMN1;COLUMN2;GROUP

build_rule_name <- function(rule, subgroup ='NA'){
  return(paste(rule$COLUMN1, rule$COLUMN2, subgroup, rule$METHOD, sep=';'))
}

write_correlation <- function(name, correlation, file){
  line_to_write = paste(name, correlation$estimate, correlation$p.value,  correlation$obs, sep=';')
  write(line_to_write, file, append = TRUE)    
}

do_kendall <- function(column1, column2){
  correlation = list(
    p.value=NA,
    estimate=NA,
    observation=NA
  )
  kendall_test = Kendall(column1, column2)
  correlation$p.value = kendall_test$sl[1]
  correlation$estimate = kendall_test$tau[1]
  correlation$observation='From Kendall package'
  return(correlation)
}

do_correlation <- function(column1, column2, method){
  correlation = list(
    p.value=NA,
    estimate=NA,
    observation=NA
  )
  tryCatch({
    if(method == 'kendall-b'){
      correlation = do_kendall(column1, column2)
    } else {
      correlation = cor.test(column1, column2, method=method)
    }
  },error = function(err){
    print(paste('ERROR DOING THE CORRELATION!', err))
  }, warning = function(warning){
    correlation$observation <<- warning
  }, finally={
    return(correlation)
  })
}

get_rule <- function(rules_file, i){
  rule = rules_file[i,]  
  rule$METHOD= ifelse(is.null(rule$METHOD), PEARSON, rule$METHOD)
  rule$GROUP= ifelse(is.null(rule$GROUP), '', rule$GROUP)
  rule
}

args = commandArgs(trailingOnly=TRUE)

DATA_FILE = args[1]
RULES_FILE = args[2]
OUTPUT_FILE = args[3]

PEARSON='pearson'

data_file = read.csv(DATA_FILE, sep=';')
rules_file = read.csv(RULES_FILE, sep=';', as.is=TRUE)
write('COLUMN1;COLUMN2;SUBGROUP;METHOD;CORRELATION;P-VALUE;OBS', OUTPUT_FILE)

for(i in 1:nrow(rules_file)){

  rule = get_rule(rules_file,i)
  column1_lbl = as.character(rule$COLUMN1)
  column2_lbl = as.character(rule$COLUMN2)
  column1_data = as.numeric(data_file[,column1_lbl])
  column2_data = as.numeric(data_file[,column2_lbl])
  if(rule$GROUP == '') {
    print(paste("Correlating", column1_lbl, column2_lbl, rule$METHOD))
    correlation = do_correlation(column1_data, column2_data, rule$METHOD )
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
       correlation = do_correlation(column1_subgroup_data, column2_subgroup_data, rule$METHOD)
       write_correlation(build_rule_name(rule, curr_group), correlation, OUTPUT_FILE)
     }
  } 
}
