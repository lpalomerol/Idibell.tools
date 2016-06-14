library("parallel")
library("optparse")

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL,
              help="Dataset file name", metavar="character"),

  make_option(c("-l", "--left-columns"), type="character", default=NULL,
              help="Columns used to comare vs the rest, the columns should be separated by comas",
              metavar="character"),

  make_option(c("-o", "--out"), type="character", default="./",
              help="output folder name [default= %default]", metavar="character")
);

description="This scripts executes some correlations tests between a set of columns and the rest of them."


validate <- function(opt){
  if (is.null(opt$file) | is.null(opt$`left-columns`) ){
    print_help(opt_parser)
    stop("Invalid arguments")
  }
}

split_columns <- function(columns){
  strsplit(columns, ',')[[1]]
}

gene_expression.load <- function(filename, first_column_name='', separator=';'){
  expression_file = read.csv(filename, sep=separator, as.is=TRUE, header=TRUE)
  if(first_column_name != ''){
    rownames(expression_file) =   expression_file[,first_column_name]
  }
  expression_file[!colnames(expression_file) %in% c(first_column_name)]
}

gene_expression.0_to_NA <- function(expression){
  expression[expression==0] <- NA
  return(expression)
}

opt_parser = OptionParser(option_list=option_list, description = description);
opt = parse_args(opt_parser);
validate(opt)
DATASET_FILE_NAME=opt$file
LEFT_COLUMNS=opt$'left-columns'
OUTPUT_FOLDER_NAME=opt$out
#
# DATASET_FILE_NAME= "C:/Temp/8.5.2/pipeline_temp/RNASeqV2_expression_genes_merged_cut.txt"
# LEFT_COLUMNS="TCF4|6925,AHR|196,AREG|374,RNF169|254225"
# LEFT_COLUMNS="TCF4.6925,AHR.196,AREG.374,RNF169.254225"
# OUTPUT_FOLDER_NAME="C:/Temp/8.5.2/pipeline_temp"

#dataset_bck=dataset

dataset = gene_expression.load(DATASET_FILE_NAME, 'gene_id', separator=';')
dataset = gene_expression.0_to_NA(dataset)
dataset = log(dataset)


correlate_all_paral <- function(gene_name, dataset,output_folder='./output'){
  library("parallel")
  genes = colnames(dataset)
  get_data_name <- function(columnA, columnB){
    paste(colnames(columnA)[[1]], 'and', colnames(columnB)[[1]] )
  }
  correlate_pairs <- function(columnA, columnB){

    test_result = list(
      'p.value'      = NA,
      'estimate'     = NA,
      'data.name'    = get_data_name(columnA, columnB),
      x.n            = sum(!is.na(columnA[[1]])),
      y.n            = sum(!is.na(columnB[[1]]))
    )
    tryCatch({
      res = cor.test(columnA[[1]], columnB[[1]])
      test_result = list(
        'p.value'      = res$'p.value',
        'estimate'     = res$estimate,
        'data.name'    = get_data_name(columnA, columnB),
        x.n            = sum(!is.na(columnA[[1]])),
        y.n            = sum(!is.na(columnB[[1]]))
      )
    }, error = function(err){
      # print(paste("Error doing the test", err))
    },finally={
      return(test_result)
    })

  }

  output_file_name= paste(output_folder, '/', URLencode(gene_name), '.csv', sep='')

  # Calculate the number of cores
  no_cores <- 7
  # Initiate cluster
  cl <- makeCluster(no_cores)

  all_correlations = parSapply(cl,genes, function(gen){
   correlate_pairs(dataset[gen],dataset[gene_name] )
  })
  write.csv(t(all_correlations), output_file_name)
  stopCluster(cl)
}


genes_to_correlate = split_columns(LEFT_COLUMNS)
sapply(genes_to_correlate, correlate_all_paral, dataset=dataset,  output_folder=OUTPUT_FOLDER_NAME)
