args = commandArgs(trailingOnly=TRUE)

FILE_GROUP_TO_MERGE = args[1]
if(is.na(FILE_GROUP_TO_MERGE) == TRUE){
  stop('Please, define a input files group to merge')
}

#TODO: Count number of files automatically
MIN = 0
MAX = 251
loop = MIN
print(FILE_GROUP_TO_MERGE)
all_files = NULL;
while(loop <= MAX){
  path = paste(FILE_GROUP_TO_MERGE, loop, sep='.')
  file = read.csv(path, sep=' ')
  print(paste('Adding file', path, 'to all groups files'))
  print(paste('This file has this number of samples', ncol(file)))
  if(loop==0){
     all_files = file
  } else {
    all_files = merge(all_files, file, by=0, ALL=TRUE)
    row.names(all_files) = all_files$Row.names
    all_files = all_files[, !(names(all_files) %in% c('Row.names'))]
  }

  loop= loop +1
}
print(paste('Merging done, writing file at', FILE_GROUP_TO_MERGE, 'with',
            ncol(all_files), 'columns and ', nrow(all_files), 'rows'))
write.table(all_files, FILE_GROUP_TO_MERGE)
