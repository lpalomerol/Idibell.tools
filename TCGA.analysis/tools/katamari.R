args=commandArgs(trailingOnly = TRUE)

FOLDER_ID=1
OUTPUT_FILE_ID =2

folder_path = args[FOLDER_ID]
output_file = args[OUTPUT_FILE_ID]

build_path <- function(dir, file){
  paste(dir, file, sep='/')
}

open_file <- function(path){
  read.csv(path, sep='\t')
}

attach_to_katamari <- function(katamari, path){
  file = open_file(path)
  return(merge(katamari, file, by=1, all = TRUE))
}

save_katamari <- function(katamari, path){
  write.table(katamari, path, sep='\t', quote = FALSE, row.names = FALSE, col.names=TRUE)  
}

folder = list.files(folder_path, pattern = '*.txt')

print("Let's start katamari!")
katamari = open_file(build_path(folder_path, folder[1]))

for(i in 2:length(folder)){
  if( i %% 50 == 0){
    print(i)    
  }

  katamari = attach_to_katamari(katamari, build_path(folder_path, folder[i]))
}

print(paste("Katamari stored at '", output_file, "'", sep=''))
save_katamari(katamari, output_file)
