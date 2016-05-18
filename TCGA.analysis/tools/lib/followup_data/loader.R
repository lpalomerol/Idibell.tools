followup.loader <- function(followup_filename, nte_filename= ''){
  followup_file = followup.load_file(followup_filename)
  if(nte_filename != ''){
    nte_file = followup.load_file(nte_filename)
    followup_file = merge(
      followup_file,
      nte_file,
      col='bcr_patient_barcode',
      all= TRUE
    )
  }
  followup_file
}

followup.load_file <- function(filename){
  raw = readLines(filename)
  #Remove unwanted headers
  raw = raw[-2:-3]
  read.csv(textConnection(raw), header = TRUE, stringsAsFactors = FALSE, sep='\t')
}

