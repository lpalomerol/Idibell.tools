somatic_mutations.loader = function(file){
  raw = readLines(file)
  #Remove unwanted headers
  raw = raw[-2:-3]
  read.csv(textConnection(raw), header = TRUE, stringsAsFactors = FALSE, sep='\t')
}
