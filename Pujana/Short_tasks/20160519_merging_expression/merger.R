
DATA_DIR = './data'
OUTPUT_DIR = './output'


diff_exp <- read.csv(
  paste(DATA_DIR, '/DiffExp_DoubleTreat_FDR5.txt', sep=''), 
  sep='\t')

annotations <- read.csv(
  paste(DATA_DIR, '/GPL6244_avelina.txt', sep=''), 
  sep='\t')

head(annotations)
merged = merge(diff_exp, annotations, by.x='Probe.ID', by.y='Probe.Set.ID', all.x = TRUE, all.y=FALSE)
write.table(merged, 
            paste(OUTPUT_DIR, '/DiffExp_DoubleTreat_FDR5.GPL6244.txt', sep=''), 
            sep=' ', 
            col.names = TRUE, 
            row.names = FALSE)
head(merged)
