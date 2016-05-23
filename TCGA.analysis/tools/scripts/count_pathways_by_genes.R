# if (length(args)==0) {
#   stop("At least one argument must be supplied (input file).n", call.=FALSE)
# } else if (length(args)==1) {
#   # default output file
#   args[2] = "out.txt"
# }
# INPUT_FILE = args[1]
# pOUTPUT_FILE = args[2]
INPUT_FILE = './data/common_kegg_pathways.csv'
OUTPUT_FILE = './output/common_kegg_pathways_catalog.csv'
pathways = read.csv(INPUT_FILE, sep=';')


catalog = data.frame(
  length = numeric(),
  pathways = character(),
  stringsAsFactors = FALSE
)


add_gene_to_catalog <- function(catalog, gene, pathway_name){
  if(gene %in% rownames(catalog) == TRUE){
    curr_pathway = catalog[gene,]$pathways
    curr_pathway = paste(curr_pathway, pathway_name, sep=';')
    catalog[gene,] = c((as.numeric(catalog[gene,]$length)+1), curr_pathway)
  } else {
    catalog[gene,] = c(1, pathway_name)
  }
  catalog
}


add_pathways_genes_to_catalog <- function(catalog, pathway_name, pathway_genes){
  for(i in 1:length(pathway_genes)){
    gene= as.character(pathway_genes[i])
    if((is.na(gene) == FALSE && gene != '')){
      catalog = add_gene_to_catalog(catalog, gene, pathway_name)
    }
  }
  catalog
}


pathway_names= colnames(pathways)
for(i in 1:ncol(pathways)){
  name = pathway_names[i]
  genes = (pathways[,name])
  catalog = add_pathways_genes_to_catalog(catalog, name, genes)
}
catalog = catalog[order(as.numeric(catalog$length)*-1),]

write.table(catalog, OUTPUT_FILE, quote = FALSE, sep=';')
