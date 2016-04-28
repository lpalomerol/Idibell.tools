###########################################
#CLustering
# Version 0.1 - Luis Palomero - 2016 04 21
###########################################
#Libraries
install.packages('amap')
library(amap)
install.packages('ape')
library(ape)
#Data loading
mRX03 <- read.table('mRX03.txt', header=T)
hRX03 <- read.table('hRX03.txt', header=T) 

head(mRX03)
head(hRX03)

#By mice
hcl <- hcluster(t((mRX03[,-1:-2])), method='euclidean', link="ward"); # cluster con las muestras (trasponer!!!)
hcl2 <- hcluster((mRX03[,-1:-2]), method='correlation', link="ward"); # cluster con las sondas
jpeg('clus_mouse_individuals.jpeg')
plot(hcl, main="Cluster of individuals (mice)")
dev.off()

CL1 <- as.hclust(hcl2)
CL2 <- as.phylo(CL1)
jpeg('clus_mouse_samples.jpeg', width=2048, height = 2048)
plot(CL2, type="fan", cex=0.5, main="Cluster of genes (mice)")
dev.off()

#By humans
hcl <- hcluster(t((hRX03[,-1:-2])), method='euclidean', link="ward"); # cluster con las muestras (trasponer!!!)
hcl2 <- hcluster((hRX03[,-1:-2]), method='correlation', link="ward"); # cluster con las sondas
jpeg('clus_humans_individuals.jpeg')
plot(hcl, main="Cluster of individuals (humans)")
dev.off()

CL1 <- as.hclust(hcl2)
CL2 <- as.phylo(CL1)
jpeg('clus_humans_samples.jpeg', width=2048, height = 2048)
plot(CL2, type="fan", cex=0.5, main="Cluster of genes (humans)")
dev.off()
