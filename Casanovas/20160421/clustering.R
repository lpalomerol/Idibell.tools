###########################################
#CLustering
# Version 0.2 - Luis Palomero - 2016 04 28
###########################################
# #Libraries
#install.packages('amap')
library(amap)

#Constants
PROBES = 'p'
SAMPLES = 's'

P_VALUE = 'p'
FDR = 'fdr'
THRESHOLD_0_1 = 0.1

WITH_NAMES = TRUE
SCALE = TRUE

#Data loading
setwd('C:/Users/lpalomero/Documents/GitHub/Idibell.tools/OCasanovas/20160421')
mRX03 <- read.table('./data/data_mRX03_withpvalues.txt', header=T)
hRX03 <- read.table('./data/data_hRX03_withpvalues.txt', header=T) 

head(mRX03)
rownames(mRX03) <- mRX03$NAME
rownames(hRX03) <- hRX03$NAME

#Functions
filter_by_threshold <- function(data, column, threshold){
  if(column == P_VALUE){
    data[data$p < threshold,]
  } else if (column == FDR) {
    data[data$FDR < threshold,]    
  } else {
    stop("Invalid criteria, please define p-value or fdr")
  }
}

reduce <- function(data){
  data[,-1:-3]
}

make_cluster <- function(data, type, scale=FALSE ) {

  reduced = reduce(data)

  if (scale == TRUE){
    reduced = scale(reduced, center = TRUE, scale= TRUE)
  }  

  if (type == SAMPLES) {
    hcluster(t(reduced), method='euclidean', link='ward') #Caution, transposed
  } else if( type == PROBES) {
    hcluster(reduced, method='correlation', link='ward')    
  } else {
    stop("Please define a valid dimension, PROBES or SAMPLES")
  }
}

save_plot <- function(data, image_name, plot_title ){
  png(image_name, width=1280, height=1280)
  plot(data, main=plot_title)
  dev.off()
}

#By mice
#Base model without filter


save_plot(make_cluster(mRX03, PROBES),
          'clus_mouse_probes_nofilter.png', 
          'Mouse - Clustering the probes without filter')

save_plot(make_cluster(mRX03, SAMPLES),
          'clus_mouse_samples_nofilter.png', 
          'Mouse - Clustering the samples without filter')

save_plot(make_cluster(mRX03, PROBES, SCALE),
          'clus_mouse_probes_nofilter_scale.png', 
          'Mouse - Clustering the probes without filter - scale')

save_plot(make_cluster(mRX03, SAMPLES, SCALE),
          'clus_mouse_samples_nofilter_scale.png', 
          'Mouse - Clustering the samples without filter - scale')


#Model with filter by p-value
mRX03.p.filter = filter_by_threshold(mRX03, P_VALUE, THRESHOLD_0_1)

save_plot(make_cluster(mRX03.p.filter, PROBES),
          'clus_mouse_probes_p_value.png', 
          'Mouse - Clustering the probes with p_value < 0.1 filter')

save_plot(make_cluster(mRX03.p.filter, SAMPLES),
          'clus_mouse_samples_p_value.png', 
          'Mouse - Clustering the samples with p_value < 0.1 filter')


save_plot(make_cluster(mRX03.p.filter, PROBES, SCALE),
          'clus_mouse_probes_p_value_scale.png', 
          'Mouse - Clustering the probes with p_value < 0.1 filter - scale')

save_plot(make_cluster(mRX03.p.filter, SAMPLES, SCALE),
          'clus_mouse_samples_p_value_scale.png', 
          'Mouse - Clustering the samples with p_value < 0.1 filter - scale')

#Model with filter by fdr
head(mRX03)
mRX03.fdr.filter = filter_by_threshold(mRX03, FDR, THRESHOLD_0_1)

head(mRX03.fdr.filter)

save_plot(make_cluster(mRX03.fdr.filter, PROBES),
          'clus_mouse_probes_fdr.png', 
          'Mouse - Clustering the probes with fdr < 0.1 filter')

save_plot(make_cluster(mRX03.fdr.filter, SAMPLES),
          'clus_mouse_samples_fdr.png', 
          'Mouse - Clustering the samples with fdr < 0.1 filter')


save_plot(make_cluster(mRX03.fdr.filter, PROBES, SCALE),
          'clus_mouse_probes_fdr_scale.png', 
          'Mouse - Clustering the probes with fdr < 0.1 filter - scale')

save_plot(make_cluster(mRX03.fdr.filter, SAMPLES, SCALE),
          'clus_mouse_samples_fdr_scale.png', 
          'Mouse - Clustering the samples with fdr < 0.1 filter - scale')


#By human
#Base model without filter

save_plot(make_cluster(hRX03, PROBES),
          'clus_human_probes_nofilter.png', 
          'Human - Clustering the probes without filter')

save_plot(make_cluster(hRX03, SAMPLES),
          'clus_human_samples_nofilter.png', 
          'Human - Clustering the samples without filter')

save_plot(make_cluster(hRX03, PROBES, SCALE),
          'clus_human_probes_nofilter_scale.png', 
          'Human - Clustering the probes without filter - scale')

save_plot(make_cluster(hRX03, SAMPLES, SCALE),
          'clus_human_samples_nofilter_scale.png', 
          'Human - Clustering the samples without filter - scale')


#Model with filter by p-value
hRX03.p.filter = filter_by_threshold(hRX03, P_VALUE, THRESHOLD_0_1)

save_plot(make_cluster(hRX03.p.filter, PROBES),
          'clus_human_probes_p_value.png', 
          'Human - Clustering the probes with p_value < 0.1 filter')

save_plot(make_cluster(hRX03.p.filter, SAMPLES),
          'clus_human_samples_p_value.png', 
          'Human - Clustering the samples with p_value < 0.1 filter')


save_plot(make_cluster(hRX03.p.filter, PROBES, SCALE),
          'clus_human_probes_p_value_scale.png', 
          'Human - Clustering the probes with p_value < 0.1 filter - scale')

save_plot(make_cluster(hRX03.p.filter, SAMPLES, SCALE),
          'clus_human_samples_p_value_scale.png', 
          'Human - Clustering the samples with p_value < 0.1 filter - scale')



#Model with filter by fdr
hRX03.fdr.filter = filter_by_threshold(hRX03, FDR, THRESHOLD_0_1)

save_plot(make_cluster(hRX03.fdr.filter, PROBES),
          'clus_human_probes_fdr.png', 
          'Human - Clustering the probes with fdr < 0.1 filter')

save_plot(make_cluster(hRX03.fdr.filter, SAMPLES),
          'clus_human_samples_fdr.png', 
          'Human - Clustering the samples with fdr < 0.1 filter')

save_plot(make_cluster(hRX03.fdr.filter, PROBES, SCALE),
          'clus_human_probes_fdr_scale.png', 
          'Human - Clustering the probes with fdr < 0.1 filter - scale')

save_plot(make_cluster(hRX03.fdr.filter, SAMPLES, SCALE),
          'clus_human_samples_fdr_scale.png', 
          'Human - Clustering the samples with fdr < 0.1 filter - scale')
