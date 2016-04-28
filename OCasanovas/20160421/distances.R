###########################################
#Distances between the six individuals
# Version 0.2 - Luis Palomero - 2016 04 28
###########################################

#Dependencies

#CONSTANTS
P_VALUE = 'p'
FDR = 'fdr'
THRESHOLD_0_1 = 0.1

TWO_DIMENSIONS = 2
SCALE = TRUE

#Data loading
setwd('C:/Users/lpalomero/Documents/GitHub/Idibell.tools/OCasanovas/20160421')
mRX03 <- read.table('./data/data_mRX03_withpvalues.txt', header=T)
hRX03 <- read.table('./data/data_hRX03_withpvalues.txt', header=T) 

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

build_cmd <- function(data, dimensions, scale){
  data.t = t(as.matrix(data[,-1:-3]))
  if(scale == TRUE){
    data.t = scale(data.t)    
  }

  cmdscale(dist(data.t), k=dimensions)
}

save_image <- function(cmd, image_name, plot_title){
  jpeg(image_name)
  plot(cmd, main=plot_title)
  text(cmd, labels = row.names(cmd), pos = 4)
  dev.off()
}


#For mice 
#Base model without filter

save_image( build_cmd(mRX03, TWO_DIMENSIONS, !SCALE ), 
            'cmd_mouse_nofilter_noscaled.png', 
            'CMD for mice no scaled - no filter')

save_image( build_cmd(mRX03, TWO_DIMENSIONS, SCALE ), 
            'cmd_mouse_nofilter_scaled.png', 
            'CMD for mice scaled - no filter')

#Model with filter by p-value (< 0.1)

mRX03.p.filtered = filter_by_threshold(mRX03, P_VALUE, THRESHOLD_0_1)

save_image( build_cmd(mRX03.p.filtered, TWO_DIMENSIONS, !SCALE ), 
            'cmd_mouse_p_filter_noscaled.png', 
            'CMD for mice no scaled - p_value < 0.1 filter')

save_image( build_cmd(mRX03.p.filtered, TWO_DIMENSIONS, SCALE ), 
            'cmd_mouse_p_filter_scaled.png', 
            'CMD for mice scaled - p_value < 0.1 filter')

rm(mRX03.p.filtered)

#Model with filter by FDR (< 0.1)
mRX03.fdr.filtered = filter_by_threshold(mRX03, FDR, THRESHOLD_0_1)

save_image( build_cmd(mRX03.fdr.filtered, TWO_DIMENSIONS, !SCALE ), 
            'cmd_mouse_fdr_filter_noscaled.png', 
            'CMD for mice no scaled - fdr < 0.1 filter')

save_image( build_cmd(mRX03.fdr.filtered, TWO_DIMENSIONS, SCALE ), 
            'cmd_mouse_fdr_filter_scaled.png', 
            'CMD for mice scaled - fdr < 0.1 filter')

rm(mRX03.fdr.filtered)


#In Humans

#Base model without filter

save_image( build_cmd(hRX03, TWO_DIMENSIONS, !SCALE ), 
            'cmd_human_nofilter_noscaled.png', 
            'CMD for human no scaled - no filter')

save_image( build_cmd(hRX03, TWO_DIMENSIONS, SCALE ), 
            'cmd_human_nofilter_scaled.png', 
            'CMD for human scaled - no filter')

#Model with filter by p-value (< 0.1)

hRX03.p.filtered = filter_by_threshold(hRX03, P_VALUE, THRESHOLD_0_1)

save_image( build_cmd(hRX03.p.filtered, TWO_DIMENSIONS, !SCALE ), 
            'cmd_human_p_filter_noscaled.png', 
            'CMD for human no scaled - p_value < 0.1 filter')

save_image( build_cmd(hRX03.p.filtered, TWO_DIMENSIONS, SCALE ), 
            'cmd_human_p_filter_scaled.png', 
            'CMD for human scaled - p_value < 0.1 filter')

rm(hRX03.p.filtered)

#Model with filter by FDR (< 0.1)
hRX03.fdr.filtered = filter_by_threshold(hRX03, FDR, THRESHOLD_0_1)

save_image( build_cmd(hRX03.fdr.filtered, TWO_DIMENSIONS, !SCALE ), 
            'cmd_human_fdr_filter_noscaled.png', 
            'CMD for human no scaled - fdr < 0.1 filter')

save_image( build_cmd(hRX03.fdr.filtered, TWO_DIMENSIONS, SCALE ), 
            'cmd_human_fdr_filter_scaled.png', 
            'CMD for human scaled - fdr < 0.1 filter')

rm(hRX03.fdr.filtered)