#Libraries
library(survival)

#Prepare tma_data
load_tma_data <- function(data_file, results_file){
  tma_data <- read.csv(data_file, sep=';')
  tma_results <- read.csv(results_file, sep=';')
  all_data <- merge(tma_data, tma_results, by.x='BdT', by.y='X', all=FALSE)
  all_data$HTT_g <- ifelse(all_data$HTT == 0, 0,1)
  all_data$DFS = as.numeric(as.character(all_data$DFS))      
  all_data$RELAPSE_g = ifelse(all_data$RELAPSE == 'SI', 1,0)
  all_data
}

do.correlations <- function(x,y){
  contingency = table(x,y)
  
  list(
    corr = cor.test(x,y),
    fisher= fisher.test(contingency)    
  )
}

print_plot <- function(model, title){
  plot(model,
       col=c('red','green','blue'),
       xlab='Relapse time',
       ylab='Survival probabilities',
       main=title
  )
  legend('bottomleft',
         c('Htt=0', 'Htt=1','Htt=2'),
         col=c('red','green','blue'),
         lty=1
  )
}

#Let's prepare the data
all_data = load_tma_data(
  './data/TMA-adyuvancia_time-revised.csv',
  './data/TMA-resultados_HTT.csv'
)


all_amplified = all_data[all_data$HER2_FISH=='amplified',]
all_ihc = all_data[all_data$HER2_IHC=='3+' | all_data$HER2_IHC=='2+',]
head(all_data)

#TASKS:
# - Correlation between htt and relapse 
# - Cox survival mode with dfs and htt, time is dfs and "event" is htt


do.correlations(all_data$RELAPSE_g, all_data$HTT)
do.correlations(all_amplified$RELAPSE_g, all_amplified$HTT)
do.correlations(all_ihc$RELAPSE_g, all_ihc$HTT)

#Cox model for relapse and htt
surv_all = Surv(all_data$DFS, all_data$RELAPSE_g==1)
print_plot(survfit(surv_all~all_data$HTT),'Survdiff for all rows')
coxph(surv_all ~ all_data$HTT)

surv_amplified = Surv(all_amplified$DFS, all_amplified$RELAPSE_g==1)
print_plot(survfit(surv_amplified~all_amplified$HTT),'Survdiff for all HER2_FISH amplified rows')
coxph(surv_amplified ~  all_amplified$HTT)

surv_ihc = Surv(all_ihc$DFS, all_ihc$RELAPSE_g)
print_plot(survfit(surv_ihc~all_ihc$HTT),'Survdiff for all HER2_IHC >= 2+ rows')
coxph(surv_ihc ~  all_ihc$HTT)
