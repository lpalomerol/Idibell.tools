#Functions

#Data
tma_data <- read.csv('./data/TMA-adyuvancia_time-revised.csv', sep=';')
tma_results <- read.csv('./data/TMA-resultados_HTT.csv', sep=';')

tma_all <- merge(tma_data, tma_results, by.x='BdT', by.y='X', all=F)

tma_all$GSDMB_g = ifelse(tma_all$GSDMB=='low', 0,1)
tma_all$HTT_g = ifelse(tma_all$HTT <= 1, 0, 1)
tma_all$HTT_g2 = ifelse(tma_all$HTT <= 0, 0, 1)

cor.test(tma_all$HTT, tma_all$GSDMB_g)
cor.test(tma_all$HTT_g, tma_all$GSDMB_g)
cor.test(tma_all$HTT_g2, tma_all$GSDMB_g)
