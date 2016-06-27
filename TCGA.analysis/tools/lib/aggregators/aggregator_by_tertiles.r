AggregatorByTertiles <- function(values){
  tertiles = quantile(values, c(1/3, 2/3))
  return(
    ifelse(values < tertiles[1], 0,
           ifelse( values < tertiles[2], 1, 2))
  )
}