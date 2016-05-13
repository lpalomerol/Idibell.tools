getwd()

raw_expression_data <- read.csv('./data/expression.txt')

expression_data = raw_expression_data[, !(names(raw_expression_data) %in% c('X'))]
colnames(expression_data) <- gsub('\\.', '-', colnames(expression_data))

write.table(expression_data, './output/expression.txt', sep=' ', 
            row.names = FALSE, quote=FALSE  )

