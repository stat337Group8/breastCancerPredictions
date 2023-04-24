# install packages
install.packages("missForest")
library("missForest")

# organize arguments
args <- c('./data/reports.csv', TRUE, './data/reports2.csv')

# load data frame
df <- read.csv(args[1])
columns <- names(df)

if (args[2] == TRUE) {
  removeCols <- c('mamAge', 'mamBDensity', 'mamShape', 'mamMargin', 'mamDistortion', 'mamLDensity', 'mamCalcifications', 'mamSize', 'usShape', 'usMargin', 'usSize', 'usEchogenicity', 'usSolid',  'usPAS', 'recurrence')
  columns <- columns[!columns %in% c(removeCols)]
} else {
  columns <- columns
}

# impute data

imputed_missForest = missForest(df[,columns][,-1])
forestImp = imputed_missForest$ximp

# save new data frame
write.csv(df, args[3])