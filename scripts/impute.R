# install packages
install.packages("missForest")
library("missForest")

# organize arguments
args <- c('./data/reportFixed.csv', TRUE, './data/reportFixed.csv') # TRUE for reports, FALSE for any others

# load data frame
df <- read.csv(args[1])
columns <- names(df)

# remove problematic columns (categorical variables that are not imputable)
if (args[2] == TRUE) {
  # this statement if the datset used is report characteristics
  # remove non-imputable columns
  removeCols <- c('patient', 'mamAge', 'mamBDensity', 'mamShape', 'mamMargin', 'mamDistortion', 'mamLDensity', 'mamCalcifications', 'mamSize', 'usShape', 'usMargin', 'usSize', 'usEchogenicity', 'usSolid',  'usPAS')
  columns <- columns[!columns %in% c(removeCols)]
  patients <- df[,'patient']
} else {
  # remove non-imputable columns
  removeCols <- c('patients', 'X')
  columns <- columns[!columns %in% c(removeCols)]
  patients <- df[,'patients']
}

# impute data using random forest
imputed_missForest <- missForest(df[,columns])
df[,columns] <- imputed_missForest$ximp
df <- cbind(patients, df[,columns]) # re-add patients columns for indexing purposes

# save new data frame
write.csv(df, args[3])
