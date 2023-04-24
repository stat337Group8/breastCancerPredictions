# install packages
install.packages("missForest")
library("missForest")

# organize arguments
args <- c('./data/imageFixed.csv', FALSE, './data/imageFixed.csv')

# load data frame
df <- read.csv(args[1])
columns <- names(df)

# remove problematic columns (categorical variables that are not imputable)
if (args[2] == TRUE) {
  removeCols <- c('patient', 'mamAge', 'mamBDensity', 'mamShape', 'mamMargin', 'mamDistortion', 'mamLDensity', 'mamCalcifications', 'mamSize', 'usShape', 'usMargin', 'usSize', 'usEchogenicity', 'usSolid',  'usPAS')
  columns <- columns[!columns %in% c(removeCols)]
  patients <- df[,'patient']
} else {
  removeCols <- c('Patient.ID', 'X.1', 'X')
  columns <- columns[!columns %in% c(removeCols)]
  patients <- df[,'Patient.ID']
}

# impute data using random forest
imputed_missForest <- missForest(df[,columns])
df[,columns] <- imputed_missForest$ximp
df <- cbind(patients, df[,columns])

# save new data frame
write.csv(df, args[3])