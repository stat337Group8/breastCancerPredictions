# input arguments
args <- commandArgs(trailingOnly=TRUE)

#read in file
imageData <- read.csv(args[1])

#find which rows have na
rowsWithoutNA <- imageData[rowSums(is.na(imageData)) == 0,]

#see which columns have na
# columnsWithNA <- names(which(colSums(is.na(imageData))>0))

# save new DF
write.csv(rowsWithoutNA, args.output)