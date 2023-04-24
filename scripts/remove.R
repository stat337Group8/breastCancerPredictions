# load necessary libraries
#install.packages("optparse")
library(optparse)


# insert command line arguments
options <- list(make_option(c("-i", "--input"), help="csv file path space delimited"), make_option(c("-o", "--output"), help="csv file path"), make_option(c("-s", "--saveIndices"), help="save path for indices removed"), make_option(c("-n", "--name"), help="name of index"))
parser <- OptionParser(option_list=options)
args <- parse_args(parser, positional_arguments=TRUE)


# load data frame
df <- read.csv(args$input)

#find which rows have na
##rowsWithoutNA <- imageData[rowSums(is.na(imageData)) == 0,]
rowsWithoutNA <- imageData[complete.cases(imageData), ]

# save new DF
write.csv(rowsWithoutNA, args$output)

# save ids removed
## write.csv(imageData[rowSums(is.na(imageData)) > 0,args$name], args$saveIndices)
write.csv(imageData[!complete.cases(imageData), args$name], args$saveIndices)
