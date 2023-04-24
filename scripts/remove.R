# load necessary libraries
#install.packages("optparse")
library(optparse)


# insert command line arguments
options <- list(make_option(c("-i", "--input"), help="csv file path space delimited"), make_option(c("-o", "--output"), help="csv file path"))
parser <- OptionParser(option_list=options)
args <- parse_args(parser, positional_arguments=TRUE)


# load data frame
df <- read.csv(args$input)

#find which rows have na
rowsWithoutNA <- imageData[rowSums(is.na(imageData)) == 0,]

# save new DF
write.csv(rowsWithoutNA, args$output)
