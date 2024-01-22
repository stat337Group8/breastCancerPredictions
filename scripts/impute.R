# load necessary libraries
## install.packages("optparse")
library(optparse)


# insert command line arguments
options <- list(make_option(c("-i", "--input"), help="csv file path space delimited"), make_option(c("-c", "--column"), help="list of column names"), make_option(c("-o", "--output"), help="csv file path"))
parser <- OptionParser(option_list=options)
args <- parse_args(parser, positional_arguments=TRUE)


# load data frame
df <- read.csv(args$input)

# impute data
for (name in list(strsplit(args$column, " ")) {
    df[,name][is.na(df[,name])] <- mean(df[,name], na.rm=T)
}

# save new data frame
write.csv(df, args$output)
