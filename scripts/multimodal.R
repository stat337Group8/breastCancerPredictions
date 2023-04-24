# load packages
# install.packages("optparse")
library(optparse)

# create options
options <- list(make_option(c("-i", "--input"), help="list of csvs to combine, space delimited"), make_option(c("-c", "--column"), help="column to merge by; needed if multiple data frames", default=NA))
parser <- OptionParser(option_list=options)
args <- parse_args(parser, positional_arguments=TRUE)

# prepare data
df <- NA
df.list <- list()

if (length(list(strsplit(args$input), " ")) == 1) {
    df <- read.csv(args$input)
} else {
for (file in list(strsplit(args$input), " ")) {
    append(df.list, read.csv(file))
}
df <- df.list[1]
i <- 2
while (i < length(df.list)) {
    df <- merge(df, df[i], by=args$column)
    i <- i+1
}
}

# create model


# train model, crossfold validate


# backward select using AIC


# metrics like accuracy and ROC/AUROC

