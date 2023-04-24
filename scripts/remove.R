# organize inputs
args <- c('./data/demographics.csv', './data/removeIndices.csv', './data/demographicsFixed.csv', FALSE)

# load data frame
df <- read.csv(args[1])

#find which rows have na
if (args[4] == TRUE) {
  indices <- complete.cases(df)
  write.csv(indices, args[2])
} else {
  indices <- read.csv(args[2])
  indices <- indices[,'x']
}

# save new DF
write.csv(df[indices,], args[3])
