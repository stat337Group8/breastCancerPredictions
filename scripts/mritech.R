# load necessary packages
install.packages("ggplot2")
library("ggplot2")

# load data
dforig <- read.csv("./data/mriFixed.csv")
dfy <- dforig$recurrence
df <- subset(dforig, select=-c(X, patients, recurrence))

# set random seed
set.seed(2022)

# reduce data dimensionality
## Perform PCA 
pr.out <- prcomp(df , scale = TRUE)
phi <- pr.out$rotation # save for next predicting dataset so that predictions are appropriately handled
Z <- pr.out$x

## Graph PC1/PC2
pcs <- data.frame(Z[,1:2])
pcs <- cbind(pcs, dforig$recurrence)
ggplot(data = pcs)+
  geom_point(aes(x=PC1,y=PC2, color=dforig$recurrence), size=2)+
  theme_bw()