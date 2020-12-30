library('dplyr')
library('tidyverse')
library('cluster')
library('reshape')
library('ggplot2')
library('som')


data <- read.csv('/Users/hubertchen/Desktop/UW/2020 Winter/CET 521 Inferential Data Analysis For Engineers/Final project/ProcessedDataset.csv')
set.seed(546)


#############################
# X: company
# Sector: categorical
# Class: 0 or 1
# Revenue.Group
#############################


# remove unused columns
data<-subset(data,select=-c(X.1))
data<-subset(data,select=-c(cashConversionCycle))
data<-subset(data,select=-c(operatingCycle))
sector<-data[c(224)]


# convert continuous variable to numeric
data[2:220]<-data.frame(data.matrix(data[2:220]))
data[222]<-data.frame(data.matrix(data[222]))


# replace na in all columns with their own mean
for (i in 2:222) {
  if (i != 221) {
    data[,i][is.na(data[,i])]<-mean(data[,i], na.rm=TRUE)
  }
}

# standardize variables beside Revenue.Growth(dependent variable)
data[2]<-scale(data[2],center=TRUE,scale=TRUE)
for (i in 4:222) {
  if (i != 221) {
    data[i]<-scale(data[i],center=TRUE,scale=TRUE)
  }
}

# drop columns with # of zero > 30% size of nrow
dropcol<-c(colnames(data)[colSums(data==0) > nrow(data1)*0.2])



datawanted <- data[c(3,2,6,15,153,185,215)]

lmodel<-lm(R.D.Expenses~., data=datawanted)
summary(lmodel)



clusters <- kmeans(data[c(2,6)],3)
#clusters <- kmeans(data[2:219],3)
data$Group <- as.factor(clusters$cluster)

ggplot(data, aes(x=RD.IO.Ratio, y=PC1, col=Group)) +
  geom_point()

