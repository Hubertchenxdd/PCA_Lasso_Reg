library(tidyverse)

############################Data Process################################

data <- read.csv('/Users/hubertchen/Desktop/UW/2020 Winter/CET 521 Inferential Data Analysis For Engineers/Final project/ProcessedDataset.csv')
data$RD.OI.Ratio <- data$R.D.Expenses/data$Operating.Income

# remove unused columns (primary nas)
data<-subset(data,select=-c(X.1,cashConversionCycle,operatingCycle,operatingProfitMargin))


categoryvar <- data[c(220,222,223)]
data<-subset(data, select=-c(Sector,Class,Revenue.Group))

#fill mean into nas
for (i in 2:221) {
  if (i != 221) {
    data[,i][is.na(data[,i])]<-mean(data[,i], na.rm=TRUE)
  }
}

data<-cbind(data,categoryvar)
data<-na.omit(data)
write.csv(data, "/Users/hubertchen/Desktop/UW/2020 Winter/CET 521 Inferential Data Analysis For Engineers/Final project/FinalDataset.csv", row.names = FALSE)



############################PCA################################

library(factoextra)
depvar <- data[221]
data<-subset(data, select=-c(X,R.D.Expenses,Operating.Income,Sector,Class,Revenue.Group))

all_pca<-prcomp(data,scale=TRUE)
print(all_pca)
summary(all_pca)

plot(all_pca,main="Scree plot")

############################Linear with PCA matrix################################
indepvar<-predict(all_pca)
lmdata<-cbind(depvar,indepvar)

#use 69 pricipal components to explain 70% of the variance
#but looking at the elbow, use 5 principal components
lmodel4<-lm(RD.OI.Ratio~., data=lmdata[1:5])
summary(lmodel4)

lmodel5<-lm(RD.OI.Ratio~., data=lmdata[1:6])
summary(lmodel5)

lmodel69<-lm(RD.OI.Ratio~., data=lmdata[1:70])
summary(lmodel69)

confint(lmodel5)
par(mfrow=c(2,2))
plot(lmodel5)



ggplot(depvar, aes(x="",y = RD.OI.Ratio))+
  ggtitle("R&D Expenses over Operating Income Distribution") +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5))

Rev.vs.Ratio <- lm(RD.OI.Ratio ~ Revenue, data = data)
summary(Rev.vs.Ratio)

ggplot(data, aes(x=Revenue, y=RD.OI.Ratio)) + 
  geom_point()+
  geom_smooth(method=lm) +
  labs(title="Relation between Revenue and the DV", x ="Revenue", y = "DV") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(x=250000, y=500000, label="y = 2218 + 0.003852x")



summary(depvar)

