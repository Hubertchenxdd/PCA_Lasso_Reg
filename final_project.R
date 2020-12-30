library(tidyverse)

data<-read.csv("ProcessedDataset.csv", header=TRUE)
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
data<-subset(data,select=-c(operatingProfitMargin))
sector<-subset(data,select=-c(X.1))

# convert continuous variable to numeric
data[2:220]<-data.frame(data.matrix(data[2:220]))
data[222]<-data.frame(data.matrix(data[222]))

# check data
# describe(data)
# nrow(data)
# ncol(data)

# replace na in all columns with their own mean
for (i in 2:222) {
  if (i != 221) {
    data[,i][is.na(data[,i])]<-mean(data[,i], na.rm=TRUE)
  }
}

# standardization
# data[4:ncol(data)]<-scale(data[4:ncol(data)],center=TRUE,scale=TRUE)
# data<-data%>%mutate_at(c(4,ncol(data)),funs(c(scale(.))))

# standardize variables beside Revenue.Growth(dependent variable)

data[2]<-scale(data[2],center=TRUE,scale=TRUE)
for (i in 4:222) {
  if (i != 221) {
    data[i]<-scale(data[i],center=TRUE,scale=TRUE)
  }
}

# data1->coutinuous variables
data1<-subset(data,select=-c(X,Sector,Class,Revenue.Group))

# drop columns with # of zero > 30% size of nrow
dropcol<-c(colnames(data1)[colSums(data1==0) > nrow(data1)*0.3])
# data1<-subset(data1,select=-c(R.D.Expenses,Net.Income...Non.Controlling.int,Net.Income...Discontinued.ops,Preferred.Dividends))
# data1<-subset(data1,select=-c(Short.term.investments,Deferred.revenue,Deposit.Liabilities,R.D.to.Revenue,Dividends.per.Share.Growth,R.D.Expense.Growth))
# data1<-subset(data1,select=-c(priceEarningsToGrowthRatio,niperEBT,effectiveTaxRate,
#                              nIperEBT,shortTermCoverageRatios,X10Y.Revenue.Growth..per.Share.,
#                              X10Y.Operating.CF.Growth..per.Share.,X10Y.Net.Income.Growth..per.Share.,
#                              X10Y.Shareholders.Equity.Growth..per.Share.,X10Y.Dividend.per.Share.Growth..per.Share.))

# remove duplicated in data1
# data1<-data1[,duplicated(cor(data1))]


# check columns with missing values
colnames(data)[colSums(is.na(data)) > 0]
colnames(data1)[colSums(is.na(data1)) > 0]

# Factor Analysis ======================================================================
corr = round(cor(data1),2)
upper<-corr
upper[upper.tri(corr)]<-""
upper<-as.data.frame(upper)

# solve singular matrix
findDepMat(corr)

library(matrixcalc)
is.singular.matrix(corr)

ev<-eigen(cor(data1))
v<-ev$vectors
part.fa<-ev$values/sum(ev$values)*100

# Scree Plot
Factor=c(1:ncol(data1))
Eigen_Values<-ev$values
Scree<-data.frame(Factor,Eigen_Values)
plot(Scree,main="Scree Plot",col="Blue",ylim=c(0,6), xlim=c(0,50))
lines(Scree,col="Red")
abline(h=1,col="Green")


###########
# choose 12 factors based on scree plot
###########
###########
# Singular matrix...
###########
nfactors<-12
fit<-factanal(covmat=corr,factors=nfactors,rotation="none") #uses maximum likelihood estimation
library(psych)
fa_fit<-fa(r=data1,nfactors=nfactors,rotate='none',fm='pa')
print(fit)


# PCA ===================================================================================
library(factoextra)

y=data1[c(2)]
data1<-subset(data1,select= -c(Revenue.Growth))
data1_pca<-prcomp(data1,scale=TRUE)
print(data1_pca)
summary(data1_pca)

fviz_eig(data1_pca)

#examine the first principal component
a12<-data1_pca$rotation[,c(1:2)]
a12

#We can use the equation for the first principal component to compute the score for each competitor

hm<-as.matrix(data1)
drop(scale(hm,center= data1_pca$center,scale= data1_pca$scale) %*% data1_pca$rotation[,1])

#the following statement is identical to above
A<-predict(data1_pca)
A<-cbind(y,A)

lmodel<-lm(Revenue.Growth~., data=A[1:70])
summary(lmodel)


round(A,3)
par(mar=c(2.5,4.1,3.5,2.1) ) #bottom, left, top and right margins
plot(data1_pca,main="Scree plot")



################

sector<-data[c(224)]
all<-cbind(A,sector)
basicmaterial<-filter(all,sector=="Basic Materials")
lmodel<-lm(Revenue.Growth~., data=basicmaterial[1:70])
summary(lmodel)


basicmaterial<-filter(data,sector=="Basic Materials")

basicmaterial<-subset(basicmaterial,select=-c(X.1))
basicmaterial<-subset(basicmaterial,select=-c(cashConversionCycle))
basicmaterial<-subset(basicmaterial,select=-c(operatingCycle))
basicmaterial<-subset(basicmaterial,select=-c(operatingProfitMargin))


# convert continuous variable to numeric
basicmaterial[2:220]<-data.frame(data.matrix(basicmaterial[2:220]))
basicmaterial[222]<-data.frame(data.matrix(basicmaterial[222]))

# check data
# describe(data)
# nrow(data)
# ncol(data)

# replace na in all columns with their own mean
for (i in 2:222) {
  if (i != 221) {
    basicmaterial[,i][is.na(basicmaterial[,i])]<-mean(basicmaterial[,i], na.rm=TRUE)
  }
}

# standardization
# data[4:ncol(data)]<-scale(data[4:ncol(data)],center=TRUE,scale=TRUE)
# data<-data%>%mutate_at(c(4,ncol(data)),funs(c(scale(.))))

# standardize variables beside Revenue.Growth(dependent variable)

basicmaterial[2]<-scale(basicmaterial[2],center=TRUE,scale=TRUE)
for (i in 4:222) {
  if (i != 221) {
    basicmaterial[i]<-scale(basicmaterial[i],center=TRUE,scale=TRUE)
  }
}

# data1->coutinuous variables
basicmaterial<-subset(basicmaterial,select=-c(X,Sector,Class,Revenue.Group))

library(factoextra)

y=basicmaterial[c(2)]
basicmaterial<-subset(basicmaterial,select= -c(Revenue.Growth))
basicmaterial_pca<-prcomp(basicmaterial,scale=TRUE)
print(basicmaterial_pca)
summary(basicmaterial_pca)

fviz_eig(basicmaterial_pca)



#We can use the equation for the first principal component to compute the score for each competitor

hm<-as.matrix(data1)
drop(scale(hm,center= data1_pca$center,scale= data1_pca$scale) %*% data1_pca$rotation[,1])

#the following statement is identical to above
A<-predict(basicmaterial_pca)
A<-cbind(y,A)

lmodel<-lm(Revenue.Growth~., data=A[1:70])
summary(lmodel)





