#import data
data <- read.csv("breast-cancer-wisconsin.txt", header=F, sep = ",", dec = ".")
data

#turn matrix into integers
for (i in 1:10){
  data[,i]<-strtoi(data[,i])
}
data$V11<-data$V11/2-1
data

#count NAs in each column
countna<-c(0)
for (i in 1:10){
  temp<-sum(is.na(data[,i]))      
  countna[i]<-temp
}
countna

#get the mean of the column with NAs
col7nas<-data[is.na(data[,7]),]
col7nas
gooddata<-data[!is.na(data[,7]),7]
gooddata
gooddata2<-data[!is.na(data[,7]),]
col7mean<-mean(gooddata)
col7mean

#replace the NAs in col 7 with the mean (rounded to a whole number to match the rest of the data)
data[is.na(data[,7]),7]<-4
data



#create a regression to predict missing values
#create training and test set
gooddata2
nrow(gooddata2)

sampleind<-sample(nrow(gooddata2), size=478, replace = FALSE)
training<-gooddata2[sampleind,]
testset<-gooddata2[-sampleind,]

#build a regression to estimate V7 for the missing values
test<-lm(V7~.,data=training)
test
summary(test)

#remove non significant factors and reassess
test<-lm(V7~V5+V11,data=training)
summary(test)

#run model on test set
test<-lm(V7~V5+V11,data=testset)
summary(test)

#Use the model to make predictions
col7pred<-c(0)
col7nas<-col7nas[,-7]
col7nas
tempval<-predict.lm(test,col7nas[3,])
tempval
for (i in 1:nrow(col7nas)){
  tempval<-predict.lm(test,col7nas[i,])
  col7pred[i]<-tempval
}
col7pred

#we could replace nas in the original data set with this regression predictions, but first were going to add perturbations 
gdsd<-sd(gooddata)
col7mean
pert<-rnorm(16, col7mean, gdsd)

final<-col7pred+pert
final

