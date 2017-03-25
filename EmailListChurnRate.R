library(glmnet)
library(caret)
library(e1071)
library(pscl)
library(pROC)
library(DMwR)

library(ggplot)
library(corrplot)
library(ggmap)
library(maptools)
library(maps)

library(randomForest)

# Importing the csv file retrieved using the query from the mailchimp.sql file
library(readr)
MailchimpData <- read_csv("PATH_TO_CSV_FILE")

#Create the subscription variables. 1 for churned of cleaned, 0 else.
MailchimpData$subscription<-as.numeric(MailchimpData$status != 'subscribed')
sapply(MailchimpData,function(x) sum(is.na(x)))

#NA values in the aggregated fields is interpreted as 0.
MailchimpData$totalopens[is.na(MailchimpData$totalopens)]<-0
MailchimpData$totalclicks[is.na(MailchimpData$totalclicks)]<-0
MailchimpData$totalbounces[is.na(MailchimpData$totalbounces)]<-0

#keeping only columns with no NAs
sapply(MailchimpData,function(x) sum(is.na(x)))
MailchimpData<-MailchimpData[colSums(!is.na(MailchimpData)) >= dim(MailchimpData)[1]]
sapply(MailchimpData,function(x) sum(is.na(x)))

#converting blendo_imported_at to actual date and timestamps to the correct timezone
MailchimpData$blendo_imported_at<- MailchimpData$blendo_imported_at/1000
MailchimpData$blendo_imported_at<-as.POSIXct(MailchimpData$blendo_imported_at, origin="1970-01-01")
MailchimpData$last_changed<-MailchimpData$last_changed + MailchimpData$location_dstoff*60*60

#detect personal and business emails 
mailService<-sapply(strsplit(as.character(MailchimpData$email_address),'@',fixed=TRUE), `[`, 2)
MailchimpData$mailService<-sapply(strsplit(as.character(mailService),'.',fixed=TRUE), `[`, 1)
MailchimpData$personalMail<-0

mailProviders<-c("gmail", "zoho", "outlook", "yahoo", "gmx", "yandex", "hushmail", "aol")
for (i in 1:length(MailchimpData$mailService)) {
  if (MailchimpData$mailService[i] %in% mailProviders){
    MailchimpData$personalMail[i]<-1
    i=i+1
  }
}

#Five Number summary
summary(MailchimpData)

#world map construction
visit.x <- MailchimpData$location_longitude
visit.y <- MailchimpData$location_latitude
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") 
mp <- ggplot() +   mapWorld
mp <- mp+ geom_point(aes(x=visit.x, y=visit.y) ,color=(as.numeric(MailchimpData$status == "unsubscribed")+3), size=1.5) +
  xlab("Longitude") +
  ylab("Latitude") 
mp

#Histogram for mailing lists
list_sub <- data.frame(table(MailchimpData$list_id,MailchimpData$subscription))
names(list_sub) <- c("List_id","Subscription","Count")
ggplot(data=list_sub, aes(x=List_id, y=Count, fill=Subscription, alpha=0.1)) + geom_bar(stat="identity")

#Pairwise scatterplot for member rating
ggplot(MailchimpData, aes(x= subscription, y = member_rating))+
  geom_jitter(alpha=0.5, aes(color=subscription),position = position_jitter(width = 0.1))+coord_flip()

#casting integer fields into numeric for easier dataset splitting later
MailchimpData$location_dstoff<-as.numeric(MailchimpData$location_dstoff)
MailchimpData$location_gmtoff<-as.numeric(MailchimpData$location_gmtoff)
MailchimpData$member_rating<-as.numeric(MailchimpData$member_rating)
MailchimpData$vip<-as.numeric(MailchimpData$vip)

#define test and train dataset
smp_size <- floor(0.75 * nrow(MailchimpData))
set.seed(42)
train_ind <- sample(seq_len(nrow(MailchimpData)), size = smp_size)

train <- MailchimpData[train_ind, ]
test <- MailchimpData[-train_ind, ]
train.n<-sapply(train,class)=='numeric'
trainNum<-train[,train.n]

#correlation plot for numeric variables
corr<-cor(trainNum[,-8])
corrplot(corr, type = "lower", tl.pos = "ld")

#Modeling with random forest
#Random forest can handle only numeric variables and factors
train$list_id<-as.factor(train$list_id)
test$list_id<-as.factor(test$list_id)
train.n<-sapply(train,class)!='character'
trainF<-train[,train.n]

set.seed(42)
#model constructin
randomForestModel <- randomForest(as.factor(subscription) ~.,
                    data=trainF, 
                    importance=TRUE, 
                    ntree=2000)

#variable importance plot
varImpPlot(randomForestModel)

#making predictions
prediction <- predict(randomForestModel, test)

#evaluating the model
confMatrix<-table(prediction,test$subscription)
A<-confMatrix[4]
B<-confMatrix[2]
C<-confMatrix[3]
D<-confMatrix[1]

accuracy<- (A+D)/(A+B+C+D)
accuracy
specificity<-D/(B+D)
specificity
sensitivity<-A/(A+C)
sensitivity
precision<-A/(A+B)
precision
F1<-2*precision*sensitivity/(precision+sensitivity)
F1

#rerun random forest. This time only with the 5 most important variables
set.seed(42)

#constructint
randomForestModel <- randomForest(as.factor(subscription) ~last_changed+stats_avg_open_rate+
                      member_rating+totalopens+list_id,
                    data=trainF, 
                    importance=TRUE, proximity=TRUE,
                    ntree=2000)
varImpPlot(randomForestModel)

#predicting
prediction <- predict(randomForestModel, test)

#evaluating
confMatrix<-table(prediction,test$subscription)

D<-confMatrix[1]
C<-confMatrix[3]
A<-confMatrix[4]
B<-confMatrix[2]
accuracy<- (A+D)/(A+B+C+D)
accuracy
specificity<-D/(B+D)
specificity
sensitivity<-A/(A+C)
sensitivity
precision<-A/(A+B)
precision
F1<-2*precision*sensitivity/(precision+sensitivity)
F1
auc(test$subscription, prediction)

## The following code generates a visual representation of a classification tree ##

to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
  
  if(dfrep[rownum,'status'] == -1){
    rval <- list()
    
    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE
    
  }else{
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)
    
    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
  }
  
  class(rval) <- "dendrogram"
  
  return(rval)
}

tree <- getTree(randomForestModel,1,labelVar=TRUE)
d <- to.dendrogram(tree)
plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=.55,p.col=NA,p.lty=0, yaxt= "n"))
plot(d,center=TRUE,edgePar=list(t.cex=.55,p.col=NA,p.lty=0), yaxt = "n",digits = 2)