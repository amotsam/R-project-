rm(list = ls())

#1. install and Load the rpart & rpart.plot packages
#install.packages("rpart")
#install.packages("rpart.plot")

# 2. load the packages rpart , rpart.plot

library(rpart) #tree models
library(rpart.plot) #plot decision tree

#3. Read the Datafile
MyData <- read.csv("treecredit.csv")
summary(MyData)

##############################################################################
#4. knowing your data
# 4.1 Plot Credit risk Distribution
tabCR<-table(MyData$CreditRating)
PtabCR<-prop.table(tabCR)*100
bpCR<- barplot(PtabCR,main = "Credit risk Distribution",
               xlab = "Credit risk",col = "orange")
text(bpCR, PtabCR, round(PtabCR, 1),cex=1.2,pos=3, xpd=NA)

# 4.2 plot a Boxplot of Age  by CreditRating status
boxplot (Age~CreditRating, data = MyData,
         main = "Age by CreditRating",
         xlab = "CreditRating",
         ylab = "Age",las=1,
         col = "orange")

#4.3 barplot of Income distribution by CreditRating status
IncomebyCreditRating<-table(MyData$Income,MyData$CreditRating)
bpIbyCRprop<-prop.table(IncomebyCreditRating,2)*100
cols<-c("lightblue","red" ,"green")
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
bpIbyCR<-barplot(bpIbyCRprop,legend.text=F ,col=cols, xlab="CreditRating", 
                 main="Income levels by Credit Rating",ylim = c(0,100), 
                 ylab="percent")
legend("topright",inset=c(-0.3,0), legend=rownames(IncomebyCreditRating),fill = cols)


# 4.4 barplot of CreditCards distribution by CreditRating status
CreditCardsbyCreditRating<-table(MyData$CreditCards,MyData$CreditRating)
bpIbyCRprop<-prop.table(CreditCardsbyCreditRating,2)*100
cols<-c("lightblue","red")
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
bpIbyCR<-barplot(bpIbyCRprop,legend.text=F ,col=cols, xlab="CreditRating", 
                 main="CreditCards levels by Credit Rating",ylim = c(0,100), 
                 ylab="percent" , cex.axis = 1.5, las=1,
                 cex.names = 1.5)
legend("topright",inset=c(-0.3,0), legend=rownames(CreditCardsbyCreditRating),fill = cols)


# 4.5 barplot of Carloans distribution by CreditRating status
CarloansbyCreditRating<-table(MyData$Carloans,MyData$CreditRating)
bpIbyCRprop<-prop.table(CarloansbyCreditRating,2)*100
cols<-c("lightblue","red")
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
bpIbyCR<-barplot(bpIbyCRprop,legend.text=F ,col=cols, xlab="CreditRating", 
                 main="Carloans levels by Credit Rating",ylim = c(0,100), 
                 ylab="percent" , cex.axis = 1, las=1,
                 cex.names = 1,)
legend("topright",inset=c(-0.3,0), legend=rownames(CarloansbyCreditRating),fill = cols, cex = 0.8)

# 4.6 barplot of Education distribution by CreditRating status
EducationbyCreditRating<-table(MyData$Education,MyData$CreditRating)
bpIbyCRprop<-prop.table(EducationbyCreditRating,2)*100
cols<-c("lightblue","red")
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
bpIbyCR<-barplot(bpIbyCRprop,legend.text=F ,col=cols, xlab="CreditRating", 
                 main="Education levels by Credit Rating",ylim = c(0,100), 
                 ylab="percent" , cex.axis = 1.5, las=1,
                 cex.names = 1.5,)
legend("topright",inset=c(-0.3,0), legend=rownames(EducationbyCreditRating),fill = cols, cex =0.8)
##########################################################################################



# 5. Building the tree model
credit.rp <- rpart(CreditRating ~ ., data=MyData)
credit.rp # Display the tree node details
# n=sample size 
#loss= misclassification cost
#yval= classified membership (Good or Bad in this case) 
#yprob=probabilities of two classes: 
# the left value refers to the prob of label "no", 
# and the right value refers to prob of label "yes"

# another way:
summary(credit.rp)
# plot the tree via rpart.plot function
rpart.plot(credit.rp, type = 2,extra=103,cex=0.8, box.palette="GnBu",nn=TRUE)
# or (extra=103 shows frequencies and extra=104 shows precents)
rpart.plot(credit.rp, type = 2,extra=104,cex=0.8, box.palette="GnBu",nn=TRUE)

#############################################
# "fancy tree"
library(party) #plot dtree
library(partykit) #plot dtree
rparty.tree <- as.party(credit.rp)
rparty.tree
plot(rparty.tree)
#############################################



# 6. Model Accuracy
# Prediction based on the model we've just built
# Type = "class",  returns classification
# x is a vector that holds a prediction for each case in our data

x <- predict(credit.rp, MyData, type = "class")
table(x)
Predict <- as.data.frame(cbind(x, MyData$CreditRating))
names(Predict) <- c("Prediction", "CreditRating")
Predict$Prediction <- ifelse(Predict$Prediction == 1, "Bad", "Good")
Predict$CreditRating <- ifelse(Predict$CreditRating == 1, "Bad", "Good")
head(Predict)

# confusion matrix

cmat <- table(truth=Predict$CreditRating, pred=Predict$Prediction)
cmat

cmat/margin.table(cmat)

prop.table(cmat, margin = 1)

# Calculate the accuracy and error rate
accuracy <- (cmat[1,1] + cmat[2,2])/sum(cmat)
accuracy

ErrorRate<-1-accuracy
ErrorRate

# or:
ErrorRate<-(cmat[2,1] + cmat[1,2])/sum(cmat)

