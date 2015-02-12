#read in all the data. The data must be in the working 
#directory to be read in.

trainData=read.table("X_train.txt",header=F)
trainSubject=read.table("subject_train.txt",header=F)
trainActivity=read.table("y_train.txt",header=F)
features=read.table("features.txt",header=F)
testData=read.table("X_test.txt",header=F)
testSubject=read.table("subject_test.txt",header=F)
testActivity=read.table("y_test.txt",header=F)

#add the subject and activity labels to the training data
trainData$subject=trainSubject$V1
trainData$activity=trainActivity$V1

#process the features data so that it does not include "-"
# or "()" which will interfere with the column names
features$V2=gsub("-",".",features$V2)
features$V2=gsub("()","",features$V2,fixed=T)
# assign the features to the column(variable) names of the training data
names(trainData)[1:561]=as.character(features$V2)

#add the subject and activity labels to the test data
testData$subject=testSubject$V1
testData$activity=testActivity$V1
#assign the features to the variable names of the test data
names(testData)[1:561]=as.character(features$V2)
#merge training data and test data to achieve one data set
mData=rbind(trainData,testData)

#extract the mean and standard deviation from the merged data set
eindex=grep("mean|std",names(mData))
eData=mData[,c(eindex,562,563)]


library(plyr)
#change the activity column to factor so that we can use the revalue method
#(which can only be applied to factor). Revalue the level name of the activity 

eData$activity=as.factor(eData$activity)

eData$activity=revalue(eData$activity, c("1"="walking", "2"="walkingUp","3"="walkingDown","4"="sitting","5"="standing","6"="lying"))

#create a new column which combines the subject and activity 
#For example,1.sitting means subject 1 performing the sitting activity
#5.walking up means subject 5 performing the walking upstrairs activity
eData$subAct=paste(eData$subject,eData$activity,sep=".")

library(reshape2)
#melt the data first to get a long thin data then calculate the mean for each feature 
#according to the subAct variable
eDataM=melt(eData,id=c("subject","activity","subAct"),measure.vars=names(eData)[1:79])
subActData= dcast(eDataM,subAct~variable,mean)
# save the data set into a text file called meanForEachSubjectEachActivity.txt
write.table(subActData,"meanForEachSubjectEachActivity.txt",row.names=F)
