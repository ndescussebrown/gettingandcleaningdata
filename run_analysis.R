run_analysis <- function() { ## 
  library(data.table)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  
## load all data necessary to the assignment  
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%
20Dataset.zip"
download.file(fileUrl,destfile="UCIHARDataset.zip")
unzip("UCIHARDataset.zip")
file.rename("UCI HAR Dataset","UCIHARDataset")
##train <- read.delim("./UCIHARDataset/train/X_train", header = TRUE, sep = "\t", dec = ".")
xtrain <- fread("./UCIHARDataset/train/X_train.txt")
ytrain <- fread("./UCIHARDataset/train/Y_train.txt")
subjecttrain <- fread("./UCIHARDataset/train/subject_train.txt")
xtest <- fread("./UCIHARDataset/test/X_test.txt")
ytest <- fread("./UCIHARDataset/test/Y_test.txt")
subjecttest <- fread("./UCIHARDataset/test/subject_test.txt")
features <- fread("./UCIHARDataset/features.txt")
activitylabels <- fread("./UCIHARDataset/activity_labels.txt")


## ASSIGNMENT REQUIREMENT 1. MERGED THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET

#add uniqueid identifier to all datasets
xtest <- cbind(c(1:length(xtest$V1)),xtest)
ytest <- cbind(c(1:length(ytest$V1)),ytest)
subjecttest <- cbind(c(1:length(subjecttest$V1)),subjecttest)
xtrain <- cbind(c(1:length(xtrain$V1)),xtrain)
ytrain <- cbind(c(1:length(ytrain$V1)),ytrain)
subjecttrain <- cbind(c(1:length(subjecttrain$V1)),subjecttrain)


## ASSIGNMENT REQUIREMENT 4. APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES 

##add labels to the various datasets
names(xtest) <- c("uniqueid",t(features[,2]))
names(ytest) <- c("uniqueid","activityid")
names(xtrain) <- c("uniqueid",t(features[,2]))
names(ytrain) <- c("uniqueid","activityid")
names(subjecttest) <- c("uniqueid","subjectid")
names(subjecttrain) <- c("uniqueid","subjectid")

##rename duplicate columns in xtest and xtrain before merging
xtest <- set_tidy_names(xtest)
xtrain <- set_tidy_names(xtrain)

##merges the training and test datasets
##xset <- merge(xtest,xtrain)
xset <- rbind(xtest,xtrain)
yset <- rbind(ytest,ytrain)
subjectset <- rbind(subjecttest,subjecttrain)


#merges the xtest dataset with the ytest and the subjecttest dataset    
mergelist <- list(xset,yset,subjectset)
dataset <- join_all(mergelist)

## ASSIGNMENT REQUIREMENT 3. USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN...
## THE DATA SET

##add descriptive labels to the activitylabels dataset
names(activitylabels) <- c("activityid","activityname")

#merges the test dataset with the activity labels dataset 
dataset <- join(dataset,activitylabels)

## ASSIGNMENT REQUIREMENT 2. EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD... 
## DEVIATION FOR EACH MEASUREMENT 

interimdataset <- set_tidy_names(dataset)

interimdataset2 <- interimdataset %>% select("uniqueid","activityname","activityid","subjectid",
  grep("mean()",names(interimdataset),fixed=TRUE,value=TRUE),grep("std",names(interimdataset),
  value=TRUE)) %>% set_names(~ str_to_lower(.) %>% str_replace_all("tbodyacc", 
  "timebodyacceleration") %>% str_replace_all("tgravityacc","timegravityacceleration") 
  %>% str_replace_all("tbodygyro","timebodygyroscopeangularvelocity") %>% 
  str_replace_all("fbodyacc", "frequencybodyacceleration") %>% 
  str_replace_all("fbodygyro","frequencybodygyroscopeangularvelocity") %>% 
  str_replace_all("fbodybody","frequencybody") %>% 
  str_replace_all("std","standarddeviation") %>% 
  str_replace_all("-","") %>%  
  str_replace_all("[()]","")  )

##colnames(interimdataset2) <- gsub("[()]", "", colnames(interimdataset2))

              

## ASSIGNMENT REQUIREMENT 5. From the data set in step 4, creates a second, independent tidy 
## data set with the average of each variable for each activity and each subject.

tidydataset <- data.frame()
 for (i in 1:30) {
  for (j in 1:6) { 
    calcdf <- filter(interimdataset2,subjectid==i & activityid==j) 
    if(dim(calcdf)[1]>0) {
      tidydataset <- rbind(tidydataset,as.numeric(c(calcdf$subjectid[1],calcdf$activityid[1],
        sapply(calcdf[5:length(calcdf)], mean))))
      names(tidydataset) <- c("subjectid","activityid",names(calcdf[5:length(calcdf)]))
    }
    }
 }
tidydataset <- merge(activitylabels,tidydataset)
tidydataset <- select(tidydataset,-activityid)
##save file
write.table(tidydataset,"tidydataset",sep="\t",row.names=FALSE)

}