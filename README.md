# **run_analysis**

run_analysis is a tool that was developed as part of the JHU Getting and Cleaning Data Programme Assignment.
It pulls data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
The input data represent data collected from the Samsung Galaxy S smartphone's embedded accelerometer and gyroscope
during experiments that were carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)
wearing the smartphone (A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

The tool returns the mean and standard deviation of the body and gravity accelerations and gyroscope angular 
velocity collected as part of the eperiments and saved under the filename 'tidydataset' in a tabulated format. 

# **Pre-requisites**

The following paR packages are required for running of the run_analysis tool:
 	- data.table
  	- plyr
  	- dplyr
  	- tidyr
  	- tibble
  	- purrr
  	- stringr
  	- dataMaid

# **Execution of the tool**

The tool executes the following steps:


- **Load the library for all required packages**
  library(data.table)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(dataMaid)
  
- **Load and read all data necessary to the assignment**  
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%
20Dataset.zip"
download.file(fileUrl,destfile="UCIHARDataset.zip")
unzip("UCIHARDataset.zip")
file.rename("UCI HAR Dataset","UCIHARDataset")
xtrain <- fread("./UCIHARDataset/train/X_train.txt")
ytrain <- fread("./UCIHARDataset/train/Y_train.txt")
subjecttrain <- fread("./UCIHARDataset/train/subject_train.txt")
xtest <- fread("./UCIHARDataset/test/X_test.txt")
ytest <- fread("./UCIHARDataset/test/Y_test.txt")
subjecttest <- fread("./UCIHARDataset/test/subject_test.txt")
features <- fread("./UCIHARDataset/features.txt")
activitylabels <- fread("./UCIHARDataset/activity_labels.txt")


- **MERGE THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET**

- **Add uniqueid identifier to all datasets to relate them to each other before merging**
xtest <- cbind(c(1:length(xtest$V1)),xtest)
ytest <- cbind(c(1:length(ytest$V1)),ytest)
subjecttest <- cbind(c(1:length(subjecttest$V1)),subjecttest)
xtrain <- cbind(c(1:length(xtrain$V1)),xtrain)
ytrain <- cbind(c(1:length(ytrain$V1)),ytrain)
subjecttrain <- cbind(c(1:length(subjecttrain$V1)),subjecttrain)


- **APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES** 

- **Add labels to the various datasets**
names(xtest) <- c("uniqueid",t(features[,2]))
names(ytest) <- c("uniqueid","activityid")
names(xtrain) <- c("uniqueid",t(features[,2]))
names(ytrain) <- c("uniqueid","activityid")
names(subjecttest) <- c("uniqueid","subjectid")
names(subjecttrain) <- c("uniqueid","subjectid")

- **Rename duplicate columns in xtest and xtrain before merging**
xtest <- set_tidy_names(xtest)
xtrain <- set_tidy_names(xtrain)

- **Merges the various training and test datasets into xset (acceleration and angular velocity data),
	yset (activity type) and subjectset (identifier of the subject taking part) datasets**   
xset <- rbind(xtest,xtrain)
yset <- rbind(ytest,ytrain)
subjectset <- rbind(subjecttest,subjecttrain)

- **Merges the xset dataset with the yset and the subjectset dataset into a single dataset linking acceleration and angular
	velocity experimental data to the subjects and the activity**   
mergelist <- list(xset,yset,subjectset)
dataset <- join_all(mergelist)

- **USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET**

- **Add descriptive labels to the activitylabels dataset**
names(activitylabels) <- c("activityid","activityname")

- **Merges the test dataset with the activity labels dataset** 
dataset <- join(dataset,activitylabels)

- **EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD... 
	DEVIATION FOR EACH MEASUREMENT** 

interimdataset <- set_tidy_names(dataset)

interimdataset2 <- interimdataset %>% select("uniqueid","activityname","activityid","subjectid",
  grep("mean()",names(interimdataset),fixed=TRUE,value=TRUE),grep("std",names(interimdataset),
  value=TRUE)) %>% set_names(~ str_to_lower(.) %>% str_replace_all("tbodyacc", 
  "timebodyacceleration") %>% str_replace_all("tgravityacc","timegravityacceleration") 
  %>% str_replace_all("tbodygyro","timebodygyroscopeangularvelocity") %>% 
  str_replace_all("fbodyacc", "frequencybodyacceleration") %>% 
  str_replace_all("fbodygyro","frequencybodygyroscopeangularvelocity") %>% 
  str_replace_all("fbodybodyacc","frequencybodyacceleration") %>%
  str_replace_all("fbodybodygyro","frequencybodygyroscopeangularvelocity") %>%
  str_replace_all("std","standarddeviation") %>% 
  str_replace_all("mag","magnitude") %>% 
  str_replace_all("-","") %>%  
  str_replace_all("[()]","")  )


- **FROM THE DATA SET IN STEP 4, CREATES A SECOND, INDEPENDENT TIDY 
  	DATA SET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT**

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

- **SAVE FILE**
write.table(tidydataset,"tidydataset",sep="\t",row.names=FALSE)

- **ADD DESCRIPTION TO VARIABLES IN DATASET AND CREATE CODEBOOK**

attr(tidydataset$subjectid,"shortDescription") <- "Subject who carried out the experiment."
attr(tidydataset$activityname,"shortDescription") <- "One of six activities performed by the
  participants (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
  wearing a smartphone (Samsung Galaxy S II) on the waist."
attr(tidydataset$timebodyaccelerationmeanx,"shortDescription") <- "Normalised mean of the body acceleration
  recorded by the accelerometer in the time domain for the x-axis."
attr(tidydataset$timebodyaccelerationmeany,"shortDescription") <- "Normalised mean of the body acceleration
  recorded by the accelerometer in the time domain for the y-axis."
attr(tidydataset$timebodyaccelerationmeanz,"shortDescription") <- "Normalised mean of the body acceleration
  recorded by the accelerometer in the time domain for the z-axis."
attr(tidydataset$timegravityaccelerationmeanx,"shortDescription") <- "Normalised mean of the gravity
  acceleration recorded by the accelerometer in the time domain for the x-axis."
attr(tidydataset$timegravityaccelerationmeany,"shortDescription") <- "Normalised mean of the gravity
  acceleration recorded by the accelerometer in the time domain for the y-axis."
attr(tidydataset$timegravityaccelerationmeanz,"shortDescription") <- "Normalised mean of the gravity
  acceleration recorded by the accelerometer in the time domain for the z-axis."
attr(tidydataset$timebodyaccelerationjerkmeanx,"shortDescription") <- "Normalised mean of the jerk body
  acceleration recorded by the accelerometer in the time domain for the x-axis."
attr(tidydataset$timebodyaccelerationjerkmeany,"shortDescription") <- "Normalised mean of the jerk body
  acceleration recorded by the accelerometer in the time domain for the y-axis."
attr(tidydataset$timebodyaccelerationjerkmeanz,"shortDescription") <- "Normalised mean of the jerk body
  acceleration recorded by the accelerometer in the time domain for the z-axis."
attr(tidydataset$timebodygyroscopeangularvelocitymeanx,"shortDescription") <- "Normalised mean of the 
  body angular velocity recorded by the gyroscope in the time domain for the x-axis."
attr(tidydataset$timebodygyroscopeangularvelocitymeany,"shortDescription") <- "Normalised mean of the 
  body angular velocity recorded by the gyroscope in the time domain for the y-axis."
attr(tidydataset$timebodygyroscopeangularvelocitymeanz,"shortDescription") <- "Normalised mean of the 
  body angular velocity recorded by the gyroscope in the time domain for the z-axis."
attr(tidydataset$timebodygyroscopeangularvelocityjerkmeanx,"shortDescription") <- "Normalised mean of the 
  jerk body angular velocity recorded by the gyroscope in the time domain for the x-axis."
attr(tidydataset$timebodygyroscopeangularvelocityjerkmeany,"shortDescription") <- "Normalised mean of the 
  jerk body angular velocity recorded by the gyroscope in the time domain for the y-axis."
attr(tidydataset$timebodygyroscopeangularvelocityjerkmeanz,"shortDescription") <- "Normalised mean of the 
  jerk body angular velocity recorded by the gyroscope in the time domain for the z-axis."
attr(tidydataset$timebodyaccelerationmagnitudemean,"shortDescription") <- "Normalised mean of the 
  magnitude of the body acceleration recorded by the accelerometer in the time domain."
attr(tidydataset$timegravityaccelerationmagnitudemean,"shortDescription") <- "Normalised mean of the 
  magnitude of the gravity acceleration recorded by the accelerometer in the time domain."
attr(tidydataset$timebodyaccelerationjerkmagnitudemean,"shortDescription") <- "Normalised mean of the 
  jerk magnitude of the body acceleration recorded by the accelerometer in the time domain."
attr(tidydataset$timebodygyroscopeangularvelocitymagnitudemean,"shortDescription") <- "Normalised mean of the 
  magnitude of the body angular velocity recorded by the gyroscope in the time domain."
attr(tidydataset$timebodygyroscopeangularvelocityjerkmagnitudemean,"shortDescription") <- "Normalised mean of the 
  jerk magnitude of the body angular velocity recorded by the gyroscope in the time domain."
attr(tidydataset$frequencybodyaccelerationmeanx,"shortDescription") <- "Normalised mean of the body acceleration
  recorded by the accelerometer in the frequency domain for the x-axis."
attr(tidydataset$frequencybodyaccelerationmeany,"shortDescription") <- "Normalised mean of the body acceleration
  recorded by the accelerometer in the frequency domain for the y-axis."
attr(tidydataset$frequencybodyaccelerationmeanz,"shortDescription") <- "Normalised mean of the body acceleration
  recorded by the accelerometer in the frequency domain for the z-axis."
attr(tidydataset$frequencybodyaccelerationjerkmeanx,"shortDescription") <- "Normalised mean of the jerk body acceleration
  recorded by the accelerometer in the frequency domain for the x-axis."
attr(tidydataset$frequencybodyaccelerationjerkmeany,"shortDescription") <- "Normalised mean of the jerk body acceleration
recorded by the accelerometer in the frequency domain for the y-axis."
attr(tidydataset$frequencybodyaccelerationjerkmeanz,"shortDescription") <- "Normalised mean of the jerk body acceleration
recorded by the accelerometer in the frequency domain for the z-axis."
attr(tidydataset$frequencybodygyroscopeangularvelocitymeanx,"shortDescription") <- "Normalised mean of the 
  body angular velocity recorded by the gyroscope in the frequency domain for the x-axis."
attr(tidydataset$frequencybodygyroscopeangularvelocitymeany,"shortDescription") <- "Normalised mean of the 
  body angular velocity recorded by the gyroscope in the frequency domain for the y-axis."
attr(tidydataset$frequencybodygyroscopeangularvelocitymeanz,"shortDescription") <- "Normalised mean of the 
  body angular velocity recorded by the gyroscope in the frequency domain for the z-axis."
attr(tidydataset$frequencybodyaccelerationmagnitudemean,"shortDescription") <- "Normalised mean of the 
  magnitude of the body acceleration recorded by the accelerometer in the frequency domain."
attr(tidydataset$frequencybodyaccelerationjerkmagnitudemean,"shortDescription") <- "Normalised mean of the 
  jerk magnitude of the body acceleration recorded by the accelerometer in the frequency domain."
attr(tidydataset$frequencybodygyroscopeangularvelocitymagnitudemean,"shortDescription") <- "Normalised mean of the 
  magnitude of the body angular velocity recorded by the gyroscope in the frequency domain."
attr(tidydataset$frequencybodygyroscopeangularvelocityjerkmagnitudemean,"shortDescription") <- "Normalised mean of the 
  jerk magnitude of the body angular velocity recorded by the gyroscope in the frequency domain."

attr(tidydataset$timebodyaccelerationstandarddeviationx,"shortDescription") <- "Standard deviation of the body acceleration
  recorded by the accelerometer in the time domain for the x-axis."
attr(tidydataset$timebodyaccelerationstandarddeviationy,"shortDescription") <- "Standard deviation of the body acceleration
  recorded by the accelerometer in the time domain for the y-axis."
attr(tidydataset$timebodyaccelerationstandarddeviationz,"shortDescription") <- "Standard deviation of the body acceleration
  recorded by the accelerometer in the time domain for the z-axis."
attr(tidydataset$timegravityaccelerationstandarddeviationx,"shortDescription") <- "Standard deviation of the gravity
  acceleration recorded by the accelerometer in the time domain for the x-axis."
attr(tidydataset$timegravityaccelerationstandarddeviationy,"shortDescription") <- "Standard deviation of the gravity
  acceleration recorded by the accelerometer in the time domain for the y-axis."
attr(tidydataset$timegravityaccelerationstandarddeviationz,"shortDescription") <- "Standard deviation of the gravity
  acceleration recorded by the accelerometer in the time domain for the z-axis."
attr(tidydataset$timebodyaccelerationjerkstandarddeviationx,"shortDescription") <- "Standard deviation of the jerk body
  acceleration recorded by the accelerometer in the time domain for the x-axis."
attr(tidydataset$timebodyaccelerationjerkstandarddeviationy,"shortDescription") <- "Standard deviation of the jerk body
  acceleration recorded by the accelerometer in the time domain for the y-axis."
attr(tidydataset$timebodyaccelerationjerkstandarddeviationz,"shortDescription") <- "Standard deviation of the jerk body
  acceleration recorded by the accelerometer in the time domain for the z-axis."
attr(tidydataset$timebodygyroscopeangularvelocitystandarddeviationx,"shortDescription") <- "Standard deviation of the 
  body angular velocity recorded by the gyroscope in the time domain for the x-axis."
attr(tidydataset$timebodygyroscopeangularvelocitystandarddeviationy,"shortDescription") <- "Standard deviation of the 
  body angular velocity recorded by the gyroscope in the time domain for the y-axis."
attr(tidydataset$timebodygyroscopeangularvelocitystandarddeviationz,"shortDescription") <- "Standard deviation of the 
  body angular velocity recorded by the gyroscope in the time domain for the z-axis."
attr(tidydataset$timebodygyroscopeangularvelocityjerkstandarddeviationx,"shortDescription") <- "Standard deviation of the 
  jerk body angular velocity recorded by the gyroscope in the time domain for the x-axis."
attr(tidydataset$timebodygyroscopeangularvelocityjerkstandarddeviationy,"shortDescription") <- "Standard deviation of the 
  jerk body angular velocity recorded by the gyroscope in the time domain for the y-axis."
attr(tidydataset$timebodygyroscopeangularvelocityjerkstandarddeviationz,"shortDescription") <- "Standard deviation of the 
  jerk body angular velocity recorded by the gyroscope in the time domain for the z-axis."
attr(tidydataset$timebodyaccelerationmagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  magnitude of the body acceleration recorded by the accelerometer in the time domain."
attr(tidydataset$timegravityaccelerationmagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  magnitude of the gravity acceleration recorded by the accelerometer in the time domain."
attr(tidydataset$timebodyaccelerationjerkmagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  jerk magnitude of the body acceleration recorded by the accelerometer in the time domain."
attr(tidydataset$timebodygyroscopeangularvelocitymagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  magnitude of the body angular velocity recorded by the gyroscope in the time domain."
attr(tidydataset$timebodygyroscopeangularvelocityjerkmagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  jerk magnitude of the body angular velocity recorded by the gyroscope in the time domain."
attr(tidydataset$frequencybodyaccelerationstandarddeviationx,"shortDescription") <- "Standard deviation of the body acceleration
  recorded by the accelerometer in the frequency domain for the x-axis."
attr(tidydataset$frequencybodyaccelerationstandarddeviationy,"shortDescription") <- "Standard deviation of the body acceleration
  recorded by the accelerometer in the frequency domain for the y-axis."
attr(tidydataset$frequencybodyaccelerationstandarddeviationz,"shortDescription") <- "Standard deviation of the body acceleration
  recorded by the accelerometer in the frequency domain for the z-axis."
attr(tidydataset$frequencybodyaccelerationjerkstandarddeviationx,"shortDescription") <- "Standard deviation of the jerk body acceleration
  recorded by the accelerometer in the frequency domain for the x-axis."
attr(tidydataset$frequencybodyaccelerationjerkstandarddeviationy,"shortDescription") <- "Standard deviation of the jerk body acceleration
  recorded by the accelerometer in the frequency domain for the y-axis."
attr(tidydataset$frequencybodyaccelerationjerkstandarddeviationz,"shortDescription") <- "Standard deviation of the jerk body acceleration
  recorded by the accelerometer in the frequency domain for the z-axis."
attr(tidydataset$frequencybodygyroscopeangularvelocitystandarddeviationx,"shortDescription") <- "Standard deviation of the 
  body angular velocity recorded by the gyroscope in the frequency domain for the x-axis."
attr(tidydataset$frequencybodygyroscopeangularvelocitystandarddeviationy,"shortDescription") <- "Standard deviation of the 
  body angular velocity recorded by the gyroscope in the frequency domain for the y-axis."
attr(tidydataset$frequencybodygyroscopeangularvelocitystandarddeviationz,"shortDescription") <- "Standard deviation of the 
  body angular velocity recorded by the gyroscope in the frequency domain for the z-axis."
attr(tidydataset$frequencybodyaccelerationmagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  magnitude of the body acceleration recorded by the accelerometer in the frequency domain."
attr(tidydataset$frequencybodyaccelerationjerkmagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  jerk magnitude of the body acceleration recorded by the accelerometer in the frequency domain."
attr(tidydataset$frequencybodygyroscopeangularvelocitymagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  magnitude of the body angular velocity recorded by the gyroscope in the frequency domain."
attr(tidydataset$frequencybodygyroscopeangularvelocityjerkmagnitudestandarddeviation,"shortDescription") <- "Standard deviation of the 
  jerk magnitude of the body angular velocity recorded by the gyroscope in the frequency domain."

makeCodebook(tidydataset,replace=TRUE)

# **Tests**
Performed prior to release on 26-Nov-2018

# **Author: Nathalie Descusse-Brown**

# **Version:** 
	1.0: 26-Nov-2018

# **Contributing**
Pull requests are welcome, For major changes, please open an issue first to discuss what you would like to change.
Please make sure to perform tests as appropriate.

# License
[MIT]
(https://choosealicense.com/licenses/mit)