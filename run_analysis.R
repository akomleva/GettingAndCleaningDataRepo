library(dplyr)

#read all the data
xTestTable <- read.table("test/X_test.txt")
yTestTable <- read.table("test/y_test.txt")
subjTestTable <- read.table("test/subject_test.txt")

xTrainTable <- read.table("train/X_train.txt")
yTrainTable <- read.table("train/y_train.txt")
subjTrainTable <- read.table("train/subject_train.txt")

features <- read.table("features.txt", stringsAsFactors = F)

activities <- read.table("activity_labels.txt", stringsAsFactors = F)

#join test tables
xTestTable <- mutate(xTestTable, activity = yTestTable$V1) 
xTestTable <- mutate(xTestTable, subject = subjTestTable$V1)

#join train tables
xTrainTable <- mutate(xTrainTable, activity = yTrainTable$V1)
xTrainTable <- mutate(xTrainTable, subject = subjTrainTable$V1)

#merge test and train tables
resultTable <- rbind(xTestTable, xTrainTable)

#select mean and std columnt
grepIndex <- grep("-mean\\(|-std\\(", features$V2)

#add last 2 columns: subject and activities to selection
grepIndexWith2Cols <- c(grepIndex,562,563)

#select just mean amd std columns in the result set
resultTable <- resultTable[, grepIndexWith2Cols]

#name the activities
activityNameByIndex <- function(x){x = activities[x, 2]}
resultTable$activity <- sapply(resultTable$activity, activityNameByIndex)

#label columns
featuresLabels <- features$V2[grepIndex]
featuresLabelsSplitted <- strsplit(featuresLabels, "\\-")
goodNames <- function(x){
        substr(x[2], 1, 1) <- toupper(substr(x[2], 1, 1)) 
        x[2] <- gsub("\\(|\\)", "", x[2])
        paste(x, collapse="")
        }
featuresLabels <- sapply(featuresLabelsSplitted, goodNames)     
featuresLabels <- c(featuresLabels, "activity", "subject")
colnames(resultTable) <- featuresLabels

#well, one data set is ready
#now we'll make data set with the average of each variable for each activity and each subject
groupedTable <- group_by(resultTable, subject, activity)
tidyTable <- summarize_all(groupedTable, funs(mean))
tidyTable