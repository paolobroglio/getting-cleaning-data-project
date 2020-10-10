library(tidyr)
library(dplyr)
library(stringr)

if (file.exists("Dataset.zip") | dir.exists("dataset")){
  unlink("dataset", recursive = TRUE)
  file.remove("Dataset.zip")
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "Dataset.zip")
  print("Dataset downloaded")
  unzip("Dataset.zip",exdir = ".")
  file.rename("UCI HAR Dataset", "dataset")
  print("Dataset extracted")
} 

gen_col_by_cell<-function(x){
  y=c()
  act_name_value=""
  for(act in x){
    act_name_value<-toString(unique(activity_labels[ which(activity_labels$act_label == act), 
                                                     "act_name"]))
    y<-c(y,act_name_value)
  }
  return(y)
}

train_flag="1"
test_flag="0"

print("Begin reading")

activity_labels <- read.table("dataset/activity_labels.txt")
features <- read.table("dataset/features.txt")

xtrain_data <- read.table("dataset/train/X_train.txt",col.names=features$V2)
subject_train<- read.table("dataset/train/subject_train.txt",col.names = c("subject"))
y_train <- read.table("dataset/train/y_train.txt",col.names = c("activity_label"))
##Read body_acc_x_train.txt into Data frame:body_acc_x_train, and add new column:train_test as identifier
body_acc_x_train<- read.table("dataset/train/Inertial Signals/body_acc_x_train.txt")
body_acc_x_train$train_test <- rep(train_flag,nrow(body_acc_x_train))
##Read body_acc_y_train.txt into Data frame:body_acc_y_train, and add new column:train_test as identifier
body_acc_y_train<- read.table("dataset/train/Inertial Signals/body_acc_y_train.txt")
body_acc_y_train$train_test <- rep(train_flag,nrow(body_acc_y_train))
##Read body_acc_z_train.txt into Data frame:body_acc_z_train, and add new column:train_test as identifier
body_acc_z_train<- read.table("dataset/train/Inertial Signals/body_acc_z_train.txt")
body_acc_z_train$train_test <- rep(train_flag,nrow(body_acc_z_train))

##Read body_gyro_x_train.txt into Data frame:body_gyro_x_train, and add new column:train_test as identifier
body_gyro_x_train<- read.table("dataset/train/Inertial Signals/body_gyro_x_train.txt")
body_gyro_x_train$train_test <- rep(train_flag,nrow(body_gyro_x_train))
##Read body_gyro_y_train.txt into Data frame:body_gyro_y_train, and add new column:train_test as identifier
body_gyro_y_train<- read.table("dataset/train/Inertial Signals/body_gyro_y_train.txt")
body_gyro_y_train$train_test <- rep(train_flag,nrow(body_gyro_y_train))
##Read body_gyro_z_train.txt into Data frame:body_gyro_z_train, and add new column:train_test as identifier
body_gyro_z_train<- read.table("dataset/train/Inertial Signals/body_gyro_z_train.txt")
body_gyro_z_train$train_test <- rep(train_flag,nrow(body_gyro_z_train))

##Read total_acc_x_train.txt into Data frame:total_acc_x_train, and add new column:train_test as identifier
total_acc_x_train<- read.table("dataset/train/Inertial Signals/total_acc_x_train.txt")
total_acc_x_train$train_test <- rep(train_flag,nrow(total_acc_x_train))
##Read total_acc_y_train.txt into Data frame:total_acc_y_train, and add new column:train_test as identifier
total_acc_y_train<- read.table("dataset/train/Inertial Signals/total_acc_y_train.txt")
total_acc_y_train$train_test <- rep(train_flag,nrow(total_acc_y_train))
##Read total_acc_z_train.txt into Data frame:total_acc_z_train, and add new column:train_test as identifier
total_acc_z_train<- read.table("dataset/train/Inertial Signals/total_acc_z_train.txt")
total_acc_z_train$train_test <- rep(train_flag,nrow(total_acc_z_train))


##Read X_test.txt into Data frame:xtest_data,and use vector data in 2nd column of feature as columns' name
xtest_data <- read.table("dataset/test/X_test.txt",col.names=features$V2)

##Read subject_test.txt.txt into Data frame:subject_test
subject_test<- read.table("dataset/test/subject_test.txt",col.names = c("subject"))

##Read y_test.txt.txt into Data frame:y_test
y_test<- read.table("dataset/test/y_test.txt",col.names = c("activity_label"))

##Read body_acc_x_test.txt into Data frame:body_acc_x_test, and add new column:train_test as identifier
body_acc_x_test<- read.table("dataset/test/Inertial Signals/body_acc_x_test.txt")
body_acc_x_test$train_test <- rep(test_flag,nrow(body_acc_x_test))
##Read body_acc_y_test.txt into Data frame:body_acc_y_test, and add new column:train_test as identifier
body_acc_y_test<- read.table("dataset/test/Inertial Signals/body_acc_y_test.txt")
body_acc_y_test$train_test <- rep(test_flag,nrow(body_acc_y_test))
##Read body_acc_z_test.txt into Data frame:body_acc_z_test, and add new column:train_test as identifier
body_acc_z_test<- read.table("dataset/test/Inertial Signals/body_acc_z_test.txt")
body_acc_z_test$train_test <- rep(test_flag,nrow(body_acc_z_test))

##Read body_gyro_x_test.txt into Data frame:body_gyro_x_test, and add new column:train_test as identifier
body_gyro_x_test<- read.table("dataset/test/Inertial Signals/body_gyro_x_test.txt")
body_gyro_x_test$train_test <- rep(test_flag,nrow(body_gyro_x_test))
##Read body_gyro_y_test.txt into Data frame:body_gyro_y_test, and add new column:train_test as identifier
body_gyro_y_test<- read.table("dataset/test/Inertial Signals/body_gyro_y_test.txt")
body_gyro_y_test$train_test <- rep(test_flag,nrow(body_gyro_y_test))
##Read body_gyro_z_test.txt into Data frame:body_gyro_z_test, and add new column:train_test as identifier
body_gyro_z_test<- read.table("dataset/test/Inertial Signals/body_gyro_z_test.txt")
body_gyro_z_test$train_test <- rep(test_flag,nrow(body_gyro_z_test))

##Read total_acc_x_test.txt into Data frame:total_acc_x_test, and add new column:train_test as identifier
total_acc_x_test<- read.table("dataset/test/Inertial Signals/total_acc_x_test.txt")
total_acc_x_test$train_test <- rep(test_flag,nrow(total_acc_x_test))
##Read total_acc_y_test.txt into Data frame:total_acc_y_test, and add new column:train_test as identifier
total_acc_y_test<- read.table("dataset/test/Inertial Signals/total_acc_y_test.txt")
total_acc_y_test$train_test <- rep(test_flag,nrow(total_acc_y_test))
##Read total_acc_z_test.txt into Data frame:total_acc_z_test, and add new column:train_test as identifier
total_acc_z_test<- read.table("dataset/test/Inertial Signals/total_acc_z_test.txt")
total_acc_z_test$train_test <- rep(test_flag,nrow(total_acc_z_test))





y_train$train_test <- rep(train_flag,nrow(y_train))
y_test$train_test <- rep(test_flag,nrow(y_test))
##combine y_train and y_test 
y_all <- rbind(y_train,y_test)

##combine subject_train and subject_test 
subject_all <- rbind(subject_train,subject_test)

##combine xtrain_data and xtest_data
xall_data <- rbind(xtrain_data,xtest_data)

##combine measurements from train and test
body_acc_x_all <- rbind(body_acc_x_train,body_acc_x_test)
body_acc_y_all <- rbind(body_acc_y_train,body_acc_y_test)
body_acc_z_all <- rbind(body_acc_z_train,body_acc_z_test)

body_gyro_x_all <- rbind(body_gyro_x_train,body_gyro_x_test)
body_gyro_y_all <- rbind(body_gyro_y_train,body_gyro_y_test)
body_gyro_z_all <- rbind(body_gyro_z_train,body_gyro_z_test)

total_acc_x_all <- rbind(total_acc_x_train,total_acc_x_test)
total_acc_y_all <- rbind(total_acc_y_train,total_acc_y_test)
total_acc_z_all <- rbind(total_acc_z_train,total_acc_z_test)


#Add mean,std columns to those measurement files
body_acc_x_all[[make.names(c("tBodyAcc-mean()-X"), unique = TRUE)]]<-xall_data$tBodyAcc.mean...X
body_acc_x_all[[make.names(c("tBodyAcc-std()-X"), unique = TRUE)]]<-xall_data$tBodyAcc.std...X

body_acc_y_all[[make.names(c("tBodyAcc-mean()-Y"), unique = TRUE)]]<-xall_data$tBodyAcc.mean...Y
body_acc_y_all[[make.names(c("tBodyAcc-std()-Y"), unique = TRUE)]]<-xall_data$tBodyAcc.std...Y

body_acc_z_all[[make.names(c("tBodyAcc-mean()-Z"), unique = TRUE)]]<-xall_data$tBodyAcc.mean...Z
body_acc_z_all[[make.names(c("tBodyAcc-std()-Z"), unique = TRUE)]]<-xall_data$tBodyAcc.std...Z

body_gyro_x_all[[make.names(c("tBodyGyro-mean()-X"), unique = TRUE)]]<-xall_data$tBodyGyro.mean...X
body_gyro_x_all[[make.names(c("tBodyGyro-std()-X"), unique = TRUE)]]<-xall_data$tBodyGyro.std...X

body_gyro_y_all[[make.names(c("tBodyGyro-mean()-Y"), unique = TRUE)]]<-xall_data$tBodyGyro.mean...Y
body_gyro_y_all[[make.names(c("tBodyGyro-std()-Y"), unique = TRUE)]]<-xall_data$tBodyGyro.std...Y

body_gyro_z_all[[make.names(c("tBodyGyro-mean()-Z"), unique = TRUE)]]<-xall_data$tBodyGyro.mean...Z
body_gyro_z_all[[make.names(c("tBodyGyro-std()-Z"), unique = TRUE)]]<-xall_data$tBodyGyro.std...Z

#Create a new column "activity_name" 
y_all$activity_name<-gen_col_by_cell(y_all$activity_label)

##merge y_all and subject_all into subject_acitivity_all
subject_activity_all<-bind_cols(subject_all, y_all)

##merge subject_activity_all with xall_data
subject_activity_features_all<-bind_cols(subject_activity_all, xall_data)

#Create a tidy data set with the average of each variable for each activity and each subject
tidy_data<-aggregate( subject_activity_features_all[,5:565], subject_activity_features_all[,1:2], FUN = mean )
#Add activity name to tidy_data
a_name <- gen_col_by_cell(tidy_data$activity_label)
tidy_data<-as.data.frame(append(tidy_data, list(activity_name = a_name), after = 2))

print("Creating tidy dataset")

#Write tidy_data to tidy_data.csv
write.csv(tidy_data, file = "tidy_data.csv")

print("Dataset created")
