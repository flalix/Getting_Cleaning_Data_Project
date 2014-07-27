#-- setwd("C:/Users/Flavio/rcode/flavio/penr_r_prog/quizz")
rm(list=ls())

setwd("C:/Users/Flavio/rcode/flavio/jtleek/project")
getwd()

#--- run_analysis.R
#- The experiments have been carried out with a group of 30 volunteers within an age 
#  bracket of 19-48 years
#
#  Each person performed six activities
#     WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
#
#      captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate
#
#  The obtained dataset has been randomly partitioned into two sets, 
#      where 70% of the volunteers was selected for generating the training data and 30% the test data. 

# treaining = 30 * 0.7  = 21 subjects
# data      = 30 * 0.3  =  9 subjects


#======= ativity ==========================================

# 'activity_labels.txt': Links the class labels with their activity name.

activ <- read.csv("activity_labels.txt",head=FALSE, sep=" ")

# V1                 V2
# 1  1            WALKING
# 2  2   WALKING_UPSTAIRS
# 3  3 WALKING_DOWNSTAIRS
# 4  4            SITTING
# 5  5           STANDING
# 6  6             LAYING

#===== features ============================================

feat <- read.csv("features.txt",head=FALSE, sep=" ")

head(feat)
dim(feat)
[1] 561   2

#-- feat$V2[1:10]
colNames <- feat$V2
length(colNames)

#=================================================


#-- Each row identifies the subject who performed the activity 
#-- for each window sample. Its range is from 1 to 30. 
subj_train <- read.csv("subject_train.txt",head=FALSE, sep=" ")
subj_train
nrow(subj_train)
head(subj_train)


#- 'train/y_train.txt': Training labels.
ytrain <- read.csv("y_train.txt",head=FALSE, sep=" ")
ytrain
nrow(ytrain)
[1] 7352
head(ytrain,12)

##################################################
##  Training data
##################################################
# - 'train/X_train.txt': Training set.
data_train <- read.table("tidy.txt", sep=" ") # ,head=TRUE, sep=" ", dec = ".")
dim(data_train)

names(data_train) <- c("subject", "y", "tBodyAccMag.mean()", "tBodyAcc-energy()-X",  "tBodyAcc-entropy()-X", "fBodyGyro-mean()-Z",  "fBodyGyro-energy()-Z")
names(data_train) 

data_train$"tBodyAccMag.mean()"


str(data_train)
# 'data.frame':  7352 obs. of  561 variables:
#   $ tBodyAcc-mean()-X                   : num  0.289 0.278 0.28 0.279 0.277 ...

sum_train <- summary(data_train)
sum_train[1:6,1:2]

# tBodyAcc-mean()-X   tBodyAcc-mean()-Y   
# "Min.   :-1.0000  " "Min.   :-1.00000  "
# "1st Qu.: 0.2630  " "1st Qu.:-0.02486  "
# "Median : 0.2772  " "Median :-0.01722  "
# "Mean   : 0.2745  " "Mean   :-0.01770  "
# "3rd Qu.: 0.2885  " "3rd Qu.:-0.01078  "
# "Max.   : 1.0000  " "Max.   : 1.00000  "


##################################################
##  Training data: adding Subject and Y
##################################################
# data_train <- cbind(subj=subj_train$V1, y=ytrain  ,data_train)

##################################################
##  Defining column names
##################################################
#names(data_train) <- c('subject', 'y', as.vector(colNames) )
#
#data_train[1:12, 1:10]
#data_train[1:5, ]


# For each record in the dataset it is provided: 
# - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
# - Triaxial Angular velocity from the gyroscope. 
# - A 561-feature vector with time and frequency domain variables. 
# - Its activity label. 
# - An identifier of the subject who carried out the experiment.

##################################################
##  Possible statues: to group
##################################################
WALKING <- 1
WALKING_UPSTAIRS <- 2
WALKING_DOWNSTAIRS <- 3
SITTING <- 4
STANDING <- 5
LAYING <- 6

##################################################
##  Means and SDV
##################################################
v1 <- match("tBodyAccMag.mean()",newNames) 
v2 <- match("tBodyAcc-energy()-X",newNames) 
v3 <- match("tBodyAcc-entropy()-X",newNames) 
v4 <- match("fBodyGyro-mean()-Z",newNames) 
v5 <- match("fBodyGyro-energy()-Z",newNames) 


##################################################
##  Analyzing some data
##################################################
data_walk_up <- data_train[data_train$y == WALKING_UPSTAIRS, ]
data_laying  <- data_train[data_train$y == LAYING, ]

names(data_walk_up)
summary(data_walk_up$"tBodyAccMag.mean()")


sum(is.na(data_laying$"tBodyAccMag.mean()"))
sum(is.na(data_walk_up$"tBodyAccMag.mean()"))

quantile(data_laying$"tBodyAccMag.mean()", na.rm=TRUE)
#0%        25%        50%        75%       100% 
#-0.9995045 -0.9915823 -0.9777668 -0.9555551  0.7152385

quantile(data_walk_up$"tBodyAccMag.mean()", na.rm=TRUE)
#0%         25%         50%         75%        100% 
#-0.43736016 -0.18436462 -0.09363462  0.02450335  0.35323247 


##################################################
##  Descriptive analysis
##################################################

##################################################
##  setting constants
##################################################
labels <- c("WALK", "WALK UP", "WALK DWN", "SITT", "STAND", "LAY")


##################################################
##  mean total body acc boxplot
##################################################
title <- "Mean Body Acceleration"
x <- data_train$"tBodyAccMag.mean()"
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)


##################################################
##  mean total body gyro Y acc boxplot
##################################################
title <- "Mean Body Gyro Acceleration Y"
x <- data_train$"tBodyGyro-mean()-Y"
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)

##################################################
##  mean total body freq gyro X boxplot
##################################################
title <- "Mean Body f(gyro-X)"
x <- data_train$"fBodyGyro-mean()-X"
labels <- c("WALK", "WALK UP", "WALK DWN", "SITT", "STAND", "LAY")
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)

##################################################
##  mean total body freq gyro Y boxplot
##################################################
title <- "Mean Body f(gyro-Y)"
x <- data_train$"fBodyGyro-mean()-Y"
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)


##################################################
##  mean total body freq gyro Z boxplot
##################################################
title <- "Mean Body f(gyro-Z)"
x <- data_train$"fBodyGyro-mean()-Z"
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)


##################################################
##  defining var positions
##################################################
#v1 <- match("tBodyAccMag.mean()",feat$V2)+2
#v2 <- match("tBodyAcc-energy()-X",feat$V2)+2
#v3 <- match("tBodyAcc-entropy()-X",feat$V2)+2
#v4 <- match("fBodyGyro-mean()-Z",feat$V2)+2
#v5 <- match("fBodyGyro-energy()-Z",feat$V2)+2

##################################################
##  deleting undesired data and redefinif vars
##  tidy data frame ... from here ....
##################################################
#data_train <- data_train[ c(1,2,v1,v2, v3,v4,v5) ]
#dim(data_train)
newNames <- names(data_train)

names(data_train)

v1 <- match("tBodyAccMag.mean()",newNames) 
v2 <- match("tBodyAcc-energy()-X",newNames) 
v3 <- match("tBodyAcc-entropy()-X",newNames) 
v4 <- match("fBodyGyro-mean()-Z",newNames) 
v5 <- match("fBodyGyro-energy()-Z",newNames) 


##################################################
##  defining var names
##################################################
vNames <- c("<Acc>","Acc-erg","Acc-h","<f(gyro Z)>","ferg(gyro Z)")
lista <- c(v1,v2, v3,v4,v5)

##################################################
##  filtering Standing and Walking Downstairs
##################################################
this <- data_train[data_train$y==c(STANDING, WALKING_DOWNSTAIRS),]
labels <- c("STAND", "WALK DOWN")
emptyLabList <- list(c("", "", "", "", ""))

########################################################
##  Lattice scatter plot (without regression analysis)
########################################################
library(lattice)

splom(this[lista], groups=this$y, 
      panel=panel.superpose, 
      varnames = vNames,
      diag.panel = function(x, ...){
        yrng <- current.panel.limits()$ylim
        d <- density(x, na.rm=TRUE)
        d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        panel.lines(d)
        diag.panel.splom(x, ...)
      },        
      key=list(title="Physical Data from Cell Device",
               columns=length(lista),
               text=emptyLabList))


########################################################
##  Saving file
########################################################

# save(data_train,file="data_train.Rda")
# any <- load("data_train.Rda")

write.table(data_train, file = "tidy.txt", sep = " ", col.names = colnames(data_train))

# read it back in
any <- read.table(file="tidy.txt",sep = " ", header = TRUE, stringsAsFactors=FALSE) 


names(any)
head(any)

########################################################
##  Didn't analyze test data
########################################################

data_test  <- read.table("../UCI HAR Dataset/test/X_test.txt") # ,head=FALSE, sep=" ")
dim(data_test)
[1] 2947  561


subj_test <- read.csv("../UCI HAR Dataset/test/subject_test.txt",head=FALSE, sep=" ")
subj_test
nrow(subj_test)
[1] 2947


ytest <- read.csv("../UCI HAR Dataset/test/y_test.txt",head=FALSE, sep=" ")
nrow(ytest)


str(data_test)
# 'data.frame':  7352 obs. of  561 variables:
#   $ tBodyAcc-mean()-X                   : num  0.289 0.278 0.28 0.279 0.277 ...

data_test <- summary(data_test)
data_test[1:6,1:2]

tBodyAcc-mean()-X   tBodyAcc-mean()-Y   
# "Min.   :-0.5920  " "Min.   :-0.36288  "
# "1st Qu.: 0.2621  " "1st Qu.:-0.02496  "
# "Median : 0.2771  " "Median :-0.01697  "
# "Mean   : 0.2740  " "Mean   :-0.01786  "
# "3rd Qu.: 0.2881  " "3rd Qu.:-0.01014  "
# "Max.   : 0.6719  " "Max.   : 0.24611  "



library(knitr)
