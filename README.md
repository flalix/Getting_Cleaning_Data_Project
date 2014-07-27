---
title: "Tidy_Project"
author: "Flavio Lichtenstin"
date: "Sunday, July 27, 2014"
output: html_document
---

## Code:
   All code can be found in tidy_project.md
   The R code can be dowloaded at run_analysis.R
  
# Data can be grouped by:
* WALKING
* WALKING UPSTAIRS
* WALKING DOWNSTAIRS
* SITTING
* STANDING
* LAYING
  

### Data training:
   . Only data training can be analysed
   . Data test no. 
   
### Variables:
  subj_train: vector with subjetcts
  data_train: data training data.frame

### Data analysis:
  Dimension and header is shown
 
  data_walk_up <- walk data is filtered
  data_laying  <- data_train[data_train$y == LAYING, ]
labels
```
  
### Descriptive analysis
  
Analyzing some data:
  
   Mean and Standard Deviation:

```{r, echo=TRUE}
sapply(data_train[c(3:7)], mean)
  
sapply(data_train[c(3:7)], sd)
```

   Total body accelarion - mean:
```{r, echo=TRUE}
summary(data_walk_up$"tBodyAccMag.mean()")
```
  
   Total body accelarion - quantile:
```{r, echo=TRUE}
quantile(data_laying$"tBodyAccMag.mean()", na.rm=TRUE)
```
  
### Box-plot
  
   Total body accelarion:
```{r, echo=FALSE}
title <- "Mean Body Acceleration"
x <- data_train$"tBodyAccMag.mean()"
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)
```
  
   Total Body gyro Y Acceleration:
```{r, echo=FALSE}
title <- "Mean Body Gyro Acceleration Y"
x <- data_train$"tBodyGyro-mean()-Y"
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)
```
  
   Total body freq gyro X:
```{r, echo=FALSE}
title <- "Mean Body f(gyro-X)"
x <- data_train$"fBodyGyro-mean()-X"
labels <- c("WALK", "WALK UP", "WALK DWN", "SITT", "STAND", "LAY")
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)
```
  
   Total body freq gyro Y:
```{r, echo=FALSE}
title <- "Mean Body f(gyro-Y)"
x <- data_train$"fBodyGyro-mean()-Y"
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65)
```
  
   Total body freq gyro Z:
```{r, echo=FALSE}
title <- "Mean Body f(gyro-Z)"
x <- data_train$"fBodyGyro-mean()-Z"
boxplot(x~data_train$y, col="lightblue",main=title,xlab="", xaxt = "n",ylab="acceleration")
axis(1, at=1:length(labels), labels=labels, las=2, adj= 1, xpd = TRUE, cex=0.65

newNames <- names(data_train)

v1 <- match("tBodyAccMag.mean()",newNames) 
v2 <- match("tBodyAcc-energy()-X",newNames) 
v3 <- match("tBodyAcc-entropy()-X",newNames) 
v4 <- match("fBodyGyro-mean()-Z",newNames) 
v5 <- match("fBodyGyro-energy()-Z",newNames) 

vNames <- c("<Acc>","Acc-erg","Acc-h","<f(gyro Z)>","ferg(gyro Z)")
lista <- c(v1,v2, v3,v4,v5)

this <- data_train[data_train$y==c(STANDING, WALKING_DOWNSTAIRS),]
labels <- c("STAND", "WALK DOWN")
emptyLabList <- list(c("", "", "", "", ""))
```


Scatter plot analysis:

```{r, echo=FALSE}
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
```

