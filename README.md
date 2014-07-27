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
1. WALKING
2. WALKING UPSTAIRS
3. WALKING DOWNSTAIRS
4. SITTING
5. STANDING
6. LAYING
  

### Data training:

* Only data training can be analysed
* Data test no. 
   
### Variables:
. subj_train: vector with subjetcts
. data_train: data training data.frame
 
. data_walk_up: walk data is filtered
. data_laying: laying data is filtered
  
. newNames <- names(data_train)

. v1: tBodyAccMag position
. v2: tBodyAcc-energy()-X position
. v3: tBodyAcc-entropy() position
. v4: fBodyGyro-mean()-Z position
. v5: fBodyGyro-energy() position

. vNames: simplified names
. lista: vector with data positions

. this <- data_train filtered only for STANDING, WALKING_DOWNSTAIRS, for scatter analysis
. labels: group lables = ""STAND"", ""WALK DOWN""
. emptyLabList: don''t show the labels

### Data analysis:
  Dimension and header is shown
  Mean and Standard Deviation is presented

  Some summary:
      Total body accelarion - mean
      Total body accelarion - quantile:
 
      a better analyis is not presented here.
  
### Box-plot
  
. Total body accelarion
. Total Body gyro Y Acceleration
. Total body freq gyro X
. Total body freq gyro Y
. Total body freq gyro Z

### Box-plot
.  Lattice scatter analysis is presented.



