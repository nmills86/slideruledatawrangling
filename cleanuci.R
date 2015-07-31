setwd("C:/Users/Nick/Documents/Capstone/UCI Har Dataset/")

library("dplyr")
library("Hmisc")

# MERGE TEST AND TRAINING SET
      setwd("C:/Users/Nick/Documents/Capstone/UCI Har Dataset/train")

      traindata <- read.table("X_train.txt") 
      trainlabel <- read.table("y_train.txt")
      trainsubject <- read.table("subject_train.txt")
      
      setwd("C:/Users/Nick/Documents/Capstone/UCI Har Dataset/test")

      testdata <- read.table("X_test.txt")
      testlabel <- read.table("y_test.txt")
      testsubject <- read.table("subject_test.txt")
      
      # COLUMNS SHOULD BE THE SAME, THEREFORE COMBINE USING RBIND. 
      data <- rbind(traindata,testdata)
      yvar <- rbind(trainlabel,testlabel)
      subject <- rbind(trainsubject, testsubject)

# NAME THE VARIABLES 
      setwd("C:/Users/Nick/Documents/Capstone/UCI Har Dataset/")

      names <- read.table("features.txt")
      write.csv(names, "names.csv")
      names <- as.vector(names[,2])
      
      for (i in 1:ncol(data)) {
            colnames(data)[i] <- names[i]
      }

      unames <- make.names(names=names(data), unique = TRUE, allow_ = TRUE)
      names(data) <- unames


# KEEP ONLY MEANS AND STANDARD DEVIATIONS

      newdatamean <- select(data, contains("mean"))
      newdatastd <-  select(data, contains("std"))
      newdata <- cbind(newdatamean, newdatastd)
      newdata <- select(newdata, -contains("Freq"))

# RENAME VARIABLES

# DEFAULT VARIABLE NAMES LOOK PRETTY GOOD, I am going to remove the "."
# FROM ALL NAMES, AND ALSO MAKE THINGS LOWERCASE

names <- colnames(newdata)
for (i in 1:ncol(newdata)) {
      colnames(newdata)[i] <- tolower(gsub("\\.","",names[i]))
}

# ADD ACTIVITY NAMES
names <- c("walking","walking_upstairs","walking_downstairs","sitting","standing","laying")
yvar <- as.vector(yvar)
yvarnew <- as.character(yvar)
for (i in 1:6) {
      j <- as.numeric(i)
      word <- names[i]
      yvarnew <- ifelse(yvar==j, word, yvarnew)
}

# ADD YVARNEW AS ACTIVITY TO DATASET
newdata$activity <- as.vector(yvarnew)

# ADD SUBJECT ID TO DATASeT
subject <- as.vector(as.matrix(subject))
newdata$subject <- as.vector(subject)

# TAKE MEANS
newdatamean <- aggregate(. ~ subject + activity, newdata, mean)

# Build strings that serve as variable labels

names <- colnames(newdatamean)
for (i in 1:ncol(newdatamean)) {
      word1 <- ifelse(substr(names[i],1,1)=="f","transformed ","")
      word1 <- ifelse(substr(names[i],1,1)=="t","untransformed ",word1)
      word3 <- ifelse(grepl("body",names[i])==TRUE, "body signal ","")
      word3 <- ifelse(grepl("grav",names[i])==TRUE, "gravity signal ",word3)
      word5 <- ifelse(grepl("acc",names[i])==TRUE, "from accelerometer ","")
      word5 <- ifelse(grepl("gyro",names[i])==TRUE, "from gyroscope ",word5)
      word2 <- ifelse(grepl("jerk",names[i])==TRUE, "jerk ","")
      word4 <- ifelse(grepl("mag",names[i])==TRUE, "magnitude ","")
      word6 <- ifelse(grepl("std",names[i])==TRUE, ",standard deviation,", "")
      word6 <- ifelse(grepl("mean",names[i])==TRUE, "mean", word6)
      word7 <- ifelse(substr(names[i],nchar(names[i]),nchar(names[i]))=="x",",x axis","")
      word7 <- ifelse(substr(names[i],nchar(names[i]),nchar(names[i]))=="y",",y axis",word7)
      word7 <- ifelse(substr(names[i],nchar(names[i]),nchar(names[i]))=="z",",z axis",word7)
      label <- paste(word1,word2,word3,word4,word5,word6,word7)
      label(newdatamean[,i]) <- label
}

label(newdatamean[,36]) <- "Angle of untranformed body signal from accelerometer, mean gravity"
label(newdatamean[,37]) <- "Angle of untransformed jerk body signal from accelerometer, mean gravity "
label(newdatamean[,38]) <- "Angle of untransformed body signal from gyroscope, mean gravity"
label(newdatamean[,39]) <- "Angle of untransformed jerk body signal from gyroscope, mean gravity"
label(newdatamean[,40]) <- "Angle of X gravity mean"
label(newdatamean[,41]) <- "Angle of Y gravity mean"
label(newdatamean[,42]) <- "Angle of Z gravity mean"
label(newdatamean[,1]) <- "Subject"
label(newdatamean[,2]) <- "Activity"

#EXPORT
write.table(newdatamean, "cleandata.csv")

#Codebook
var1 <- as.data.frame(as.vector((names(newdatamean))))
var2 <- as.data.frame(as.vector(label(newdatamean)))

codebook <- cbind(var1,var2)
names(codebook) <-c("varname","varlabel")
write.table(codebook,"codebook.md",sep="  ")

