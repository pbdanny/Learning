setwd("C:/Users/Danny/Documents/R Learning/Coursera/GettingAndCleanningData/Project/")

test <- read.table("./UCI HAR Dataset/test/X_test.txt", stringsAsFactors = F, colClasses = "numeric")
test.act <- read.table("./UCI HAR Dataset/test/y_test.txt", stringsAsFactors = F)
test.subj <- read.table("./UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = F)

train <- read.table("./UCI HAR Dataset/train/X_train.txt", stringsAsFactors = F, colClasses = "numeric")
train.act <- read.table("./UCI HAR Dataset/train/y_train.txt", stringsAsFactors = F)
train.subj <- read.table("./UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = F)

data <- rbind(test, train)

# Load column names data
na <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = F)

# Remove unused running no. in the first column
na <- na[, -1]

# Create logical vector of names with mean() or std()
na.l <- grepl("mean()|std()", na)

# Subsetting data with Logical vector of names na.l
data.sub <- data[,na.l]

# Name subsetted data with subset of colunm names data 
names(data.sub) <- na[na.l]

# -- Done for data part --

# -- Start activity part --

# Combine test and train activity vector
data.act <- rbind(test.act, train.act)
names(data.act) <- "actID"

# Load activity lable
act.label <- read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors = F)
names(act.label) <- c("actID", "activity")

# -- Combine data.sub with activity label --
data.sub.act <- cbind(data.act, data.sub)

# data.sub.act right join act.label on x.actID == y.actID  
data.sub.act.lab <- merge(data.sub.act, act.label, by.x = "actID", by.y = "actID", all.x = T)

# -- Done activity part --

# -- start subject part --
data.subj <- rbind(test.subj, train.subj)
names(data.subj) <- "subID"

# Combine subID to data.sub.act.lab
data.final <- cbind(data.sub.act.lab, data.subj)


# -- Done for data part --

# -- Start creating tidy data mean of each variable in each subject and activity

# Aggregate data by id and activiy
data.agg <- aggregate(data.final ,list(ID=data.final$subID, Activity=data.final$activity), FUN = mean)

# subset unuse column order by ID and activity, reset row.names
data.agg <- subset(data.agg, select = -c(actID, activity, subID))
data.agg <- data.agg[order(data.agg$ID, data.agg$Activity),]
row.names(data.agg) <- NULL

write.table(data.agg, file ="tidy.txt", row.names = F)
