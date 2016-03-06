# Experimental design: 30 test subjects x 2 data sets (testing & training) x 6 exercise activities x 561 variables/activity
# Descriptions of files:
# X_train = variables for the training data set, 7351 rows, 561 columns (find using str(x_train))
# X_test = variables for the test data set, 2946 rows, 561 columns
# y_train = exercise activities for the training data set as code, 7351 rows, 1 column
# y_test = exercise activites for the test data set as code, 2946 rows, 1 column
# subject_test = subject for the test data set, 2946 rows, 1 column
# subject_train	= subject for the training data set, 7351 rows, 1 column
# features = names of exercise activities for both the test and training data sets, 560 rows, 2 columns (1st column is index)
# activity_labels = names for exercise activities corresponding to code in y_train & y_test files

# Read in data and get dimensions. Use default separate sep="".
setwd("C:/Users/Amy/Documents/Data Science Specialization - Coursera/3 - Getting and Cleaning Data/Project")
x_train <- read.table("./X_train.txt", header = TRUE) # turns data into data frame & characters into vectors
y_train <- read.table("./y_train.txt", header = TRUE)
subject_train <- read.table("./subject_train.txt", header = TRUE)
x_test <- read.table("./X_test.txt", header = TRUE)
y_test <- read.table("./y_test.txt", header = TRUE)
subject_test <- read.table("./subject_test.txt", header = TRUE)
features <- read.table("./features.txt", header = FALSE, stringsAsFactors = FALSE) # keep characters as characters
exercise <- read.table("./activity_labels.txt", header = FALSE, stringsAsFactors = FALSE) # headers are V1 & V2

# Add column names to dataframes
colnames(y_train) <- "act"
colnames(y_test) <- "act"
colnames(subject_test) <- "Subject"
colnames(subject_train) <- "Subject"
activities <- features[ ,2] # create character vector of names of exercise activities (column names)
colnames(x_train) <- activities 
colnames(x_test) <- activities

# add column Status indicating whether data came from testing or training
Stest <- rep("testing", times = nrow(x_test))
Strain <- rep("training", times = nrow(x_train))
x_test$Status <- Stest	
x_train$Status <- Strain

# combine test datasets first, then training datasets
test <- cbind(y_test, subject_test, x_test)
train <- cbind(y_train, subject_train, x_train)
# merge test and training datasets together
data1 <- rbind(test, train) # merge function should work but doesn't

# Generate character vector of all column names except those that aren't means and standard deviations (std)
# Assuming that means are column names ending in mean(), not ones with Mean in the name
# pipe symbol means mean() OR std(); \\ used to indicate () isn't an operator but regular characters
keep <- grep("mean\\()|std\\()", activities, value = TRUE)
# melt data set so all mean and std variables are in column called variables
library(reshape2)
data <- melt(data1, id = c("Status", "act", "Subject"), measure = keep)
# unique(data$variable) returns 66 unique variables

# create Exercise_Activity column with descriptive terms substituted for numbers
ea1 <- as.data.frame(data$act)
names(ea1) <- "V1" # V1 is name of column containing code in activity_labels
ea2 <- merge(ea1, exercise, by = "V1", all.x = TRUE) # V2 column
data$Exercise_Activity <- ea2$V2

# Create Domain column to separate t and f, the first character in the column name
data$Domain <- substr(data$variable, 0, 1)
# change one-letter symbols to descriptive labels
data$Domain[data$Domain == "t"] <- "time"
data$Domain[data$Domain == "f"] <- "frequency"

# Create Acceleration_Signal column to separate Body and Gravity, using Body as default
data$Acceleration_Signal <- rep("body motion", times = nrow(data))
ASg <- grep("Gravity", data$variable)
data[ASg, "Acceleration_Signal"] <- "gravitational"

# Create Sensor column to separate Acc and Gyro, using Acc as default
data$Sensor <- rep("acceleration", times = nrow(data))
Sg <- grep("Gyro", data$variable)
data[Sg, "Sensor"] <- "gyroscope"

# Create Signal_Type column to separate out Jerk, default is Not Jerk
data$Signal_Type <- rep("Not Jerk", times = nrow(data))
J <- grep("Jerk", data$variable)
data[J, "Signal_Type"] <- "Jerk"

# Create Measure column to separate out Mag, default is amplitude
data$Measure <- rep("amplitude", times = nrow(data))
M <- grep("Mag", data$variable)
data[J, "Measure"] <- "magnitude"

# Create Axis column to separate out -X, -Y and -Z, making all the default
data$Axis <- rep("all", times = nrow(data))
X <- grep("-X", data$variable)
Y <- grep("-Y", data$variable)
Z <- grep("-Z", data$variable)
data[X, "Axis"] <- "X"
data[Y, "Axis"] <- "Y"
data[Z, "Axis"] <- "Z"

# Create column Variable to separate out mean and std, making mean the default 
data$Variable <- rep("average", times = nrow(data))
sd <- grep("std", data$variable)
data[sd, "Variable"] <- "standard deviation"

# Rearrange columns + exclude redundant act & variable columns
finaldata <- subset(data, select = c(Exercise_Activity, Subject, Status, Sensor, Domain, Acceleration_Signal, Signal_Type, Measure, Axis, Variable, value))  
str(finaldata) # indicates data frame has 67902 observations of 11 variables

# Create second data set with average of each variable for each activity and each subject
# average of means and average of standard deviations in 2 separate columns
summary_data <- dcast(finaldata, Exercise_Activity + Subject ~ Variable, mean) # returns df of 180 rows x 4 variables
# write data to text file
write.table(summary_data, "./summarydata.txt", row.name=FALSE)