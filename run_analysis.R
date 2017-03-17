
# Reads the base sets (files with begining by X) in an optimal way
readBaseSet <- function(filePath, filteredFeatures, features) {
  cols_widths <- rep(-16, length(features))
  cols_widths[filteredFeatures] <- 16
  rawSet <- read.fwf(
    file=filePath,
    widths=cols_widths,
    col.names=features[filteredFeatures])
}

# Reads an additional file (other than the base sets). Used for subjects and labels.
readAdditionalFile <- function(dataDirectory, filePath) {
  filePathTest <- paste(dataDirectory, "/test/", filePath, "_test.txt", sep="")
  filePathTrain <- paste(dataDirectory, "/train/", filePath, "_train.txt", sep="")
  data <- c(read.table(filePathTest)[,"V1"], read.table(filePathTrain)[,"V1"])
  data
}





# Path used to save data
dataDirectory <- "UCI HAR Dataset"

# Download an extract data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
tmp_file <- "./temp.zip"
download.file(url,tmp_file, method="curl")
unzip(tmp_file, exdir="./")
unlink(tmp_file)

# From sets, creates the tidy dataset (a summary)
# Adding main data files (X_train and X_test)
featuresFilePath <- paste(dataDirectory, "/features.txt", sep="")
features <- read.table(featuresFilePath)[,"V2"]
filteredFeatures <- sort(union(grep("mean\\(\\)", features), grep("std\\(\\)", features)))

# Correct a feature name - makes it nicer for dataframe columns (removes parentheses)
# because otherwise they are transformed to dots.
features <- gsub("\\(", "", features)
features <- gsub("\\)", "", features)

set <- readBaseSet(paste(dataDirectory, "/test/X_test.txt", sep=""), filteredFeatures, features)
set <- rbind(set, readBaseSet(paste(dataDirectory, "/train/X_train.txt", sep=""), filteredFeatures, features))

# Adding subjects
set$subject <- readAdditionalFile("UCI HAR Dataset", "subject")

# Adding activities
activitiesFilePath <- paste(dataDirectory, "/activity_labels.txt", sep="")
activities <- read.table(activitiesFilePath)[,"V2"]
set$activity <- activities[readAdditionalFile("UCI HAR Dataset", "y")]


sets_x <- set[,seq(1, length(names(set)) - 2)]
summary_by <- by(sets_x,paste(set$subject, set$activity, sep="_"), FUN=colMeans)
summary <- do.call(rbind, summary_by)
summary

# Run the analysis -> creates the tiny data set and saves it under "tiny.txt"
write.table(summary, "tidy.txt")