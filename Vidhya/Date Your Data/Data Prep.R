# Load and Clean Data

setwd("~/Git/Contests/Vidhya/Date Your Data/")

# Load Data
internship <- read.csv("Internship.csv",header=TRUE)
train <- read.csv("train.csv",header=TRUE)
#test <- read.csv("test.csv",header=TRUE)

#### We will load in the unique_student data set from "Consolidate Student.R" 
unique_student <- read.csv("unique_student.csv",header=TRUE)


# Merge into one training set (also remove any duplicate entries)
temp <- merge(train,unique_student,by = "Student_ID")
training <- merge(temp,internship,by="Internship_ID")
rm(temp,unique_student,train,internship)

# First, check target rate
sum(as.integer(training$Is_Shortlisted))/nrow(training)

# Target rate is somewhat low, might want to weight classes.

##### Converting Categorical to Numeric #####

# What data types are we working with?
dtypes <- sapply(training,FUN=class)
table(dtypes)
which(dtypes=="factor")

# We can create a scaled GPA variable
PG_GPA <- training$Performance_PG/training$PG_scale
UG_GPA <- training$Performance_UG/training$UG_Scale

# However, many GPAs aren't listed. We should probably make a missing indicator for this,
# as people probably don't truly have a 0.0  GPA
PG_Missing_GPA <- ifelse(PG_GPA==0,1,0)
UG_Missing_GPA <- ifelse(UG_GPA==0,1,0)

# Now we should remove the old GPA variables 
training <- subset(training,select=-c(Performance_PG,PG_scale,Performance_UG,UG_Scale))
training <- cbind(training,PG_GPA,PG_Missing_GPA,UG_GPA,UG_Missing_GPA)

rm(PG_GPA,UG_GPA,PG_Missing_GPA,UG_Missing_GPA)

##### Imputation #####

# Check for missing values
missing <- sapply(X=training,FUN=anyNA)
which(missing==TRUE)
# Missing Values only appear to be an issue for 3 variables (Degree, Stream, Profile)
percent_missing <- sum(is.na(training$Stream))/nrow(training)

# There are so few missing values, I'm willing to just throw them out
remove <- which(is.na(training$Stream))
training <- training[-remove,]
rm(remove)

# Let's save just so we don't have to re-do this
save(training,file="cleaned_training.RData")

##### Check levels of categorical variables #####
number_levels <- sapply(X=training,FUN=function(x) length(unique(x)))
which(number_levels > 20)

