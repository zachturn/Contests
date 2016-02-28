# Load and Clean Data

setwd("~/Git/Contests/Vidhya/Date Your Data/")

# Load Data
internship <- read.csv("Internship.csv",header=TRUE)
unique_student <- read.csv("unique_student.csv",header=TRUE)
test <- read.csv("test.csv",header=TRUE)

# Merge into one testing set (also remove any duplicate entries)
temp <- merge(test,unique_student,by = "Student_ID")
testing <- merge(temp,internship,by="Internship_ID")
rm(temp,unique_student,test,internship)

# First, check target rate
sum(as.integer(testing$Is_Shortlisted)-1)/dim(testing)[1]

# Target rate is somewhat low, might want to weight classes.


# We can create a scaled GPA variable
PG_GPA <- testing$Performance_PG/testing$PG_scale
UG_GPA <- testing$Performance_UG/testing$UG_Scale

# However, many GPAs aren't listed. We should probably make a missing indicator for this,
# as people probably don't truly have a 0.0  GPA
PG_Missing_GPA <- ifelse(PG_GPA==0,1,0)
UG_Missing_GPA <- ifelse(UG_GPA==0,1,0)

# Now we should remove the old GPA variables 
testing <- subset(testing,select=-c(Performance_PG,PG_scale,Performance_UG,UG_Scale))
testing <- cbind(testing,PG_GPA,PG_Missing_GPA,UG_GPA,UG_Missing_GPA)

rm(PG_GPA,UG_GPA,PG_Missing_GPA,UG_Missing_GPA)

##### Imputation #####

# Check for missing values
missing <- sapply(X=testing,FUN=anyNA)
which(missing==TRUE)

# Stream is only variable with missing values. This never entered the model anyways
# because it had too many levels. So we can just ignore it for now.
# <- which(is.na(testing$Stream))

#testing <- testing[-remove,]
#rm(remove)