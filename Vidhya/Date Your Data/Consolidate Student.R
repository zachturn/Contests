# We need to consolidate the student data set into a unique row per student.

# Basically, what I want to do is convert the Experience Type column into new variables,
# with counts of each Experience Type listed. Also, I want to create a variable indicating
# the student's total length of work experience (in months)

setwd("~/Git/Contests/Vidhya/Date Your Data/")
student <- read.csv("Student.csv",header=TRUE)
student$Start.Date <- as.character(student$Start.Date)
student$End.Date <- as.character(student$End.Date)

# First, we need to remove the profile, Location, degree variable. There's just too many 
# levels to be meaningful. 
student <- subset(student,select=-c(Profile,Degree,Location))

# To make computation easier, let's grab only the variables we are concerned with
reduced <- student[,c("Student_ID","Experience_Type","Start.Date","End.Date")]

# Create new data frame with our desired structure.


consolidated <- data.frame(cbind(unique(reduced$Student_ID),0,0,0,0,0,0,0,0,0,0,0))
colnames(consolidated) <- c("Student_ID","academic_project","award","internship",
                            "job","NULL","other","participation",
                            "por","training","workshop","Weeks_Experience")

count_elements <- function(x){
  # Store the counts of each experience type all in one row
  for(i in 1:nrow(consolidated)){
    indices <- which(x$Student_ID==consolidated$Student_ID[i])
    temp <- x[indices,]
    table <- table(temp$Experience_Type)
    for(j in 1:10){
      consolidated[i,j+1] <- table[j]
    }
    # Use the date time variables to extract # months experience
    for(k in 1:nrow(temp)){
      if(!(temp[k,"Start.Date"]=="NULL" | temp[k,"End.Date"]=="NULL")){
        start <- as.Date(temp[k,"Start.Date"],format="%d-%m-%Y")
        end <- as.Date(temp[k,"End.Date"],format="%d-%m-%Y")
        months <- difftime(end,start,units="weeks")
        consolidated[i,"Weeks_Experience"] <- consolidated[i,"Weeks_Experience"] +months
      }
    }
  }
  return(consolidated)
}

# Run the function to populate the new data frame
consolidated <- count_elements(reduced)

# Now let's merge the two student data sets together to get our final, unique student ID
# dataset.
student2 <- merge(subset(student,select=-c(Experience_Type,Start.Date,End.Date)),
                  consolidated,by="Student_ID")
unique_student <- unique(student2)

write.table(x = unique_student,file="unique_student.csv",sep=",")


