library(dplyr)
setwd("~/Git/Contests/DrivenData/WaterTable")
training1 <- read.csv("training.csv",sep=",")
training2 <- read.csv("training_labels.csv",sep=",")
training <- inner_join(training1,training2,by="id")
rm(training1,training2)

clean <- function(input){
  # Make training labels shorter
  levels(input$status_group) <- c("F","R","NF")
  
  # Make factors character
  input[sapply(input,class)=="factor"] <- lapply(input[sapply(input,class)=="factor"],as.character)
  
  numvars <- colnames(input)[sapply(input,class)!="character"]

  input[c("region_code","district_code")] <- lapply(input[c("region_code","district_code")],as.character)
  
  char_vars <- colnames(input)[sapply(input,class)=="character"]
  
  input$date_recorded <- as.Date(input$date_recorded)
  input$month_recorded <- as.character(month(input$date_recorded,label=TRUE))
  
  # Drop variables with too many levels (and recorded_by since it has 1 level)
  input <- subset(input,select=-c(date_recorded,wpt_name,subvillage,ward,scheme_name,recorded_by))
  
  return(input)
}

output <- clean(training)
write.table(output,file="~/Git/Contests/DrivenData/WaterTable/train_cleaned.csv",sep="\t")
rm(list=ls())
