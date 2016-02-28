# Run initial Random Forest just to benchmark predictions
library(randomForest)
load("cleaned_training.RData")

# Remove categorical variables with over 32 levels (also remove the ID variables)
is_factor <- sapply(X=training, FUN=is.factor)
over_30 <- sapply(X=training,FUN=function(x) ifelse(length(unique(x))>32,TRUE,FALSE))
remove <- unname(ifelse(is_factor & over_30, TRUE, FALSE))
remove[1] <- TRUE
remove[2] <- TRUE

# Convert response to factor
training$Is_Shortlisted <- as.factor(training$Is_Shortlisted)

start_time <- proc.time()
test_run <- randomForest(Is_Shortlisted ~.,data=training[,!remove],
                         ntree=500,sampsize=.1*nrow(training),
                         nodesize=50,importance=TRUE)
run_time <- proc.time() - start_time
run_time

preds <- predict(test_run,newdata=testing)

submission <- data.frame(cbind(testing$Internship_ID,testing$Student_ID,as.integer(preds)-1))
colnames(submission) <- c("Internship_ID","Student_ID","Is_Shortlisted")

write.table(submission,file="Submission1.csv",row.names=FALSE,col.names=TRUE, 
            quote=FALSE,sep=",")
