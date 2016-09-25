# This script will collapse the categorical variables that have too many levels.
# The goal is to create a data matrix with 3 variables: one for each class proportion for each unique level
# Then I'll perform k-means to group the levels into a specified number of new levels
# I'll need to store which levels are transformed as well so I can apply this to the test set as well

data <- read.csv("~/Git/Contests/DrivenData/WaterTable/train_cleaned.csv",sep="\t")

collapse_var <- "installer"
target <- "status_group"
max_clusters <- 32

collapse_dict <- function(data,max_clusters,collapse_var,target){
  
  # Create reduced data frame
  reduced <- data[c(collapse_var,target)]
  
  # Tabulate data
  table <- as.data.frame.matrix(table(reduced[,1],reduced[,2]))
  
  # Perform kmeans
  
  results <- lapply(1:max_clusters,function(x) kmeans(table,centers=x,iter.max=20,nstart=10))
  results_ratio <- sapply(results, function(x) return(x[["betweenss"]]/x[["totss"]]))
  for(i in 1:(max_clusters-1)){
    improvement[i] <- results_ratio[i+1]/results_ratio[i]-1
    if(improvement[i] < 0.01){
      break
    } 
  }
  stop_cluster <- length(improvement)
  final_cluster <- results[[stop_cluster]]

  km_transform <- data.frame(before=names(final_cluster$cluster),after=final_cluster$cluster,row.names=NULL)
  return(collapse_var=list(before=km_transform$before,after=km_transform$after))
}

variables <- c("funder","installer","lga")
variable_dicts <- lapply(variables,function(x) collapse_dict(data,max_clusters=32,collapse_var=x,target="status_group"))
names(variable_dicts) <- variables

apply_variable_dicts <- function(data,var_name,variable_dict){
  levels(data[[var_name]]) <- variable_dict[[var_name]]$after
  return(data[[var_name]])
}

data[variables] <- lapply(variables,function(x) apply_variable_dicts(data,x,variable_dicts))

write.csv(data,file="train_collapsed.csv")
