
#Dataset 
#https://www.kaggle.com/datasets/anth7310/mental-health-in-the-tech-industry

library(RSQLite)
library(tidyverse)

# Connect to db 
con <- dbConnect(drv = SQLite(), dbname="Downloads/mental_health.sqlite")

#list tables 
tables <- dbListTables(con)

#create a list data frame for table 
lDataFrames <- map(tables, ~{ dbGetQuery(conn = con, 
                                                statement = paste("SELECT * FROM  '", .x, "'", sep = "", id="a"))
  })

#Disconnect from db
dbDisconnect(con)

# Select question database
questions <- lDataFrames[[2]]
questions  <- questions[c("questionid", "questiontext")]

#left join questions and answer & survey dataset
mental_health_survey <- questions %>% 
  inner_join(lDataFrames[[1]], by=c("questionid" = "QuestionID")) %>% 
  inner_join(lDataFrames[[3]], by="SurveyID")

# EDA 

#view dataset 
glimpse(mental_health_survey)

##What we know:
##Around 240k rows and 6 columns, types of each column 
##Possible to change Desc to a factor variable 

#Check the values in the dataset

#FUNCTION
profile_dataset <- function(data){
  dataset <- data.frame(
    n_na = sapply(data , function(x) sum(is.na(x))),
    p_na = round(100 * sapply(data , function(x) sum(is.na(x)))/
                   nrow(data ),2),
    n_zeros = sapply(data , function(x) sum(x==0, na.rm=T)),
    p_zeros = round(100 * sapply(data , function(x) 
      sum(x==0, na.rm=T))/nrow(data),2),
    type = sapply(data , class),
    n_distinct = sapply(data, function(x) n_distinct(x)) 
  )
  
  dataset$variable <- rownames(dataset)
  rownames(dataset) <- NULL
  dataset <- dataset[, c(7,1, 2, 3, 4, 5, 6 )]
  
  return (dataset)
}


data_profile <- profile_dataset(mental_health_survey)

## From assessing the data we can make Description into a Factor 

data_profile

