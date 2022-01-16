# Description: Analysis of the current state of the Australian tech labour market
#              and predicting the future trend.
# Part A: Import Data
# Part B: Data Preparation and Wrangling
# Part C: Initial Analysis
# Part D: Data Visualization
# Part E: Exploratory Data Analysis, Data-Driven Modeling
# Part F: Evaluate Results
# Part G: Forecast trend

##################### Part A #####################
# install packages if it is not installed
list.of.packages <- c("tidyverse", "stringr", "reshape2", "Metrics", "scales", "lubridate", 
                      "RSocrata", "zoo", "xts", "httr", "xlsx", "readxl", "sjmisc", "dplyr", 
                      "rpart", "rattle", "randomForest", "gsubfn", "e1071")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages installed
lapply(list.of.packages, library, character.only = TRUE)

# Import list of time series file names
data_ts_list = list.files(path='data/', pattern="EQ", full.names=TRUE)

# Load and ttrim names of data frames
data_name_list <- NULL #initialise list name for list of data frames
data_name_list_trimed <- NULL
for (i in 1:length(data_ts_list)){ # extract titles of each data frames and trim the strings into more readable form
  data_name_list[i] <- gsub( ".*Employed (.+) job .*", "\\1", 
                             as.character(colnames(
                               read_excel(
                                 data_ts_list[i], sheet = 3,range = cell_rows(2:2)))
                               )
                             ) 
  data_name_list_trimed[i] <- gsub(" ", "_", paste0("Employed ", data_name_list[i], " job"))
}

# Import time series files and store as tibble
for (i in 1:length(data_ts_list)){
  data_imported <- as_tibble(read_excel(data_ts_list[i], 
                                        skip = 3, 
                                        sheet = 3
                                        ), trimws("both"))
  names(data_imported)[1] <- "Date"
  names(data_imported)[grepl('v', names(data_imported))] <- "Occupation"
  names(data_imported) <- gsub(" ", "_", names(data_imported))
  data_imported$Occupation <- gsub("[[:digit:]]", "", data_imported$Occupation)
  assign(paste(data_name_list_trimed[i]), data_imported) # rename each data frame imported to start with data_
}

##################### Part B #####################
# Merge data frames into a master data frame
master_df <- get(data_name_list_trimed[1]) #initialise empty tibble data
for (i in 2:length(data_name_list_trimed)){ #merge every tibble data imported; data
  master_df <- merge(master_df, get(data_name_list_trimed[2]), 
                     by = c("Date", "Occupation"), all.x = TRUE)
}

# Remove columns where all values are NA
master_df <- Filter(function(x)!all(is.na(x)), master_df)

# Remove columns where NA values are more than 90% of records
master_df <- master_df[, which(colMeans(!is.na(master_df)) >= 0.1)] # filter the columns with more than 10% of NA values

# Replace the remaining NAs with zero, if exist
nums <- unlist(lapply(master_df, is.numeric)) # select only numeric columns
for(i in 1:ncol(master_df[, nums])){
  return_numeric <- as.numeric(as.vector(as.numeric(unlist(master_df[, nums][, i])))) # converts imported list into the numeric value
  master_df[, nums][is.na(master_df[, nums][,i]), i]<- 0 # replace missing values with Zero
  }

# Remove unnecessary characters from col names
names(master_df) <- sub("\\.x", "", names(master_df))

# Encode date column into Date format
master_df$Date <- as.Date(master_df$Date, format = "%Y-%m-%d")

# Change column names in a data frame
# format: "new column name to be assigned" = "previous column name"
master_df <- master_df %>% 
  rename(
    Area = `Greater_capital_city_and_rest_of_state_(GCCSA):_ASGS_(2011)`,
    Employed_full_time = `Employed_full-time_('000)`,
    Employed_part_time = `Employed_part-time_('000)`,
    Number_of_hours_worked_full_time = `Number_of_hours_actually_worked_in_all_jobs_(employed_full-time)_('000_Hours)`,
    Number_of_hours_worked_part_time = `Number_of_hours_actually_worked_in_all_jobs_(employed_part-time)_('000_Hours)`
    )

# Derive a column from the master data frame to indicate relevance to tech sector
tech_value <- c("ICT", "Info", "Tech", "Engin", "IT", "Sci", "Math", "Mechanic", "Network") # Keywords used to filer tech sector jobs
master_df$Tech <- grepl(paste(tech_value, collapse="|"),master_df$Occupation, ignore.case = TRUE)

# Print occupation types specified as being relevant to tech sector
occupation_tech <- unique(grep(paste(tech_value, collapse="|"),
                               master_df$Occupation, value=TRUE, ignore.case = TRUE))

# Add column into master data frame indicating combined number of jobs and worked hour
master_df <- master_df %>%
  group_by(Date) %>%
  mutate(Employed_total = Employed_full_time + Employed_part_time,
         Number_of_hours_worked_avg = (Number_of_hours_worked_full_time + Number_of_hours_worked_part_time)/2)

# check for problems
assertthat::assert_that(nrow(problems(master_df)) == 0, # assert that there is NO problems
                        msg="There is still problem/s, which you need to fix first")

# print data frame dimensions
cat("data dimensions are: ", dim(master_df))

# Export the completed records
write_csv(master_df, "data/master_df_all.csv")

# Derive a data frame only containing occupation types relevant to tech sector
tech_df <- master_df %>% filter(Tech == TRUE)

# Export the completed records
write_csv(tech_df, "data/master_df_tech.csv")

##################### Part C #####################
# Generate data frame arranged by area and occupation
mdata <- melt(master_df, id=c("Date", "Occupation", "Sex", "Area", "Tech"))
df_by_occupation_area <- dcast(mdata, Date + Occupation + Area + Tech ~ variable, mean)

# Generate data frame arranged by occupaiton and gender
df_by_occupation_gender <- dcast(mdata, Date + Occupation + Tech + Sex ~ variable, mean)

# Generate data frame arranged by occupation
df_by_occupation <- dcast(mdata, Date + Occupation + Tech ~ variable, mean)

# Generate newly arranged data frame only containing occupation types relevant to tech sector
mdata_tech <- master_df %>% 
  filter(Tech == TRUE) %>% 
  # mutate(Employed_total_tech = Employed_full_time + Employed_part_time,
  #        Number_of_hours_worked_avg_tech = (Number_of_hours_worked_full_time + Number_of_hours_worked_part_time)/2) %>% 
  melt(id=c("Date", "Occupation", "Sex", "Area", "Tech"))
df_tech <- dcast(mdata_tech, Date + Sex + Area ~ variable, mean)

# Generate tech sector data frame arranged by gender
df_tech_by_area <- dcast(mdata_tech, Date + Sex ~ variable, mean)

# Generate tech sector data frame arranged by area
df_tech_by_gender <- dcast(mdata_tech, Date + Area ~ variable, mean)

# Generate tech sector data total
df_tech_total <- dcast(mdata_tech, Date ~ variable, mean)

# Generate newly rarranged data frame with occupation types other than tech sector
mdata_others <- master_df %>% 
  filter(Tech == FALSE) %>% 
  # mutate(Employed_total_others = Employed_full_time + Employed_part_time,
  #        Number_of_hours_worked_avg_others = (Number_of_hours_worked_full_time + Number_of_hours_worked_part_time)/2) %>% 
  melt(id=c("Date", "Occupation", "Sex", "Area", "Tech"))
df_others <- dcast(mdata_others, Date + Sex + Area ~ variable, mean)

# Generate data frame arranged by gender
df_others_by_area <- dcast(mdata_others, Date + Sex ~ variable, mean)

# Generate data frame arranged by area
df_others_by_gender <- dcast(mdata_others, Date + Area ~ variable, mean)

# Generate data frame total
df_others_total <- dcast(mdata_others, Date ~ variable, mean)

# Print the date with the highest volume tech sector jobs
Highest_job_date_tech <- df_tech_total[which(df_tech_total$Employed_total == max(df_tech_total$Employed_total)), ] # a row with the highest emplyed value were extracted
Highest_job_date_tech$Date

# Print the date with the highest volume in jobs excluding tech sector
Highest_job_date_others <- df_others_total[which(df_others_total$Employed_total == max(df_others_total$Employed_total)), ] # a row with the highest emplyed value were extracted
Highest_job_date_others$Date

# Print the date with highest worked hours in tech sector
Highest_wokred_hour_tech <- df_tech_total[which(df_tech_total$Number_of_hours_worked_avg == max(df_tech_total$Number_of_hours_worked_avg)), ] # a row with the highest emplyed value were extracted
Highest_wokred_hour_tech$Date

# Print the date with highest worked hours in jobs excluding tech sector
Highest_wokred_hour_others <- df_others_total[which(df_others_total$Number_of_hours_worked_avg == max(df_others_total$Number_of_hours_worked_avg)), ] # a row with the highest emplyed value were extracted
Highest_wokred_hour_others$Date

# Generate data frame comparing job volume development between tech sector and others
df_volume_job_tech_others <- data.frame(Date = df_tech_total$Date,
                                        Emplyed_tech = df_tech_total$Employed_total,
                                        Emplyed_others = df_others_total$Employed_total
)

# Generate data frame comparing number of hour worked between tech sector and others
df_worked_hour_tech_others <- data.frame(Date = df_tech_total$Date,
                                        Number_of_hours_worked_tech = df_tech_total$Number_of_hours_worked_avg,
                                        Number_of_hours_worked_others = df_others_total$Number_of_hours_worked_avg
)

# Generate data frame comparing job volumes in each occupation
mdata_others_by_occupation <- df_by_occupation %>% 
  filter(Tech == FALSE) %>% 
  group_by(Date) %>% 
  select(-Tech, -Employed_full_time, -Employed_part_time, -Number_of_hours_worked_full_time, 
         -Number_of_hours_worked_part_time)
mdata_others_volume_by_occupation <- melt(mdata_others_by_occupation, id=c("Date", "Occupation"))
mdata_others_colume_by_occupation <- dcast(mdata_others_volume_by_occupation, Date ~ Occupation, mean)
names(mdata_others_colume_by_occupation) <- gsub(" ", "_", names(mdata_others_colume_by_occupation))
df_job_volume_development_tech_others <- data.frame(Date = df_volume_job_tech_others$Date,
                                                    Employed_tech = df_volume_job_tech_others$Emplyed_tech
                                                    )
df_job_volume_development_tech_others <- merge(df_job_volume_development_tech_others, 
                                               mdata_others_colume_by_occupation)
names(df_job_volume_development_tech_others) <- gsubfn(".", list(" " = "_", "," = "_"), 
                                                       names(df_job_volume_development_tech_others))

##################### Part D #####################
#print summary of data frame generated in the previous step
head(df_volume_job_tech_others)
summary(df_volume_job_tech_others)
df_volume_job_tech_others %>% tail
df_volume_job_tech_others %>% head
min(df_volume_job_tech_others[, 1], na.rm=T)
max(df_volume_job_tech_others[, 1], na.rm=T)

# Generate a graph showing development of volume of jobs in tech sector compared to others
cols <- c("Tech_sector"="darkblue","Others_employed"="brown3") # map the color in order to generate legend
graph_employement_tech_others <-  df_volume_job_tech_others %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Emplyed_tech, color = "Tech_sector_employed")) +
  geom_line(aes(y = Emplyed_others, color = "Others_employed")) +
  scale_colour_manual(name="Lines",values=cols) +
  ggtitle("Employment Volume Tech Sector vs. Others") +
  scale_x_date(date_breaks = "5 year", 
               labels=date_format("%Y")) +
  ggtitle("Employment Volume Tech Sector vs. Others") +
  ylab("Employment Volume ('000)")
print(graph_employement_tech_others)

# limit x axis to recent 10 years
cols <- c("Tech_sector"="darkblue","Others_employed"="brown3") # map the color in order to generate legend
graph_employement_tech_others_xlimited <-  df_volume_job_tech_others %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Emplyed_tech, color = "Tech_sector_employed")) +
  geom_line(aes(y = Emplyed_others, color = "Others_employed")) +
  scale_colour_manual(name="Lines",values=cols) +
  scale_x_date(date_breaks = "1 year", 
               labels=date_format("%Y"),
               limits = as.Date(c(max(df_volume_job_tech_others$Date) - years(10),
                                  max(df_volume_job_tech_others$Date)))) +
  ggtitle("Employment Volume Tech Sector vs. Others (recent 10 years)") +
  ylab("Employment Volume ('000)")
print(graph_employement_tech_others_xlimited)

##################### Part E #####################
# For reproducibility
set.seed(123)

# randomly select 70% of the number of observations
index <- sample(1:nrow(df_job_volume_development_tech_others),
                size = 0.7*nrow(df_job_volume_development_tech_others))

# subset other variables from the train data frame to include only the elements in the index
train <- df_job_volume_development_tech_others[index,] 

# subset other variables from the test data frame to include only the elements in the index
test <- df_job_volume_development_tech_others [-index,] 

nrow(train)
nrow(test)

# Create a data frame with train and test indicator
group <- rep(NA,nrow(train)+nrow(test))
group <- ifelse(seq(1,nrow(train)+nrow(test)) %in% index,"Train","Test")
df <- data.frame(Date=df_job_volume_development_tech_others$Date,
                 Employed_tech=df_job_volume_development_tech_others$Employed_tech,group)

# Plot test vs. train data frames
ggplot(df,aes(x = Date,y = Employed_tech, color = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")

# Baseline model - predict the mean of the training data
best.guess <- mean(train$Employed_tech)

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- rmse(test$Employed_tech, best.guess)
RMSE.baseline

MAE.baseline <- mae(test$Employed_tech, best.guess)
MAE.baseline

#Multiple linear regression
# Create a multiple (log)linear regression model using the training data
lin.reg <- lm(log(Employed_tech+1) ~ ., data = train)

# Inspect the model
summary(lin.reg)

# Multiplicative effect of "Mining" variable
exp(lin.reg$coefficients["Mining"])

# Apply the model to the testing data
test.pred.lin <- exp(predict(lin.reg,test))-1

# Evaluate the accuracy
RMSE.lin.reg <- rmse(test$Employed_tech, test.pred.lin)
RMSE.lin.reg

MAE.lin.reg <- mae(test$Employed_tech, test.pred.lin)
MAE.lin.reg

#Decision Tree
# rpart function applied to a numeric variable
rt <- rpart(Employed_tech ~ ., data=train)

fancyRpartPlot(rt)

# Predict and evaluate on the test set
test.pred.rtree <- predict(rt,test)

RMSE.rtree <- rmse(test$Employed_tech, test.pred.rtree)
RMSE.rtree

MAE.rtree <- mae(test$Employed_tech, test.pred.rtree)
MAE.rtree

# Cross-validation results (xerror)
printcp(rt)

# Compute optimal CP
min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
min.xerror

# Prune the tree
rt.pruned <- prune(rt,cp = min.xerror)

# Plot pruned tree
fancyRpartPlot(rt.pruned)

# Evaluate pruned tree on the test set
test.pred.rtree.p <- predict(rt.pruned,test)
RMSE.rtree.pruned <- rmse(test$Employed_tech, test.pred.rtree.p)
RMSE.rtree.pruned

MAE.rtree.pruned <- mae(test$Employed_tech, test.pred.rtree.p)
MAE.rtree.pruned

# Random forests
set.seed(123)

# Create a model with 1000 trees
rf <- randomForest(Employed_tech ~ ., data = train, importance = TRUE, ntree=1000)

importance(rf)
varImpPlot(rf)

# Compute optimal number of trees 
which.min(rf$mse)

# Calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

# Predict and evaluate on the test set
test.pred.forest <- predict(rf,test)
RMSE.forest <- rmse(test$Employed_tech, test.pred.forest)
RMSE.forest

MAE.forest <- mae(test$Employed_tech, test.pred.forest)
MAE.forest

# SVM
# trainx <- train[ , purrr::map_lgl(train, is.numeric)] # leave only numeric varialbes
# Train SVM model using train set
svm.model <- svm(Employed_tech ~., scale = T,
                 data=train,kernel="radial",cost=100,gamma=0.1);
# testx <- test[ , purrr::map_lgl(test, is.numeric)]

# Predict and evaluate on the test set
svm.pred <- predict(svm.model, test,decision.values =TRUE);
summary(svm.model)

RMSE.svm <- rmse(test$Employed_tech, svm.pred)
MAE.svm <- mae(test$Employed_tech, svm.pred)

##################### Part F #####################
#Evaluate Results
# Create a data frame for error metrics of each method
accuracy <- data.frame(Method = c("Baseline","Linear Regression","Full tree","Pruned tree",
                                  "Random forest", "Support Vector"),
                       RMSE   = c(RMSE.baseline,RMSE.lin.reg,
                                  RMSE.rtree,RMSE.rtree.pruned,RMSE.forest,RMSE.svm),
                       MAE    = c(MAE.baseline,MAE.lin.reg,MAE.rtree,
                                  MAE.rtree.pruned,MAE.forest,MAE.svm)) 

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 

accuracy

#Print prediction
# Create a data frame with the predictions for each method
all.predictions <- data.frame(actual = test$Employed_tech,
                              baseline = best.guess,
                              linear.regression = test.pred.lin,
                              full.tree = test.pred.rtree,
                              pruned.tree = test.pred.rtree.p,
                              random.forest = test.pred.forest,
                              support.vector = svm.pred)

# First 10 observations of predictions
head(all.predictions, 10)

# Convert the data frame in longer format
all.predictions <- gather(all.predictions,key = model,value = predictions,2:6)

# Plot predicted vs. actual for each model
ggplot(data = all.predictions,aes(x = actual, y = predictions)) + 
  geom_point(colour = "blue") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2) + 
  coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
  ggtitle("Predicted vs. Actual, by model")

# Plot Predicted vs. actual by Random Forest model
random.forest.prediction <- data.frame(actual = test$Employed_tech,
                                       prediction = test.pred.forest)
random.forest.prediction
random.forest.prediction$index <- index(random.forest.prediction)
random.forest.prediction %>%
  ggplot(aes(x=index)) +
  geom_line(aes(y = actual, colour="actual")) +
  geom_line(aes(y = prediction, colour= "prediction")) +
  ylab("Employment Volume ('000)") +
  xlab("Index") +
  ggtitle(paste("Predicted vs. Actual by Random Forest"))

##################### Part G #####################
#set length of forecast (quaters)
duration <- 4

# Create a data frame used for forecasting
df <- df_job_volume_development_tech_others
df$Date <- as.yearmon(df$Date, "%Y-%m")

# Rearrange data frame depending on the prediction length
data_modifi <- df[,-which(names(df) == "Employed_tech")]
data_modifi$Date <- as.yearmon(data_modifi$Date) + (3/12)*duration
data_volume <- data.frame(Date = df$Date,
                          volume_employed = df[,which(names(df) == "Employed_tech")])
data <- merge(data_modifi, data_volume, all = TRUE)

# Drop dates out of range
data <- tail(data, -duration)

# leave only train data range
train <- data[as.yearmon(data$Date)<=(as.yearmon(tail(data$Date,1)) - (3/12)*duration),]

# subset elements in the index
test <- data[as.yearmon(data$Date)>(as.yearmon(tail(data$Date,1)) - (3/12)*duration),]

nrow(train)
nrow(test)

# Generate predictions using Random Forest
rf <- randomForest(volume_employed ~., data = train, importance = TRUE, ntree=1000)

# Compute optimal number of trees
ntree <- which.min(rf$mse)

# Regenerate predictions using optimal number of trees
rf <- randomForest(volume_employed ~., data = train, importance = TRUE, ntree=ntree)
test.pred <- predict(rf,test[,-which(names(test) == "volume_employed")])
test.pred

# Covert data frame into time series format
train.xts <- xts(train, order.by = as.Date(train$Date))
test.xts <- xts(test, order.by = as.Date(test$Date))

# Create a data frame indicating date range
pred2 <- rbind(train.xts[, "volume_employed"], test.xts[, "volume_employed"])

# Add corresponding date variables to predicted data
volume_employed_pred_xts <- xts(test.pred, order.by = as.Date(test$Date))
names(volume_employed_pred_xts) <- "volume_employed_pred"
volume_employed_pred_xts

# Create an data frame used for computing slope of change
df_slope <- data.frame(Date = index(volume_employed_pred_xts),
                       pred = volume_employed_pred_xts$volume_employed_pred)

# Compute slope of change
lm(df_slope$volume_employed_pred ~ index(df_slope))

# Compute average percentage change
APC <- ((df_slope[nrow(df_slope),2] - df_slope[1,2]) / df_slope[1,2]) * 100
APC

# Comupte average percentage change per each quarter
APC_quarters <- APC/duration
APC_quarters

# Merge predicted data with the original data
df_volume_pred_prev <- merge(pred2, volume_employed_pred_xts)
df_volume_pred_prev$volume_employed_pred[nrow(df_volume_pred_prev$volume_employed_pred)-duration] <- 
  df_volume_pred_prev$volume_employed[nrow(df_volume_pred_prev$volume_employed_pred)-duration]

# Plot predicted values with the original data
df <- df_volume_pred_prev
graph_pred_prev <- plot(df[as.Date(as.yearmon(index(df)))>(as.Date(tail(as.yearmon(index(df)),1)-(3/12)*duration*5)), # limit the rows
                           c("volume_employed", "volume_employed_pred")], 
                     major.ticks = "months", 
                     grid.ticks.on = "months",
                     grid.ticks.lty = 3,
                     main = paste(duration,  "Quaters Forecast of Tech Sector Job Volume", sep=" "),
                     col = c("black", "blue"),
                     ylab = "Employment Volume in Tech Sector ('000)")
graph_pred_prev