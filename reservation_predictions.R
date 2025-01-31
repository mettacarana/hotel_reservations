
#-----------------------------------------------------------------------
data = read.csv('Hotel Reservations.csv')

# 1. DATA PREPARATION
# 1A. Data Cleaning----------------------------------------------------
library(skimr)

summary(data)
skim(data)
str(data)

#check missing 
colSums(is.na(data))

#check duplicates
duplicated_rows <- duplicated(data)
data[duplicated_rows, ]

#data has no missing & duplicates

# 1B. Data Transformation----------------------------------------------------

#Booking_ID column not needed, erase
data2 = subset(data, select = -Booking_ID)

#convert target var to true/false
names(data2)[names(data2) == "booking_status"] = "booking_canceled"
data2$booking_canceled <- ifelse(data2$booking_canceled == "Canceled", TRUE, FALSE)
head(data2)

summary(as.factor(data$type_of_meal_plan))
summary(as.factor(data$room_type_reserved))
summary(data4$type_of_meal_plan)
summary(data4$room_type_reserved)
summary(data4$market_segment_type)
#binary vars : required_car_parking_space , repeated_guest >> no transform

#character vars: type_of_meal_plan , room_type_reserved >> convert to int
#room_type_reserved (0-3)
data2$room_type_reserved = gsub("Room_Type ", "", data2$room_type_reserved) 
data2$room_type_reserved = as.integer(data2$room_type_reserved)
print(typeof(data2$room_type_reserved))

#convert type_of_meal_plan to integer (0-7)
data2$type_of_meal_plan = gsub("Not Selected", 0, data2$type_of_meal_plan) 
data2$type_of_meal_plan = gsub("Meal Plan ", "", data2$type_of_meal_plan)
data2$type_of_meal_plan = as.integer(data2$type_of_meal_plan)
print(typeof(data2$type_of_meal_plan))

#convert market_segment_type to factors
data2$market_segment_type <- as.numeric(as.factor(data2$market_segment_type))
head(data2["market_segment_type"])
table(data2$market_segment_type)
#1: Aviation 2:complementary  3:corporate 4: offline, 5: online

#date format
data3 <- data.frame(
  data2[, 1:11],
  date = as.Date(paste(data2$arrival_date, 
                       data2$arrival_month, 
                       data2$arrival_year, sep="-"), 
                 format = "%d-%m-%Y"),
  data2[, 12:ncol(data2)]
)
#check missing date
summary(data3$date)
data3 = data3[complete.cases(data3$date), ]

summary(data3)
#Convert target var to factor
data3$booking_canceled <- as.numeric(data3$booking_canceled)

#create another dataset where multilevel variables are treated as factors
data4 = data3

#multilevel factors to var
data4$type_of_meal_plan <- as.factor(data4$type_of_meal_plan)
data4$room_type_reserved <- as.factor(data4$room_type_reserved)
data4$market_segment_type <- as.factor(data4$market_segment_type)
data4$required_car_parking_space <- as.factor(data4$required_car_parking_space)
data4$repeated_guest <- as.factor(data4$repeated_guest)
data4$booking_canceled <- factor(data4$booking_canceled)
str(data4)

# 2. EDA-------------------------------------------------------------------------

palette = c('#C06AFF', '#F6AE0A' , '#0EECE5', '#6CFE4B', '#FF7ACD', '#9B8E96', '#A39DF5')

# 2a. Target var pie chart

pie = ggplot(data3, aes(x = "", fill = booking_canceled)) + geom_bar(width = 1) + 
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "booking_canceled", ncol = 1)) +
  geom_text(aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) * 100, 1), "%")),
            stat = "count", 
            position = position_stack(vjust = 0.5))+
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = palette)

bar = ggplot(data3, aes(x = booking_canceled, fill = booking_canceled)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  guides(fill = "none") +
  scale_fill_manual(values = palette) +
  coord_flip()
# 2b. categorical vars bar chart / pie chart

#multi
mealplan <- ggplot(data4, aes(x = type_of_meal_plan, fill = type_of_meal_plan)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  guides(fill = "none") +
  scale_fill_manual(values = palette)

roomtype <- ggplot(data4, aes(x = room_type_reserved, fill = room_type_reserved)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  guides(fill = "none") +
  scale_fill_manual(values = palette)

market <- ggplot(data4, aes(x = market_segment_type, fill = market_segment_type)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  guides(fill = "none") +
  scale_fill_manual(values = palette)

#binary
parkspace <- ggplot(data4, aes(x = required_car_parking_space, fill = required_car_parking_space)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  guides(fill = "none") +
  scale_fill_manual(values = palette)

repeate <- ggplot(data4, aes(x = repeated_guest, fill = repeated_guest)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  guides(fill = "none") +
  scale_fill_manual(values = palette)

# 2c. Correlation matrix

data_corr = data3[, -which(names(data3) == "date")]

corr_matrix = round(cor(data_corr), 2)
library(reshape2)
meltcorrmat = melt(corr_matrix)

ggplot(data = meltcorrmat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = '#F65D06', high = '#0693EE',
                       limit = c(-1,1), name="Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(Var2, Var1, label = value),size = 2) +
  labs(x = NULL, y = NULL)

# 2d. lead time to target var

ggplot(data4, aes(x = lead_time, fill = booking_canceled, group = booking_canceled)) +
  geom_density(alpha = 0.5, aes(y = after_stat(count))) +
  labs(x = "Lead Time", y = "Count of Bookings", fill = "booking_canceled") +
  scale_fill_manual(values = palette)

# 2e. price to target var
ggplot(data4, aes(x = avg_price_per_room, fill = booking_canceled, group = booking_canceled)) +
  geom_density(alpha = 0.5, aes(y = after_stat(count))) +
  labs(x = "Avg Room Prices", y = "Count of Bookings", fill = "booking_canceled") +
  scale_fill_manual(values = palette)

# 2f. price and lead time

ggplot(data4, aes(x = avg_price_per_room, y = lead_time)) +
  geom_point(colour = "#A32AFC", size = 0.72) +
  labs(x = "Avg Room Price", y = "Lead Time")

# 2g. num of child & adult

ggplot(data4, aes(x = as.factor(no_of_children), fill = as.factor(no_of_children))) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  labs(x = "No of Children", y = "Count") +
  guides(fill = "none") +
  scale_fill_manual(values = palette) +
  coord_flip()

ggplot(data4, aes(x = as.factor(no_of_adults), fill = as.factor(no_of_adults))) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  labs(x = "No of Adults", y = "Count") +
  guides(fill = "none") +
  scale_fill_manual(values = palette) +
  coord_flip()

# 2h. num of bookings by month
ggplot(data4, aes(x = as.factor(arrival_month), fill = as.factor(arrival_month))) + 
  geom_line() + 
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.1) +
  labs(x = "Arrival month", y = "Count") +
  guides(fill = "none") +
  scale_fill_manual(values = palette)

# 2i. price by month

ggplot(data4, aes(x = date, y = avg_price_per_room)) +
  geom_point(size = 0.5) +
  geom_smooth(method="auto", color = "#0EECE5") +
  labs(x = "Month", y = "Average Price per Room")

# 3. TRAINING---------------------------------------------------------------------
#    done on data4

#split data
library(caret)
set.seed(77)
for_training <- createDataPartition(data4$booking_canceled, p = 0.7, list = FALSE)
train_set <- data4[for_training, ]
test_set <- data4[-for_training, ]


#Random Forest Model
library(randomForest)

mod_rf = randomForest(booking_canceled ~ ., data = train_set)
rf_predict = predict(mod_rf, newdata = test_set)

confusionMatrix(rf_predict, test_set$booking_canceled)

#Decision Tree Model
library(rpart)
library(rpart.plot)
mod_dt = rpart(booking_canceled ~ ., data = train_set, method = "class")
dt_predict = predict(mod_dt, newdata = test_set, type = "class")

# class method & type - model for classification
confusionMatrix(dt_predict, test_set$booking_canceled)


#Support Vector Machine
library(e1071)
mod_svm = svm(booking_canceled ~ ., data = train_set)
svm_predict = predict(mod_svm, newdata = test_set)

confusionMatrix(svm_predict, test_set$booking_canceled)

#USE SVM 83.4%, DT 82.76%, RF 90.24%

#4. MODEL VALIDATION-------------------------------------------------------------
dt_accuracy <- confusionMatrix(dt_predict, test_set$booking_canceled)$overall['Accuracy']
rf_accuracy <- confusionMatrix(rf_predict, test_set$booking_canceled)$overall['Accuracy']
svm_accuracy <- confusionMatrix(svm_predict, test_set$booking_canceled)$overall['Accuracy']

dt_TestConfM <- confusionMatrix(dt_predict, test_set$booking_canceled)
rf_TestConfM <- confusionMatrix(rf_predict, test_set$booking_canceled)
svm_TestConfM <- confusionMatrix(svm_predict, test_set$booking_canceled)

dtf1 <- dt_TestConfM$byClass["F1"]
rff1 <- rf_TestConfM$byClass["F1"]
svmf1 <- svm_TestConfM$byClass["F1"]

dtprec <- dt_TestConfM$byClass["Pos Pred Value"]
rfprec <- rf_TestConfM$byClass["Pos Pred Value"]
svmprec <- svm_TestConfM$byClass["Pos Pred Value"]

dtrecall <- dt_TestConfM$byClass["Sensitivity"]
rfrecall <- rf_TestConfM$byClass["Sensitivity"]
svmrecall <- svm_TestConfM$byClass["Sensitivity"]

dtBAcc <- dt_TestConfM$byClass["Balanced Accuracy"]
rfBAcc <- rf_TestConfM$byClass["Balanced Accuracy"]
svmBAcc <- svm_TestConfM$byClass["Balanced Accuracy"]

acc_summary <- data.frame(Model = c("RF", "SVM", "DT"),
                          Accuracy = c(rf_accuracy, svm_accuracy, dt_accuracy))
score_summary <- data.frame(Model = c("RF", "SVM", "DT"),
                            Accuracy = c(rf_accuracy, svm_accuracy, dt_accuracy),
                            F1 = c(rff1, svmf1, dtf1),
                            Precision = c(rfprec, svmprec, dtprec),
                            Recall = c(rfrecall, svmrecall, dtrecall),
                            BalancedAcc = c(rfBAcc, svmBAcc, dtBAcc))

#bar representation
Accplot = ggplot(acc_summary, aes(x = Accuracy, y = reorder(Model, Accuracy))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Accuracy", y = "Model", title = "Model Accuracy - Unbalanced 70/30") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Accuracy * 100, digits = 1), "%")))



#5. IF DATA IS BALANCED-------------------------------------------------------

data5 = subset(data3,select = -date)
str(data5)
data5$arrival_month <- as.numeric(data5$arrival_month)
data5$booking_canceled <- as.numeric(data5$booking_canceled)
table(data5$booking_canceled)
#false = 1, true = 2

library(lattice)
library(grid)
library(UBL)

data_SMOTE = SmoteClassif(data5$booking_canceled ~ ., data5, C.perc = "balance")
data_SMOTE = data_SMOTE[complete.cases(data_SMOTE$booking_canceled), ]
table(data_SMOTE$booking_canceled)
prop.table(table(data_SMOTE$booking_canceled)) 

#date edit
data6 <- data.frame(
  data_SMOTE[, 1:11],
  date = as.Date(paste(data_SMOTE$arrival_date, 
                       data_SMOTE$arrival_month, 
                       data_SMOTE$arrival_year, sep="-"), 
                 format = "%d-%m-%Y"),
  data_SMOTE[, 12:ncol(data_SMOTE)]
)
#check missing date
summary(data6$date)
data6 = data6[complete.cases(data6$date), ]

#convert to factors
data6$arrival_month <- as.factor(data6$arrival_month)
data6$booking_canceled <- as.factor(data6$booking_canceled)

data6$type_of_meal_plan <- as.factor(data6$type_of_meal_plan)
data6$room_type_reserved <- as.factor(data6$room_type_reserved)
data6$market_segment_type <- as.factor(data6$market_segment_type)

#convert to int
data6$no_of_children <- as.integer(data6$no_of_children)
data6$no_of_weekend_nights <- as.integer(data6$no_of_weekend_nights)
data6$no_of_week_nights <- as.integer(data6$no_of_week_nights)
#data6$required_car_parking_space <- as.integer(data6$required_car_parking_space)
data6$lead_time <- as.integer(data6$lead_time)
data6$arrival_year <- as.integer(data6$arrival_year)
data6$arrival_date <- as.integer(data6$arrival_date)
#data6$repeated_guest <- as.integer(data6$repeated_guest)
data6$no_of_previous_cancellations <- as.integer(data6$no_of_previous_cancellations)
data6$no_of_previous_bookings_not_canceled <- as.integer(data6$no_of_previous_bookings_not_canceled)
data6$no_of_special_requests <- as.integer(data6$no_of_special_requests)

# TRAINING - BALANCED DATA-------------------------------

#split data
set.seed(77)
library(caret)
for_training_bal <- createDataPartition(data6$booking_canceled, p = 0.7, list = FALSE)
train_set_balanced <- data6[for_training_bal, ]
test_set_balanced <- data6[-for_training_bal, ]

#Random Forest Model
library(randomForest)

mod_rf_bal = randomForest(booking_canceled ~ ., data = train_set_balanced)
rf_predict_bal = predict(mod_rf_bal, newdata = test_set_balanced)
confusionMatrix(rf_predict_bal, test_set_balanced$booking_canceled)

rf_trained_bal = predict(mod_rf_bal, newdata = train_set_balanced)
confusionMatrix(rf_trained_bal, train_set_balanced$booking_canceled)
# > mod_rf_bal$ntree
# [1] 500
# > mod_rf_bal$mtry
# [1] 4

#tuning

mod_rftuned_bal = randomForest(booking_canceled ~ ., data = train_set_balanced,
                               ntree = 600,
                               mtry = 6,
                               nodesize = 2,
                               importance = TRUE
                               )
rftuned_predict_bal = predict(mod_rftuned_bal, newdata = test_set_balanced)
confusionMatrix(rftuned_predict_bal, test_set_balanced$booking_canceled)

rftuned_train_bal = predict(mod_rftuned_bal, newdata = train_set_balanced)
confusionMatrix(rftuned_train_bal, train_set_balanced$booking_canceled)

#Decision Tree Model
set.seed(77)
library(rpart)
library(rpart.plot)
mod_dt_bal = rpart(booking_canceled ~ ., data = train_set_balanced, method = "class")
dt_predict_bal = predict(mod_dt_bal, newdata = test_set_balanced, type = "class")

rpart.plot(mod_dt_bal)
prp(mod_dt_bal, type = 0, fallen.leaves = TRUE)

dt_trained_bal = predict(mod_dt_bal, newdata = train_set_balanced, type = "class")
confusionMatrix(dt_trained_bal, train_set_balanced$booking_canceled)
# class method & type - model for classification
confusionMatrix(dt_predict_bal, test_set_balanced$booking_canceled)

#Tuned DT
cplist <- c(0.005, 0.01, 0.05, 0.1)
models <- list()
for (cp in cplist) {
  # Train with cp variation
  model = rpart(booking_canceled ~ ., data = train_set_balanced, method = "class", cp = cp)
  models[[as.character(cp)]] = model
}

dttunedacc = sapply(models, function(model) {
  predictions = predict(model, newdata = test_set_balanced, type = "class")
  mean(predictions == test_set_balanced$booking_canceled)
})

# cp w highest acc
best_cp <- names(dttunedacc)[which.max(dttunedacc)]
best_model <- models[[best_cp]]

#DT Tuned Final Model
mod_dttuned_bal = rpart(booking_canceled ~ ., data = train_set_balanced, method = "class", cp = "0.001")
dttuned_predict_bal = predict(mod_dttuned_bal, newdata = test_set_balanced, type = "class")
confusionMatrix(dttuned_predict_bal, test_set_balanced$booking_canceled)$byClass["F1"]

rpart.plot(mod_dttuned_bal)

dttuned_train_bal = predict(mod_dttuned_bal, newdata = train_set_balanced, type = "class")
confusionMatrix(dttuned_train_bal, train_set_balanced$booking_canceled)

rpart.plot(mod_dttuned_bal)

#SUPPORT VECTOR MACHINE
library(e1071)
mod_svm_bal = svm(booking_canceled ~ ., data = train_set_balanced)
svm_predict_bal = predict(mod_svm_bal, newdata = test_set_balanced)
confusionMatrix(svm_predict_bal, test_set_balanced$booking_canceled)

svm_trained_bal = predict(mod_svm_bal, newdata = train_set_balanced)
confusionMatrix(svm_trained_bal, train_set_balanced$booking_canceled)

set.seed(77)
#Tuned SVM

# tune w grid search
svmtuned_model = tune(svm, booking_canceled~., data=train_set_balanced,
                   ranges = list(epsilon = seq (0, 1, 0.1), cost = 2^(0:2)))
#manual
mod_svmtuned_bal <- svm(booking_canceled~., data = train_set_balanced, epsilon = 0, cost = 20)
svmtuned_predict_bal = predict(mod_svmtuned_bal, newdata = test_set_balanced)

confusionMatrix(svmtuned_predict_bal, test_set_balanced$booking_canceled)$byClass["F1"]

svmtuned_train_bal = predict(mod_svmtuned_bal, newdata = train_set_balanced)
confusionMatrix(svmtuned_train_bal, train_set_balanced$booking_canceled)
#6. FINAL COMPARISON ----------------------------------------------------------------

#70/30
dtBAL_accuracy <- confusionMatrix(dt_predict_bal, test_set_balanced$booking_canceled)$overall['Accuracy']
rfBAL_accuracy <- confusionMatrix(rf_predict_bal, test_set_balanced$booking_canceled)$overall['Accuracy']
svmBAL_accuracy <- confusionMatrix(svm_predict_bal, test_set_balanced$booking_canceled)$overall['Accuracy']

dtBAL_TestConfM <- confusionMatrix(dt_predict_bal, test_set_balanced$booking_canceled)
rfBAL_TestConfM <- confusionMatrix(rf_predict_bal, test_set_balanced$booking_canceled)
svmBAL_TestConfM <- confusionMatrix(svm_predict_bal, test_set_balanced$booking_canceled)

dtBALf1 <- dtBAL_TestConfM$byClass["F1"]
rfBALf1 <- rfBAL_TestConfM$byClass["F1"]
svmBALf1 <- svmBAL_TestConfM$byClass["F1"]

dtBALprec <- dtBAL_TestConfM$byClass["Pos Pred Value"]
rfBALprec <- rfBAL_TestConfM$byClass["Pos Pred Value"]
svmBALprec <- svmBAL_TestConfM$byClass["Pos Pred Value"]

dtBALrecall <- dtBAL_TestConfM$byClass["Sensitivity"]
rfBALrecall <- rfBAL_TestConfM$byClass["Sensitivity"]
svmBALrecall <- svmBAL_TestConfM$byClass["Sensitivity"]

dtBALBAcc <- dtBAL_TestConfM$byClass["Balanced Accuracy"]
rfBALBAcc <- rfBAL_TestConfM$byClass["Balanced Accuracy"]
svmBALBAcc <- svmBAL_TestConfM$byClass["Balanced Accuracy"]

acc_summary_bal <- data.frame(Model = c("RF Balanced", "SVM Balanced", "DT Balanced"),
                          Accuracy = c(rfBAL_accuracy, svmBAL_accuracy, dtBAL_accuracy))

score_summary_balanced <- data.frame(Model = c("RF Balanced", "SVM Balanced", "DT Balanced"),
                            Accuracy = c(rfBAL_accuracy, svmBAL_accuracy, dtBAL_accuracy),
                            F1 = c(rfBALf1, svmBALf1, dtBALf1),
                            Precision = c(rfBALprec, svmBALprec, dtBALprec),
                            Recall = c(rfBALrecall, svmBALrecall, dtBALrecall),
                            BalancedAcc = c(rfBALBAcc, svmBALBAcc, dtBALBAcc))


#bar representation
library(ggplot2)
ggplot(acc_summary_bal, aes(x = Accuracy, y = reorder(Model, Accuracy))) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Accuracy", y = "Model", title = "Model Accuracy - Balanced 70/30") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Accuracy * 100, digits = 1), "%")))


#Chosen model RF

#ROC Curve
install.packages("pROC")
library(pROC)

#RF NOT TUNED, BALANCED
rf_probs <- predict(mod_rf_bal, newdata = test_set_balanced, type = "prob")

roc_obj <- roc(test_set_balanced$booking_canceled, rf_probs[, 2])
plot(roc_obj, print.auc = TRUE, main ="ROC RF - Balanced - No Tuning")

#RF TUNED
rf_probs <- predict(mod_rftuned_bal, newdata = test_set_balanced, type = "prob")

roc_obj <- roc(test_set_balanced$booking_canceled, rf_probs[, 2])
plot(roc_obj, print.auc = TRUE, main ="ROC RF - Balanced, Tuned")

#RF UNBALANCED
rf_probs <- predict(mod_rf, newdata = test_set, type = "prob")

roc_obj <- roc(test_set$booking_canceled, rf_probs[, 2])
plot(roc_obj, print.auc = TRUE, main ="ROC RF - Unbalanced")

#DT BALANCED
dt_probs <- predict(mod_dt_bal, newdata = test_set_balanced, type = "prob")

roc_obj <- roc(test_set_balanced$booking_canceled, dt_probs[, 2])
plot(roc_obj, print.auc = TRUE, main ="ROC DT - Balanced")

#DT Balanced Tuned
dt_probs <- predict(mod_dttuned_bal, newdata = test_set_balanced, type = "prob")

roc_obj <- roc(test_set_balanced$booking_canceled, dt_probs[, 2])
plot(roc_obj, print.auc = TRUE, main ="ROC DT - Balanced, Tuned")

#DT UNBALANCED
dt_probs <- predict(mod_dt, newdata = test_set, type = "prob")

roc_obj <- roc(test_set$booking_canceled, dt_probs[, 2])
plot(roc_obj, print.auc = TRUE, main ="ROC DT - Unbalanced")
-----------------------------------
#7. TRY WITH NORMALIZED (BALANCED) DATA
-----------------------------------
  
#normalize data6
tonormal = c(1:4, 6,8, 14:18)
data7 = as.data.frame(lapply(data6[, tonormal], function(x) (x - min(x)) / (max(x) - min(x))))

data7 = cbind(data7,data6[ ,-(tonormal)] )

#split data
set.seed(77)
library(caret)
for_training_norm <- createDataPartition(data7$booking_canceled, p = 0.7, list = FALSE)
train_set_norm <- data7[for_training_norm, ]
test_set_norm <- data7[-for_training_norm, ]

#Random Forest Model
library(randomForest)

mod_rf_norm = randomForest(booking_canceled ~ ., data = train_set_norm)
rf_predict_norm = predict(mod_rf_norm, newdata = test_set_norm)

confusionMatrix(rf_predict_norm, test_set_norm$booking_canceled)

#Decision Tree Model
library(rpart)
library(rpart.plot)
mod_dt_norm = rpart(booking_canceled ~ ., data = train_set_norm, method = "class")
dt_predict_norm = predict(mod_dt_norm, newdata = test_set_norm, type = "class")

rpart.plot(mod_dt_norm)
prp(mod_dt_norm, type = 0, fallen.leaves = TRUE)
# class method & type - model for classification
confusionMatrix(dt_predict_norm, test_set_norm$booking_canceled)


#Support Vector Machine
library(e1071)
mod_svm_norm = svm(booking_canceled ~ ., data = train_set_norm)
svm_predict_norm = predict(mod_svm_norm, newdata = test_set_norm)

confusionMatrix(svm_predict_norm, test_set_norm$booking_canceled)

#confusion matrix
dtNOR_accuracy <- confusionMatrix(dt_predict_norm, test_set_norm$booking_canceled)$overall['Accuracy']
rfNOR_accuracy <- confusionMatrix(rf_predict_norm, test_set_norm$booking_canceled)$overall['Accuracy']
svmNOR_accuracy <- confusionMatrix(svm_predict_norm, test_set_norm$booking_canceled)$overall['Accuracy']

dtNOR_TestConfM <- confusionMatrix(dt_predict_norm, test_set_norm$booking_canceled)
rfNOR_TestConfM <- confusionMatrix(rf_predict_norm, test_set_norm$booking_canceled)
svmNOR_TestConfM <- confusionMatrix(svm_predict_norm, test_set_norm$booking_canceled)

dtNORf1 <- dtNOR_TestConfM$byClass["F1"]
rfNORf1 <- rfNOR_TestConfM$byClass["F1"]
svmNORf1 <- svmNOR_TestConfM$byClass["F1"]

dtNORprec <- dtNOR_TestConfM$byClass["Pos Pred Value"]
rfNORprec <- rfNOR_TestConfM$byClass["Pos Pred Value"]
svmNORprec <- svmNOR_TestConfM$byClass["Pos Pred Value"]

dtNORrecall <- dtNOR_TestConfM$byClass["Sensitivity"]
rfNORrecall <- rfNOR_TestConfM$byClass["Sensitivity"]
svmNORrecall <- svmNOR_TestConfM$byClass["Sensitivity"]

dtNORBAcc <- dtNOR_TestConfM$byClass["Balanced Accuracy"]
rfNORBAcc <- rfNOR_TestConfM$byClass["Balanced Accuracy"]
svmNORBAcc <- svmNOR_TestConfM$byClass["Balanced Accuracy"]

acc_summary_norm <- data.frame(Model = c("RF Bal + Norm", "SVM Bal + Norm", "DT Bal + Norm"),
                              Accuracy = c(rfNOR_accuracy, svmNOR_accuracy, dtNOR_accuracy))

ggplot(acc_summary_norm, aes(x = Accuracy, y = reorder(Model, Accuracy))) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(x = "Accuracy", y = "Model", title = "Model Accuracy - Bal + Norm 70/30") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(Accuracy * 100, digits = 1), "%")))


score_summary_norm <- data.frame(Model = c("RF Bal + Norm", "SVM Bal + Norm", "DT Bal + Norm"),
                                     Accuracy = c(rfNOR_accuracy, svmNOR_accuracy, dtNOR_accuracy),
                                     F1 = c(rfNORf1, svmNORf1, dtNORf1),
                                     Precision = c(rfNORprec, svmNORprec, dtNORprec),
                                     Recall = c(rfNORrecall, svmNORrecall, dtNORrecall),
                                     BalancedAcc = c(rfNORBAcc, svmNORBAcc, dtNORBAcc))
