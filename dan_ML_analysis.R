setwd("~")

# Load necessary libraries
library(pROC)
library(randomForest)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
library(caret)
library(readxl)
library(randomForest)

dan_data <- read_xlsx("0400-21-HMO for fares.xlsx")
dan_data <- as.data.frame(dan_data)

#####analysis for WT neutrulization #######
analysis_data <- dan_data %>% dplyr::select(-c(neutralizing_category_Delta, neutralizing_category_Omicron))

analysis_data$outcome <- analysis_data$neutralizing_category_WT
analysis_data <- analysis_data %>% dplyr::select(-neutralizing_category_WT)
analysis_data$outcome<-  factor(analysis_data$outcome, levels = c("0", "1"))
analysis_data <- na.omit(analysis_data)
analysis_data <- analysis_data %>% dplyr::select(-c(date))
rownames(analysis_data) <- analysis_data$sample
analysis_data <- analysis_data %>% dplyr::select(-sample)
analysis_data$group <- factor(analysis_data$group)

calculate_auc <- function(data, outcome) {
  auc_values <- NULL
  variable_importance <- list() # Store variable importance for each fold
  roc_objects <- list()
  all_predictions <- NULL  # Store predictions for all folds
  
  
  cv <- createFolds(outcome, k = 5, list = TRUE, returnTrain = FALSE)
  
  for (i in 1:length(cv)) {
    test_indices <- unlist(cv[i])
    train_data <- data[-test_indices, ]
    test_data <- data[test_indices, ]
    
    # Train the Random Forest model
    library(randomForest)  # Make sure the randomForest package is loaded
    fit <- randomForest(outcome ~., data = train_data, ntree = 500, preProcess=c("BoxCox"))
    
    # Make predictions on the test set
    predictions <- predict(fit, newdata = test_data, type = "prob")
    predictions <- data.frame(predictions)
    predictions <- predictions[,2]
    
    # Calculate AUC and store it
    roc_obj <- roc(test_data$outcome, predictions)
    auc_value <- auc(roc_obj)
    auc_values <- c(auc_values, auc_value)
    
    # Store variable importance for this fold
    variable_importance <- rbind(variable_importance, varImp(fit))
    roc_objects[[i]] <- roc_obj
    # Store predictions for this fold
    all_predictions <- c(all_predictions, predictions)
  }
  # Calculate the overall ROC curve and AUC
  overall_roc <- roc(outcome, all_predictions)
  overall_auc <- auc(overall_roc)
  
  
  return(list(mean_auc = mean(auc_values), variable_importance = variable_importance, 
              roc_objects=roc_objects, overall_roc = overall_roc))
}


# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(analysis_data), 0.7 * nrow(analysis_data))
train_data <- analysis_data[train_index, ]
test_data <- analysis_data[-train_index, ]

#train random forest 
res <- calculate_auc(train_data, train_data$outcome)
print(paste("Random Forest AUC:", res$mean_auc))
print("Mean Variable Importance:")
print(res$variable_importance)
res$roc_objects

WT_res <- res



#####analysis for Delta neutrulization #######
analysis_data <- dan_data %>% dplyr::select(-c(neutralizing_category_WT, neutralizing_category_Omicron))

analysis_data$outcome <- analysis_data$neutralizing_category_Delta
analysis_data <- analysis_data %>% dplyr::select(-neutralizing_category_Delta)
analysis_data$outcome<-  factor(analysis_data$outcome, levels = c("0", "1"))
analysis_data <- na.omit(analysis_data)
analysis_data <- analysis_data %>% dplyr::select(-c(date))
rownames(analysis_data) <- analysis_data$sample
analysis_data <- analysis_data %>% dplyr::select(-sample)
analysis_data$group <- factor(analysis_data$group)

calculate_auc <- function(data, outcome) {
  auc_values <- NULL
  variable_importance <- list() # Store variable importance for each fold
  roc_objects <- list()
  all_predictions <- NULL  # Store predictions for all folds
  
  
  cv <- createFolds(outcome, k = 5, list = TRUE, returnTrain = FALSE)
  
  for (i in 1:length(cv)) {
    test_indices <- unlist(cv[i])
    train_data <- data[-test_indices, ]
    test_data <- data[test_indices, ]
    
    # Train the Random Forest model
    library(randomForest)  # Make sure the randomForest package is loaded
    fit <- randomForest(outcome ~., data = train_data, ntree = 500, preProcess=c("BoxCox"))
    
    # Make predictions on the test set
    predictions <- predict(fit, newdata = test_data, type = "prob")
    predictions <- data.frame(predictions)
    predictions <- predictions[,2]
    
    # Calculate AUC and store it
    roc_obj <- roc(test_data$outcome, predictions)
    auc_value <- auc(roc_obj)
    auc_values <- c(auc_values, auc_value)
    
    # Store variable importance for this fold
    variable_importance <- rbind(variable_importance, varImp(fit))
    roc_objects[[i]] <- roc_obj
    # Store predictions for this fold
    all_predictions <- c(all_predictions, predictions)
  }
  # Calculate the overall ROC curve and AUC
  overall_roc <- roc(outcome, all_predictions)
  overall_auc <- auc(overall_roc)
  
  
  return(list(mean_auc = mean(auc_values), variable_importance = variable_importance, 
              roc_objects=roc_objects, overall_roc = overall_roc))
}


# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(analysis_data), 0.7 * nrow(analysis_data))
train_data <- analysis_data[train_index, ]
test_data <- analysis_data[-train_index, ]

#train random forest 
res <- calculate_auc(train_data, train_data$outcome)
print(paste("Random Forest AUC:", res$mean_auc))
print("Mean Variable Importance:")
print(res$variable_importance)
res$roc_objects

Delta_res <- res



#####analysis for Omicron neutrulization #######
analysis_data <- dan_data %>% dplyr::select(-c(neutralizing_category_WT, neutralizing_category_Delta))

analysis_data$outcome <- analysis_data$neutralizing_category_Omicron
analysis_data <- analysis_data %>% dplyr::select(-neutralizing_category_Omicron)
analysis_data$outcome<-  factor(analysis_data$outcome, levels = c("0", "1"))
analysis_data <- na.omit(analysis_data)
analysis_data <- analysis_data %>% dplyr::select(-c(date))
rownames(analysis_data) <- analysis_data$sample
analysis_data <- analysis_data %>% dplyr::select(-sample)
analysis_data$group <- factor(analysis_data$group)

calculate_auc <- function(data, outcome) {
  auc_values <- NULL
  variable_importance <- list() # Store variable importance for each fold
  roc_objects <- list()
  all_predictions <- NULL  # Store predictions for all folds
  
  
  cv <- createFolds(outcome, k = 5, list = TRUE, returnTrain = FALSE)
  
  for (i in 1:length(cv)) {
    test_indices <- unlist(cv[i])
    train_data <- data[-test_indices, ]
    test_data <- data[test_indices, ]
    
    # Train the Random Forest model
    library(randomForest)  # Make sure the randomForest package is loaded
    fit <- randomForest(outcome ~., data = train_data, ntree = 500, preProcess=c("BoxCox"))
    
    # Make predictions on the test set
    predictions <- predict(fit, newdata = test_data, type = "prob")
    predictions <- data.frame(predictions)
    predictions <- predictions[,2]
    
    # Calculate AUC and store it
    roc_obj <- roc(test_data$outcome, predictions)
    auc_value <- auc(roc_obj)
    auc_values <- c(auc_values, auc_value)
    
    # Store variable importance for this fold
    variable_importance <- rbind(variable_importance, varImp(fit))
    roc_objects[[i]] <- roc_obj
    # Store predictions for this fold
    all_predictions <- c(all_predictions, predictions)
  }
  # Calculate the overall ROC curve and AUC
  overall_roc <- roc(outcome, all_predictions)
  overall_auc <- auc(overall_roc)
  
  
  return(list(mean_auc = mean(auc_values), variable_importance = variable_importance, 
              roc_objects=roc_objects, overall_roc = overall_roc))
}


# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(analysis_data), 0.7 * nrow(analysis_data))
train_data <- analysis_data[train_index, ]
test_data <- analysis_data[-train_index, ]

#train random forest 
res <- calculate_auc(train_data, train_data$outcome)
print(paste("Random Forest AUC:", res$mean_auc))
print("Mean Variable Importance:")
print(res$variable_importance)
res$roc_objects

Omicron_res <- res


WT_res$mean_auc
Delta_res$mean_auc
Omicron_res$mean_auc
Omicron_res$roc_objects

#plot mean AUC for every variant 
auc_df_new <- data.frame(variant=c("WT", "Delta", "Omicron"), mean_AUC=c(0.775, 0.775, 0.8), min_auc=c(0.5909,0.5909, 0.5), max_auc=c(0.9583,0.9583,1))

#pllot df
pdf(file = "auc_mean_and_range_across_each_variant.pdf", height = 8, width = 6)
ggplot(auc_df_new, aes(x = variant)) +
  geom_point(aes(y = mean_AUC), color = "blue", size = 3) +
  geom_errorbar(aes(ymin = min_auc, ymax = max_auc), width = 0.2, color = "red", size = 1) +
  labs(
    x = "Variant",
    y = "AUC",
    title = "Mean and Range of AUC for Each Variant"
  ) +
  scale_y_continuous(limits = c(0.1,1), breaks = seq(0,1,0.2))+
  theme_prism()
dev.off()

#plot all variabel importance results 
variable_importance_df <- data.frame(WT = WT_res$variable_importance, delta=Delta_res$variable_importance, omicron=Omicron_res$variable_importance)
variable_importance_df <- variable_importance_df %>% dplyr::rename(WT=Overall, delta=Overall.1, Omicron=Overall.2)
variable_importance_df_t <- data.frame(t(variable_importance_df))

#create mean df for every variant 
group_vars <- c(
  "group", "group1", "group2", "group3", "group4"
)

age_vars <- c(
  "age", "age1", "age2", "age3", "age4"
)

sex_vars <- c(
  "sex", "sex1", "sex2", "sex3", "sex4"
)

serum_IGG_vars <- c(
  "serum_IGG_RBD_NM", "serum_IGG_RBD_NM1", "serum_IGG_RBD_NM2", "serum_IGG_RBD_NM3", "serum_IGG_RBD_NM4"
)

serum_IGA_vars <- c(
  "serum_IGA_RBD_NM", "serum_IGA_RBD_NM1", "serum_IGA_RBD_NM2", "serum_IGA_RBD_NM3", "serum_IGA_RBD_NM4"
)

serum_igg_abbott_vars <- c(
  "serum_igg_rbd_abbott_au", "serum_igg_rbd_abbott_au1", "serum_igg_rbd_abbott_au2", "serum_igg_rbd_abbott_au3", "serum_igg_rbd_abbott_au4"
)

saliva_IGG_vars <- c(
  "saliva_IGG_RBD_NM", "saliva_IGG_RBD_NM1", "saliva_IGG_RBD_NM2", "saliva_IGG_RBD_NM3", "saliva_IGG_RBD_NM4"
)

saliva_IGA_vars <- c(
  "saliva_IGA_RBD_NM", "saliva_IGA_RBD_NM1", "saliva_IGA_RBD_NM2", "saliva_IGA_RBD_NM3", "saliva_IGA_RBD_NM4"
)

bal_IGG_vars <- c(
  "bal_IGG_RBD_NM", "bal_IGG_RBD_NM1", "bal_IGG_RBD_NM2", "bal_IGG_RBD_NM3", "bal_IGG_RBD_NM4"
)

bal_IGA_vars <- c(
  "bal_IGA_RBD_NM", "bal_IGA_RBD_NM1", "bal_IGA_RBD_NM2", "bal_IGA_RBD_NM3", "bal_IGA_RBD_NM4"
)

variable_importance_df_t$mean_age <- rowMeans(variable_importance_df_t[age_vars])
variable_importance_df_t$mean_group <- rowMeans(variable_importance_df_t[group_vars])
variable_importance_df_t$mean_sex <- rowMeans(variable_importance_df_t[sex_vars])
variable_importance_df_t$mean_serum_IGG <- rowMeans(variable_importance_df_t[serum_IGG_vars])
variable_importance_df_t$mean_serum_IGA <- rowMeans(variable_importance_df_t[serum_IGA_vars])
variable_importance_df_t$mean_serum_igg_abbott <- rowMeans(variable_importance_df_t[serum_igg_abbott_vars])
variable_importance_df_t$mean_saliva_IGG <- rowMeans(variable_importance_df_t[saliva_IGG_vars])
variable_importance_df_t$mean_saliva_IGA <- rowMeans(variable_importance_df_t[saliva_IGA_vars])
variable_importance_df_t$mean_bal_IGG_vars<- rowMeans(variable_importance_df_t[bal_IGG_vars])
variable_importance_df_t$mean_bal_IGA_vars <- rowMeans(variable_importance_df_t[bal_IGA_vars])

variable_importance_df_t <- variable_importance_df_t %>% select(starts_with("mean"))

variable_importance_df_t$variant <- rownames(variable_importance_df_t)

variable_importance_df <- variable_importance_df_t %>% dplyr::select(-variant) %>% t(.)
variable_importance_df <- data.frame(variable_importance_df)


#plot for WT
plot_df <- variable_importance_df %>% select(WT)
plot_df<- plot_df %>% arrange(desc(.))
plot_df$feature <- rownames(plot_df)
plot_df$feature <- factor(plot_df$feature)

pdf(file = "WT_featuers.pdf", height = 8, width = 8)
ggplot(plot_df, aes(x=reorder(feature, WT), y=WT))+
  geom_bar(stat = "identity", fill = "dodgerblue") +
  ylab("Importance")+
  xlab("")+
  scale_y_continuous(breaks = seq(0,6,0.5))+
  coord_flip()+
  theme_prism()
dev.off()

#plot for Delta 
plot_df <- variable_importance_df %>% select(delta)
plot_df<- plot_df %>% arrange(desc(.))
plot_df$feature <- rownames(plot_df)
plot_df$feature <- factor(plot_df$feature)

pdf(file = "Delta_featuers.pdf", height = 8, width = 8)
ggplot(plot_df, aes(x=reorder(feature, delta), y=delta))+
  geom_bar(stat = "identity", fill = "dodgerblue") +
  ylab("Importance")+
  xlab("")+
  scale_y_continuous(breaks = seq(0,6,0.5))+
  coord_flip()+
  theme_prism()
dev.off()

#plot for omicron 

#plot for Omicron 
plot_df <- variable_importance_df %>% select(Omicron)
plot_df<- plot_df %>% arrange(desc(.))
plot_df$feature <- rownames(plot_df)
plot_df$feature <- factor(plot_df$feature)

pdf(file = "Omicron_featuers.pdf", height = 8, width = 8)
ggplot(plot_df, aes(x=reorder(feature, Omicron), y=Omicron))+
  geom_bar(stat = "identity", fill = "dodgerblue") +
  ylab("Importance")+
  xlab("")+
  scale_y_continuous(breaks = seq(0,6,0.5))+
  coord_flip()+
  theme_prism()
dev.off()
  


save.image(file = "dan_covid_analysis.RData")

sessionInfo()
