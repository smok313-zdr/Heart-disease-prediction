library(tidyverse)
library(caret)

rm(list=ls())

heart <- read_csv("heart.csv", col_types = "nffnnffnfnfnff")

colSums(is.na(heart))
glimpse(heart)

# Przygotowanie danych 
heart <- heart %>% filter(!is.na(restingBP) & !is.na(cholesterol) & !is.na(highBloodSugar) & !is.na(restingECG) & !is.na(restingHR) 
                          & !is.na(exerciseAngina) & !is.na(STdepression) & !is.na(STslope) & !is.na(coloredVessels) & !is.na(defectType))

heart <- heart %>% filter(!highBloodSugar=="NA" & !STslope=="NA" & !defectType=="NA")

# normalizacja
normalize <- function(x) { 
  return((x-min(x))/(max(x)-min(x)))
}

heart <- heart %>% mutate(age = normalize(age)) %>%
  mutate(restingBP = normalize(restingBP)) %>%
  mutate(cholesterol = normalize(cholesterol)) %>%
  mutate(restingHR = normalize(restingHR)) %>%
  mutate(STdepression = normalize(STdepression)) %>%
  mutate(coloredVessels = normalize(coloredVessels))

set.seed(1432)
sample_index <- sample(nrow(heart), round(nrow(heart)*.75), replace = FALSE)
heart_train <- heart[sample_index,]
heart_test <- heart[-sample_index,]

# knn
library(class)
heart_pred1 <- knn(train = heart_train, test = heart_train, cl = heart_train_labels, k = 7, prob = TRUE)

table1 <- confusionMatrix(heart_train_labels, heart_pred1)
table1

heart_pred2 <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k = 7, prob = TRUE)

table2 <- confusionMatrix(heart_test_labels, heart_pred2)
table2

stats1 <- round(c(table1$byClass[c(1,2,7,11)],table1$overall[1]),4) 
stats2 <- round(c(table2$byClass[c(1,2,7,11)],table2$overall[1]),4)

stats <- data.frame(stats1,stats2) %>% `colnames<-`(c("heart_pred1","heart_pred2")) %>% t() %>% as.data.frame() %>% rownames_to_column("model")

#Accuracy plot 
i=1
k.optm=1
for (i in 1:15){
  knn.mod <- knn(train=heart_train, test=heart_test, cl=heart_train_labels, k=i)
  k.optm[i] <- 100 * sum(heart_test_labels == knn.mod)/NROW(heart_test_labels)
  k=i
  cat(k,'=',k.optm[i],'')
}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

# -------------------------------------------------------------------------------------------------------
# knn3

heart_mod1 <- caret::knn3(heartDisease~., heart_train, k = 6)

heart_pred_knn1 <- predict(heart_mod1, heart_train, type = "class")

table1 <- confusionMatrix(heart_pred_knn1,heart_train$heartDisease, positive = "TRUE")
table1

heart_pred_knn2 <- predict(heart_mod1, heart_test, type = "class")

table2 <- confusionMatrix(heart_pred_knn2, heart_test$heartDisease, positive = "TRUE")
table2

stats1 <- round(c(table1$byClass[c(1,2,7,11)],table1$overall[1]),4) 
stats2 <- round(c(table2$byClass[c(1,2,7,11)],table2$overall[1]),4)

stats <- data.frame(stats1,stats2) %>% `colnames<-`(c("heart_pred1","heart_pred2")) %>% t() %>% as.data.frame() %>% rownames_to_column("model")

#Accuracy plot 
K_VALUES  <- 1:20
test_acc  <- numeric(0)
train_acc <- numeric(0)

# calculate different models for each value of k 
for (x in K_VALUES){
  model <- knn3(heartDisease~., heart_train, k = x)
  pred_test <- predict(model, heart_test, type = "class")
  pred_test_acc <- confusionMatrix(table(pred_test,
                                         heart_test$heartDisease), positive = "TRUE")$overall["Accuracy"]
  test_acc <- c(test_acc, pred_test_acc)
  
  pred_train <- predict(model, heart_train, type = "class")
  pred_train_acc <- confusionMatrix(table(pred_train,
                                          heart_train$heartDisease), positive = "TRUE")$overall["Accuracy"]
  train_acc <- c(train_acc, pred_train_acc)
}

library(data.table)
data <- data.table(x = K_VALUES, train = train_acc, test = test_acc)

# plot a validation curve 
plot_data <- gather(data, "type", "value", -x)
g <- qplot(x = x,
           y = value,
           data = plot_data,
           color = type,
           geom = "path") + theme_bw() + 
  scale_x_continuous(labels = c(min(K_VALUES):max(K_VALUES)),breaks = c(min(K_VALUES):max(K_VALUES))) +
  labs(x = "k", y = "Accuracy", title = "Accuracy plot for different k")
print(g)

library(ROCR)
# zbiór treningowy
pred1 <- predict(heart_mod1, heart_train, type = "prob")
roc_pred_knn <- prediction(pred1[,2], heart_train$heartDisease)
AUC1 <- performance(roc_pred_knn, "auc")@y.values # AUC
roc_pred_knn <- performance(roc_pred_knn, "tpr", "fpr")
plot(roc_pred_knn, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)")) 
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c(paste("KNN model, AUC:",AUC1),"Random"), col = c(4,2), lty = c(1,2), lwd = 2)

# zbiór testowy
pred1_2 <- predict(heart_mod1, heart_test, type = "prob")
roc_pred_knn <- prediction(pred1_2[,2], heart_test$heartDisease)
AUC1_2 <- performance(roc_pred_knn, "auc")@y.values # AUC
roc_pred_knn <- performance(roc_pred_knn, "tpr", "fpr")
plot(roc_pred_knn, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)")) 
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c(paste("KNN model, AUC:",AUC1_2),"Random"), col = c(4,2), lty = c(1,2), lwd = 2)


# treningowy wszystkie modele
pred1 <- predict(heart_mod1, heart_train, type = "prob")
roc_pred_knn <- prediction(pred1[,2], heart_train$heartDisease)
AUC1 <- performance(roc_pred_knn, "auc")@y.values # AUC
roc_pred_knn <- performance(roc_pred_knn, "tpr", "fpr")
plot(roc_pred_knn, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)")) # ROC
plot(roc_perf_svm, add = TRUE, col = 3, lwd = 2)
plot(roc_perf_rpart, add = TRUE, col = 5, lwd = 2)
plot(roc_perf_rf, add = TRUE, col = 6, lwd = 2)
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c(paste("KNN model, AUC:",AUC1),paste("SVM model, AUC:",AUC2),paste("Decision tree model, AUC:",AUC3),
                                        paste("Random forest model, AUC:",AUC4),"Random"), col = c(4,3,5,6,2), lty = c(1,1,1,1,2), lwd = 2)


# testowy wszystkie modele
pred1_2 <- predict(heart_mod1, heart_test, type = "prob")
roc_pred_knn <- prediction(pred1_2[,2], heart_test$heartDisease)
AUC1_2 <- performance(roc_pred_knn, "auc")@y.values # AUC
roc_pred_knn <- performance(roc_pred_knn, "tpr", "fpr")
plot(roc_pred_knn, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)")) # ROC
plot(roc_perf_svm, add = TRUE, col = 3, lwd = 2)
plot(roc_perf_rpart, add = TRUE, col = 5, lwd = 2)
plot(roc_perf_rf, add = TRUE, col = 6, lwd = 2)
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c(paste("KNN model, AUC:",AUC1_2),paste("SVM model, AUC:",AUC2_2),paste("Decision tree model, AUC:",AUC3_2),
                                        paste("Random forest model, AUC:",AUC4_2),"Random"), col = c(4,3,5,6,2), lty = c(1,1,1,1,2), lwd = 2)

