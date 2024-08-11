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

set.seed(4321)
sample_index <- sample(nrow(heart), round(nrow(heart)*.75), replace = FALSE)
heart_train <- heart[sample_index,]
heart_test <- heart[-sample_index,]

# Random Forest
library(randomForest)

heart_rf_mod1 <- randomForest(heartDisease~., data = heart_train)
heart_rf_mod1_pred <- predict(heart_rf_mod1,heart_train)

confusionMatrix(heart_train$heartDisease,heart_rf_mod1_pred, positive = "TRUE")

heart_rf_mod2_pred <- predict(heart_rf_mod1,heart_test)

confusionMatrix(heart_test$heartDisease,heart_rf_mod2_pred, positive = "TRUE")

library(ROCR)
# par(mfrow = c(1,2))
# zbiór treningowy
pred2 <- predict(heart_rf_mod1, heart_train, type = "prob")
pred = prediction(pred2[,2], heart_train$heartDisease)
perf = performance(pred, "tpr","fpr")
AUC1 <- performance(pred, "auc")@y.values # AUC

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)\nAUC:",AUC1)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Random forest model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)


# zbiór testowy
pred2 <- predict(heart_rf_mod1, heart_test, type = "prob")
pred = prediction(pred2[,2], heart_test$heartDisease)
perf = performance(pred, "tpr","fpr")
AUC2 <- performance(pred, "auc")@y.values # AUC

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)\nAUC:",AUC2)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Random forest model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

# walidacja krzyżowa

seeds <- vector(mode = "list", length = nrow(heart_train) + 1)
seeds <- lapply(seeds, function(x) 1:11)
 
 
   fitControl <- trainControl(
                              method = "cv", number = 10,
                              returnResamp="all",
                              #    ## Estimate class probabilities
                              classProbs = T,
                              #    ## Evaluate performance using
                              #    ## the following function
                              summaryFunction = twoClassSummary,
                              savePredictions = T,
                              search = "random"
                              )
   
   rf <- train(make.names(heartDisease)~., data=heart_train,
                                  method =  'rf',
                                  importance=TRUE,
                                  trControl = fitControl,
                                  # ntree=500,
                                  metric="ROC",
                                  tuneLength = 5
                  )

   heart_rf_mod1 <- randomForest(heartDisease~., data = heart_train, mtry = rf$bestTune$mtry, ntree = 500)
   heart_rf_mod1_pred <- predict(heart_rf_mod1,heart_train)
   table1 <- confusionMatrix(heart_rf_mod1_pred, heart_train$heartDisease, positive = "TRUE")
   table1
   
   heart_rf_mod2_pred <- predict(heart_rf_mod1,heart_test)
   table2 <- confusionMatrix(heart_rf_mod2_pred, heart_test$heartDisease, positive = "TRUE")
   table2
   
   stats1 <- round(c(table1$byClass[c(1,2,7,11)],table1$overall[1]),4) 
   stats2 <- round(c(table2$byClass[c(1,2,7,11)],table2$overall[1]),4)
   
   stats <- data.frame(stats1,stats2) %>% `colnames<-`(c("rf_train","rf_test")) %>% t() %>% as.data.frame() %>% rownames_to_column("model")
   
   library(ROCR)
   # par(mfrow = c(1,2))
   # zbiór treningowy
   pred4 <- predict(heart_rf_mod1, heart_train, type = "prob")
   roc_pred_rf = prediction(pred4[,2], heart_train$heartDisease)
   roc_perf_rf = performance(roc_pred_rf, "tpr","fpr")
   AUC4 <- performance(roc_pred_rf, "auc")@y.values # AUC
   plot(roc_perf_rf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)\nAUC:",AUC4)) # ROC
   lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
   legend(cex=1, "bottomright", legend = c("Random forest model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)
   
   
   # zbiór testowy
   pred4_2 <- predict(heart_rf_mod1, heart_test, type = "prob")
   roc_pred_rf = prediction(pred4_2[,2], heart_test$heartDisease)
   roc_perf_rf = performance(roc_pred_rf, "tpr","fpr")
   AUC4_2 <- performance(roc_pred_rf, "auc")@y.values # AUC
   plot(roc_perf_rf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)\nAUC:",AUC4_2)) # ROC
   lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
   legend(cex=1, "bottomright", legend = c("Random forest model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)
   
   