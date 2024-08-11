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

set.seed(5524)
sample_index <- sample(nrow(heart), round(nrow(heart)*.75), replace = FALSE)
heart_train <- heart[sample_index,]
heart_test <- heart[-sample_index,]

# SVM
library(e1071)
heart_mod1 <- svm(heartDisease~., type = "C-classification", data = heart_train, probability = TRUE)

heart_pred_svm1 <- predict(heart_mod1, heart_train, type = "class")

confusionMatrix(heart_train$heartDisease,heart_pred_svm1, positive = "TRUE")

heart_pred_svm2 <- predict(heart_mod1, heart_test, type = "class")

confusionMatrix(heart_test$heartDisease,heart_pred_svm2, positive = "TRUE")

library(ROCR)
# par(mfrow = c(1,2))
# zbiór treningowy
pred2 <- predict(heart_mod1, heart_train, type = "prob", probability = TRUE)
pred2 <- attr(pred2,"probabilities")
pred = prediction(pred2[,2], heart_train$heartDisease)
perf = performance(pred, "tpr","fpr")
AUC1 <- performance(pred, "auc")@y.values # AUC

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)\nAUC:",AUC1)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Tree model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

# zbiór testowy
pred2 <- predict(heart_mod1, heart_test, type = "prob", probability = TRUE)
pred2 <- attr(pred2,"probabilities")
pred = prediction(pred2[,2], heart_test$heartDisease)
perf = performance(pred, "tpr","fpr")
AUC2 <- performance(pred, "auc")@y.values # AUC

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)\nAUC:",AUC2)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Tree model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)


# walidacja krzyżowa
ctrl_cv5 <- trainControl(method = "cv",number = 5, classProbs = TRUE)

parametryC_sigma <- 
  expand.grid(C = c(0.01, 0.05, 0.1, 0.5, 1, 5),
              sigma = c(0.5, 1, 2.5, 5, 10))

grupa.svm_Radial <- train(make.names(heartDisease) ~ .,
                          data = heart_train, 
                          method = "svmRadial",
                          tuneGrid = parametryC_sigma,
                          trControl = ctrl_cv5)

grupa.svm_Radial$finalModel
modelLookup("svmRadial")

prognozy1 <- predict(grupa.svm_Radial,newdata = heart_train)
prognozy1 <- factor(ifelse(prognozy1=="TRUE.","TRUE","FALSE"),levels = c("FALSE","TRUE"))
table1 <- confusionMatrix(prognozy1, heart_train$heartDisease, positive = "TRUE")
table1

prognozy2 <- predict(grupa.svm_Radial,newdata = heart_test)
prognozy2 <- factor(ifelse(prognozy2=="TRUE.","TRUE","FALSE"),levels = c("FALSE","TRUE"))
table2 <- confusionMatrix(prognozy2, heart_test$heartDisease, positive = "TRUE")
table2

stats1 <- round(c(table1$byClass[c(1,2,7,11)],table1$overall[1]),4) 
stats2 <- round(c(table2$byClass[c(1,2,7,11)],table2$overall[1]),4)

stats <- data.frame(stats1,stats2) %>% `colnames<-`(c("svm_train","svm_test")) %>% t() %>% as.data.frame() %>% rownames_to_column("model")


# ROC
# zbiór treningowy
pred2 <- predict(grupa.svm_Radial, heart_train, type = "prob", probability = TRUE)
roc_pred_svm = prediction(pred2[,2], heart_train$heartDisease)
roc_perf_svm = performance(roc_pred_svm, "tpr","fpr")
AUC2 <- performance(roc_pred_svm, "auc")@y.values # AUC
plot(roc_perf_svm, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)\nAUC:",AUC2)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("SVM model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

# zbiór testowy
pred2_2 <- predict(grupa.svm_Radial, heart_test, type = "prob", probability = TRUE)
roc_pred_svm = prediction(pred2_2[,2], heart_test$heartDisease)
roc_perf_svm = performance(roc_pred_svm, "tpr","fpr")
AUC2_2 <- performance(roc_pred_svm, "auc")@y.values # AUC
plot(roc_perf_svm, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)\nAUC:",AUC2_2)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("SVM model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)


