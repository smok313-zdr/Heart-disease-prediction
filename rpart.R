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

# drzewa decyzyjne
library(rpart)
heart_mod1 <- rpart(heartDisease~., method = "class", data = heart_train)

library(rpart.plot)
rpart.plot(heart_mod1)

heart_pred_rpart1 <- predict(heart_mod1, heart_train, type = "class")

confusionMatrix(heart_train$heartDisease,heart_pred_rpart1, positive = "TRUE")

heart_pred_rpart2 <- predict(heart_mod1, heart_test, type = "class")

confusionMatrix(heart_test$heartDisease,heart_pred_rpart2, positive = "TRUE")

library(ROCR)
# par(mfrow = c(1,2))
# zbiór treningowy
pred2 <- predict(heart_mod1, heart_train, type = "prob")
pred = prediction(pred2[,2], heart_train$heartDisease)
perf = performance(pred, "tpr","fpr")
AUC1 <- performance(pred, "auc")@y.values # AUC

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)\nAUC:",AUC1)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Tree model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)


# zbiór testowy
pred2 <- predict(heart_mod1, heart_test, type = "prob")
pred = prediction(pred2[,2], heart_test$heartDisease)
perf = performance(pred, "tpr","fpr")
AUC2 <- performance(pred, "auc")@y.values # AUC

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)\nAUC:",AUC2)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Tree model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

# pruuning
# ------------------------------------------------------------------------------------------------
opt <- which.min(heart_mod1$cptable[, "xerror"])
cp <- heart_mod1$cptable[opt, "CP"]
heart_mod1p <- prune(heart_mod1, cp = cp)
rpart.plot(heart_mod1p)

heart_pred_rpart1p <- predict(heart_mod1p, heart_train, type = "class")

confusionMatrix(heart_train$heartDisease,heart_pred_rpart1p, positive = "TRUE")

heart_pred_rpart2p <- predict(heart_mod1p, heart_test, type = "class")

confusionMatrix(heart_test$heartDisease,heart_pred_rpart2p, positive = "TRUE")

library(ROCR)
# par(mfrow = c(1,2))
# zbiór treningowy
pred2 <- predict(heart_mod1p, heart_train, type = "prob")
pred = prediction(pred2[,2], heart_train$heartDisease)
perf = performance(pred, "tpr","fpr")
AUC1 <- performance(pred, "auc")@y.values # AUC

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)\nAUC:",AUC1)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Tree model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)


# zbiór testowy
pred2 <- predict(heart_mod1p, heart_test, type = "prob")
pred = prediction(pred2[,2], heart_test$heartDisease)
perf = performance(pred, "tpr","fpr")
AUC2 <- performance(pred, "auc")@y.values # AUC

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)\nAUC:",AUC2)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Tree model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

# walidacja krzyżowa

seeds <- vector(mode = "list", length = nrow(heart_train) + 1)
seeds <- lapply(seeds, function(x) 5:25)

fitControl <- trainControl(method = "cv",
                           number = 10,
                           returnResamp="final",
                           #    ## Estimate class probabilities
                           classProbs = T,
                           #    ## Evaluate performance using
                           #    ## the following function
                           summaryFunction = twoClassSummary,
                           savePredictions = T,
                           seeds=seeds,
                           search = "random"
)

DT <- train(make.names(heartDisease)~.,data=heart_train,
            method =  'rpart',
            trControl = fitControl,
            metric="ROC",
            tuneLength = 5,
            parms = list(split = 'information'),
            maxdepth = 10,
            minsplit = 50, # number of obs in a split
            minbucket =  round(50/3)
)

heart_mod1p <- prune(heart_mod1, cp = DT$bestTune$cp)

heart_pred_rpart1p <- predict(heart_mod1p, heart_train, type = "class")
table1 <- confusionMatrix(heart_pred_rpart1p, heart_train$heartDisease, positive = "TRUE")
table1

heart_pred_rpart2p <- predict(heart_mod1p, heart_test, type = "class")
table2 <- confusionMatrix(heart_pred_rpart2p, heart_test$heartDisease, positive = "TRUE")
table2 

stats1 <- round(c(table1$byClass[c(1,2,7,11)],table1$overall[1]),4) 
stats2 <- round(c(table2$byClass[c(1,2,7,11)],table2$overall[1]),4)

stats <- data.frame(stats1,stats2) %>% `colnames<-`(c("rpart_train","rpart_test")) %>% t() %>% as.data.frame() %>% rownames_to_column("model")

# zbiór treningowy
pred3 <- predict(heart_mod1p, heart_train, type = "prob")
roc_pred_rpart = prediction(pred3[,2], heart_train$heartDisease)
roc_perf_rpart = performance(roc_pred_rpart, "tpr","fpr")
AUC3 <- performance(roc_pred_rpart, "auc")@y.values # AUC
plot(roc_perf_rpart, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Train)\nAUC:",AUC3)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Tree model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

# zbiór testowy
pred3_2 <- predict(heart_mod1p, heart_test, type = "prob")
roc_pred_rpart = prediction(pred3_2[,2], heart_test$heartDisease)
roc_perf_rpart = performance(roc_pred_rpart, "tpr","fpr")
AUC3_2 <- performance(roc_pred_rpart, "auc")@y.values # AUC
plot(roc_perf_rpart, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = paste("ROC Curve (Test)\nAUC:",AUC3_2)) # ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=1, "bottomright", legend = c("Tree model","Random"), col = c(4,2), lty = c(1,2), lwd = 2)
