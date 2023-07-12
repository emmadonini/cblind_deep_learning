library(terra)
library(plotly)
library(tidyverse)
library(caTools)
library(randomForest)

### Caricare le numerose palette 
df_fin <- load("df_fin.RData")

# Dividere il dataset per training e test
split <- sample.split(df_fin, SplitRatio = 0.7)
train <- subset(df_fin, split == "TRUE")
test <- subset(df_fin, split == "FALSE")

# Modello random forest train
classifier_RF <- randomForest(x = train[,-4], y = as.factor(train$km.cluster), ntree = 500)

# Testare il modello e verificare 
pr <- predict(classifier_RF, test[,-4])
confusion_mtx <- table(test[, 4], pr)
confusion_mtx 
