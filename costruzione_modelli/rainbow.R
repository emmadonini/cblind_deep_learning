library(terra)
library(plotly)
library(tidyverse)
library(caTools)
library(randomForest)

### Caricare la palette rainbow
rain <- rast("rainbow1.png")

# Trasformare in dataframe e rinominare le colonne
df_rain <- data.frame(rain)
colnames(df_rain) <- c("R", "G", "B")

# Classificazione unsupervised per dividere i colori e creare il dataset per il random forest
# esempio: creare 6 cluster per colori (red, yellow, green, light blue, blue, purple)
km <- kmeans(df_rain[,1:3], centers = 6, nstart = 20, iter.max = 50)

# Plot per vedere a che colori corrispondono le classi uscite
plot_ly(x = df_rain$R, 
        y = df_rain$G, 
        z = df_rain$B, 
        type="scatter3d", 
        mode="markers", 
        color = as.factor(km$cluster), 
        colors = "Set1") 

# Creato il plot interattivo assegnare le classi in base ai valori RGB
# Le classi sono:
# red: R ~ 255; G ~ 0; B ~ 0;
# yellow: R ~ 255; G ~ 255; B ~ 0;
# green: R ~ 0; G ~ 255; B ~ 0;
# light blue: R ~ 0; G ~ 255; B ~ 255;
# blue: R ~ 0; G ~ 0; B ~ 255;
# purple: R ~ 255; G ~ 0; B ~ 255;

# Cambiare le etichette in base ai valori della classificazione
km$cluster[km$cluster == 1] <- "yellow"
km$cluster[km$cluster == 2] <- "red"
km$cluster[km$cluster == 3] <- "blue"
km$cluster[km$cluster == 4] <- "green"
km$cluster[km$cluster == 5] <- "lightblue"
km$cluster[km$cluster == 6] <- "purple"

# Ora che si hanno i dati creare il dataset per la supervised classification
df_class <- data.frame(df_rain[,1:3], km$cluster)

# Dividere il dataset per training e test
split <- sample.split(df_class, SplitRatio = 0.7)
train <- subset(df_class, split == "TRUE")
test <- subset(df_class, split == "FALSE")

# Modello random forest train
classifier_RF <- randomForest(x = train[,-4], y = as.factor(train$km.cluster), ntree = 500)

# Testare il modello e verificare 
pr <- predict(classifier_RF, test[,-4])
confusion_mtx <- table(test[, 4], pr)
confusion_mtx 
