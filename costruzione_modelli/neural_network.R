library(terra)
library(plotly)
library(tidyverse)
library(caTools)
library(randomForest)
library(nnet)

essenz4 <- load("essenz4.RData")

# Trasformare colonna color in factor perchè serve per eseguire la funzione nnet
color <- factor(essenz4$color)
essenz4_f <- cbind(essenz4[, 1:3], color)

# Estrarre un campione di 1/2 delle righe totali del dataframe
samp <- sample(1:193280, 96640) 

# Eseguiti diversi tentativi della funzione nnet per capire il migliore argomento della funzione da usare
# Modello nnet 
mdRFe <- nnet(color ~ ., data=essenz4_f, subset=samp, size=4, rang=0.15,  
              decay=1.0e-10, maxit=300, MaxNWts = 3500)

# Testare il modello e verificare
pr <- predict(mdRFe, essenz4_f[-samp, ], type="class")
cm <- table(essenz4_f$color[-samp], pr)
