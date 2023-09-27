



#Divisione colori 
#.....

l_col <- list(red, yellow, white, green,....)
pal <- NA
i <- 1

for(col in l_col) {

  if(!all(is.na(col))) {
    #Usa switch - case
    #Accedi a dove è scritta la stringa del colore per l'espressione
    #Nei case ci metti il colore da sodtituire già in esadecimale (pal[i] <- ...)
    i <- i + 1
  }

}

#Grafico ggplot in cui usi pal con scale_fill_manual() -> argomento values


