palette_rainbow <- function(im, cvd = c("protanopia", "deuteranopia", "tritanopia"), r = 1, g = 2, b = 3) {

  #Controllo cvd
  cvd <- cvd[1]
  if(!cvd %in% c("protanopia", "deuteranopia", "tritanopia")) stop("Wrong 'cvd` value. It can be 'protanopia', 'deuteranopia', or 'tritanopia'")

  #Controllo immagine
  if (!inherits(im, "SpatRaster") && !inherits(im, "RasterLayer") && !inherits(im, "RasterStack") && !inherits(im, "RasterBrick") && !inherits(im, "list") && !inherits(im, "character")){
    stop("'im' must be a raster object, a list or a path to an image")
  } else if (inherits(im, "RasterLayer") || inherits(im, "RasterStack") || inherits(im, "RasterBrick") || is.character(im)){
    suppressWarnings(im <- terra::rast(im))
  } else if (inherits(im, "list")){
    invisible(lapply(im, function(x) if(!inherits(x, "SpatRaster")) stop("all the elements of the list must be SpatRaster objects")))
    suppressWarnings(im <- terra::rast(im))
  }

  #Preparazione dati
  df <- as.data.frame(im, xy = TRUE)
  df_mod <- data.frame(df[,1], df[,2], df[,(r + 2)], df[,(g + 2)], df[,(b + 2)])
  colnames(df_mod) <- c("x", "y", "R", "G", "B")

 #Modello
 classifier <- load("classifier_RF_rain_pal_cl.RDS")
 colors <- predict(classifier, df_mod[,c(3,4,5)])
 col <- cbind(df_mod, colors)

#Divisione colori
red <- col[col$colors == "red",]
yellow <- col[col$colors == "yellow",]
green <- col[col$colors == "green",]
lb <- col[col$colors == "lightblue",]
blue <- col[col$colors == "blue",]
purple <- col[col$colors == "purple",]

#Ordino per intensità
red <- red[with(red, order(R, -G, B)),]
yellow <- yellow[with(yellow, order(R, G, -B)),]
green <- green[with(green, order(G, -B, -R)),]
lb <- lb[with(lb, order(G, B, -R)),]
blue <- blue[with(blue, order(B, -R, -G)),]
purple <- purple[with(purple, order(B, R, -G)),]

# Assemblare il dataset ordinato per colore per ricolorarlo con la palette viridis
fin <- rbind(red, yellow, green, lb, blue, purple)
fin <- cbind(fin, seq(1, nrow(fin)))
colnames(fin)[ncol(fin)] <- "values"

#Plot
ggt <- ggplot(fin, aes(x = x, y = y, fill = values)) +
    geom_raster() +
    coord_equal() +
    theme_void() 
                     
  if(cvd == "deuteranopia") {
    pl <- ggt + scale_fill_viridis_c(na.value = "transparent")
  } else if(cvd == "protanopia") {
    pl <- ggt + scale_fill_viridis_c(na.value = "transparent", option = "E")
  } else if(cvd == "tritanopia") {
    pl <- ggt + scale_fill_viridis_c(na.value = "transparent", option = "A")
  }
  
  return(pl)
  
}
  
