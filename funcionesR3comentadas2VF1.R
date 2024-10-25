##################################################################################################
# La función "Productos" hace multiplicaciones para obtener las interacciones de dos factores
# entre las variables X1, X2, X3, X4, X5 y X6. Es decir, genera las nuevas variables X1X2,
# X1X3, ..., X5X6.
# Variable de entrada: mmatriz.- Una matriz de mxn
# Variable de salida : mvarint.- La multiplicaci??n (sin repeticiones) de las variables que son elementos
#                    de "mmatriz", generando as?? la interacci??n de 2 factores de las variables

productos<- function(mmatriz){
filas<- length(mmatriz[,1])
mvarint<- matrix(0,nrow = filas,ncol = 15)
k<-1
t<-0
for (p in 1:5) {
  for (r in 2:6) {
    if(r+t>=7){
      break
    }else{
      for (ss in 1:filas) {
        mvarint[ss,k]<- mmatriz[ss,p]*mmatriz[ss,r+t]
        
      }
      k<- k+1
    }
  }
  t<- t+1
}
return(mvarint)
}
#############################################################################

##########################################################################################
# Simula la base que describe el articulo. 
# Variables de entrada: X1.- Es un vector de 1x30 con valores generados a partir de una 
#                            uniforme entre 0 y 100 que se genera previamente.
# Variables de salida: matini.- Una base de 30 x 6 con valores entre -1 y 1 y que presenta
#                               correlaci??n inducida en las variables X2 y X3.

basesim <- function(X1){
  #Se agrega tambase para crear simulaciones con menos observaciones
  tambase <- 30
  sigma<- .40*X1
  X2 <- rep(0, times = tambase)
  X3 <- rep(0, times = tambase)
  #X4 <- rep(0, times = tambase)
  extras <- runif(3, 0, 100)
  #extras <- runif(2, 0, 100)
  sigmaex <- .10*extras
  
  for (i in 1:tambase) {
    X2[i] <- rnorm(1, mean = X1[i], sd = sigma[i])
    X3[i] <- rnorm(1, mean = X1[i], sd = sigma[i])
    #X4[i] <- rnorm(1, mean = X1[i], sd = sigma[i])
    }
  X4 <- rnorm(tambase, mean = extras[1], sd = sigmaex[1])
  X5 <- rnorm(tambase, mean = extras[2], sd = sigmaex[2])
  X6 <- rnorm(tambase, mean = extras[3], sd = sigmaex[3])
  #X5 <- rnorm(tambase, mean = extras[1], sd = sigmaex[1])
  #X6 <- rnorm(tambase, mean = extras[2], sd = sigmaex[2])
  
  matini <- cbind(X1,X2,X3,X4,X5,X6)
  
  ##########Ahora el codificado entre -1 y 1############
  ### codificado en toda la matriz
  maxi <- max(matini)
  mini <- min(matini)
  matini <- (2*(matini)-(maxi+mini))/(maxi-mini)
  return(matini)
}

############################################################################################

############################################################################################
# La funci??n "correla2" verifica la correlaci??n que hay entre las variables X1, X2 y X3 para
# as?? escoger en qu?? orden se asignaran los elementos de la matriz "matmisra" para aumentar
# la matriz y as?? disminuir la colinealidad. Es el paso fundamental del m??todo R3 descrito en
# el art??culo. Se aplica de la misma manera que se sugiere en el art??culo.
# Variable de entrada: matini.- matriz aumentada a la cual se le verificar?? la colinealidad
# variable de salida: sindupli.- un vector de 1x3 que contiene los indices 1, 2 y 3 de las
#                                variables X1, X2, y X3, respectivamente, los cuales est??n 
#                                ordenados dependiendo del valor de su colinealidad (de 
#                                mayor a menor). 

correla2<- function(matini){
  primeros <- c("X1","X2","X3")
  
  extraer <- c(1,2,3)
  correlacion <- cor(matini[extraer+1])
  # Se ordenan los elementos de la matriz de correlaci??n en orden descendente
  importantes <- sort(correlacion[upper.tri(correlacion, diag = FALSE)], decreasing = TRUE)
  completa <- cor(matini[,2:7])
  # Se asignan los ??ndices dependiendo del orden obtenido en la matriz de correlaci??n.
  indices <- rbind(which(completa==importantes[1], arr.ind=TRUE),
                     which(completa==importantes[2], arr.ind=TRUE),
                     which(completa==importantes[3], arr.ind=TRUE))
  # eliminamos duplicados para solo extraer los valores que nos interesan
  sindupli <- unique(indices[,1])[1:3]
  sindupli <- sindupli[!is.na(sindupli)]
  return(sindupli)
  
  
}

##############################################################################################
correla<- function(matini,stepreg){
  primeros <- c("X1","X2","X3","X4","X5", "X6")
  largo <- length(stepreg$coefficients)
  coeficcomp<- names(stepreg$coefficients)[2:largo]
  inx <- 2
  if(sum(inx)<=1){
    sindupli <- c(1,2,3)
    return(sindupli)
  }else{
    posi <- 1:6
    #extraer <- posi[inx>0]
    extraer <- posi
    #Se hace la matriz de correlacion para los efectos principales
    correlacion <- cor(matini[extraer+1])
    importantes <- sort(correlacion[upper.tri(correlacion, diag = FALSE)], decreasing = TRUE)
    importantes <- importantes[!is.na(importantes)]
    completa <- cor(matini[,2:7])
    indi<- length(importantes[importantes>=0.5])
    if(indi<= 1){
      indices <- as.vector(which(completa==importantes[1], arr.ind=TRUE)[2,])
      sindupli <- indices
    }else if(indi==2){
      indices <- rbind(which(completa==importantes[1], arr.ind=TRUE)[1,],
                       which(completa==importantes[2], arr.ind=TRUE)[1,])
      sdprov <- as.vector(c(indices[1,],indices[2,]))
      lcadena <- length(unique(sdprov))
      if(lcadena==4){
        sindupli <- sdprov
      }else{
        compara <- unique(sdprov[1:4])
        evn <- matrix(0,1,length(compara))
        sss<-0
        for (j in 1:length(compara)) {
          evn[j] <- length(subset(sdprov[1:4], sdprov[1:4]==compara[j]))
        }
        cadena<- rbind(compara,evn)
        
        rrr<- compara[which(cadena[2,1:length(compara)] == 2)]
        sindupli <- c(rrr,compara[-which(compara==rrr)])
      }
    }else{
      # En caso de que el indice sea mayor 2 proseguimos a formar la cadena extrayendo los indices
      indices <- rbind(which(completa==importantes[1], arr.ind=TRUE)[1,],
                       which(completa==importantes[2], arr.ind=TRUE)[1,],
                       which(completa==importantes[3], arr.ind=TRUE)[1,])
      sdprov <- as.vector(c(indices[1,],indices[2,]))
      lcadena <- length(unique(sdprov))
      if(lcadena==4){
        sindupli <- sdprov
      }else{
        sdprov <- as.vector(c(sdprov,indices[3,]))
        compara <- unique(sdprov[1:4])
        evn <- matrix(0,1,length(compara))
        sss<-0
        #Aqui se cuenta cuantos elementos hay en la cadena tentativa para acomodar
        # los indices de manera correcta
        for (j in 1:length(compara)) {
          evn[j] <- length(subset(sdprov[1:4], sdprov[1:4]==compara[j]))
        }
        cadena<- rbind(compara,evn)
        # Se identifica el de mayor aparicion para formar la cadena
        rrr<- compara[which(cadena[2,1:length(compara)] == 2)]
        sindupli <- c(rrr,compara[-which(compara==rrr)])
        # Aqui checamos que el elemento que escogimos como el que esta confundido
        # no corresponda a los indices restantes y en caso de que si, se cambia 
        # para obtener la cadena correcta
        boole <- sindupli[1] %in% indices[3,]
        if(boole == TRUE){
          sindupli <- unique(c(sindupli,indices[3,]))
        }else{
          sindupli<- sindupli
        }
        
      }
      
    }
    
  }
  return(sindupli)
}
##############################################################################################
decouple2f <- function(indfijo,indmax,aumen){
  maxmax <- length(indmax)
  lif <- length(indfijo)
  nuevindi <- matrix(0,2,lif)
  completa <- cor(matini[,2:7])
  for (i in 1:lif) {
    parasacar<- c(indmax,indfijo[i])
    correlacion <- cor(matini[parasacar+1])
    
    importantes <- sort(correlacion[1:maxmax,(maxmax+1)],decreasing = TRUE)
    importantes <- importantes[!is.na(importantes)]
    indices <- rbind(which(completa==importantes[1], arr.ind=TRUE)[1,])
    pm <- indmax[indmax %in% indices]
    # Aqui volteo el signo para desconfundir los t??rminos
    nuevindi[,i] <- -aumen[,pm]
  }
  for (j in 1:lif) {
    aumen[,indfijo[j]]<- nuevindi[,j]
  }
  
  return(aumen)
}







