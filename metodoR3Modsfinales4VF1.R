## Paquetes que se usar??n en el m??todo######################################################
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("car", lib.loc="~/R/R-3.4.3/library")
library("tictoc", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("glmnet")
############################################################################################

# El nombre de todas las variables que se utilizan
variables<- c("y","X1","X2","X3","X4","X5", "X6","X1X2","X1X3","X1X4","X1X5", "X1X6",
              "X2X3","X2X4","X2X5", "X2X6","X3X4","X3X5", "X3X6","X4X5", "X4X6","X5X6")
obligados <- c("X1","X2","X3")
secundarios <- c("X4","X5","X6")
variablesridge <- c("y","X1","X2","X3","X1X2","X1X3","X2X3")

#listadenombres<-list(1)
# Estas son matrices fijas que nos serviran para aumentar la matriz. Se obtuvieron a partir
#  del ejemplo mostrado en el art??culo (Tablas 12a, 12b y 12c). Se asigna solamente a las 
#  variables X1, X2 o X3 y se utilizan de forma intercalada. Es decir, en una iteraci??n se 
#  utiliza "matmisra1" y en la siguiente cambia a "matmisra2", siguiendo esa secuencia hasta
#  que el m??todo se detiene.

matmisra1<-matrix(c(-1,1,-1,1,
                    1,-1,-1,1), nrow = 2, ncol = 4, byrow = TRUE)
matmisra2<-matrix(c(-1,1,1,-1,
                    1,-1,1,-1), nrow = 2, ncol = 4, byrow = TRUE)
# Es una matriz fija que se a??ade a las variables X4, X5 y X6 siempre en el mismo orden. 

#matdefaul2<- matrix(c(-1,1,1,-1,
 #                     1,-1,1,-1), nrow = 2, ncol = 4, byrow = TRUE)
#matdefaul2<- matrix(c(-1,1,-1,1,
 #                     1,-1,1,-1), nrow = 4, ncol = 4, byrow = TRUE)
# Las variables con las que se forma el modelo real (quer deseamos obtener)
elreal <- sort(c("X1","X2","X3","X1X2","X1X3","X2X3"))

# maxiter: n??mero de bases que se van a simular. Limita el n??mero de veces que se aplicar?? el m??todo.
maxiter<- 1
#Gigantevarcero guarda todas las variables de los modelos inciales
#Gigantecoefcero guarda los valores de todos los modelos iniciales
#Vecyini guardar?? las y iniciales
#Vecfivini guardara los vifs maximos iniciales PARA TODOS LOS MODELOS
#Ridgegigante guarda todas las variables de los modelos finales usando Ridge
#Reggigante guarda los resultados para una regresion con las variables 
#generadoras sin aplicarles ningun cambio
#contaresta me dice cuantas veces modifiqué stepreg2 porque me salió sin modelo inicial
gigantevarcero <- matrix(0,maxiter,22)
gigantecoefcero <- matrix(0,maxiter,21)
gigantevarcerostep1 <- matrix(0,maxiter,22)
gigantevarcerostep2 <- matrix(0,maxiter,22)
vecyini<- matrix(0,maxiter,1)
vecfivini <- matrix(0,maxiter,1)
ridgegigante <- matrix(0,maxiter,7)
reggigante <- matrix(0,maxiter,7)
containi<- 1
contaresta <- 1
donderestar <- matrix(0,1500,1)
# gigante guarda todas las variables de los modelos finales
# en "vecfiv" y "veccorridas" se guardan el facotr de inflaci??n de la varianza
# de la i-ésima iteración y el número de corridas que fueron necesarias para 
# alcanzar el resultado, respectivamente.
# Vecyfinal guarda las y finales
gigante <- matrix(0,maxiter,21)
vecyfinal <- matrix(0,1,maxiter)
vecfiv<- matrix(0,1,maxiter)
veccorridas<- matrix(0,1,maxiter)


coeficor <- matrix(0,1500,6)
coeficor2 <- matrix(0,1500,3)
coeficorini <- matrix(0,1500,23)
# indicecorrectos me dir?? la posici??n exacta de los correctos para extraerlos de la matriz general
indicecorrectos <- matrix(0,1500)
# contador: veces que ya ha concluido el m??todo
contador<-0
# correctos: n??mero de veces en que obtenemos el modelo correcto al finalizar las iteraciones.
correctos <- 0
# Cerocorrectos me da el criterio para guardar o no correctos, es un indicador
cerocorrectos <- 0
#tambase define el número de observaciones a simular
tambase <- 30

# Semilla e inicio del reloj
set.seed(241024)
tic()
########################################################################################
# En el primer "while" se arma la base inicial "matini" cuyos datos utilizaremos
# para aplicar el algoritmo R3 para reducir la colinealidad y encontrar el 
# modelo correcto.
# En este primer while se establece el límite para las iteraciones. El método 
# se detiene cuando se alcanza la cota de "maxiter"
while(contador<maxiter){
  x1<- runif(tambase, 0, 100) # se simula la variable X1 y se aplica la función 
  #"basesim" para generar el resto de las variables
  matini <- basesim(x1)
  # Se completan las interacciones entre los factores mediante la función
  # "productos"
  comple<- productos(matini)
  matini<- cbind(matini,comple)
  # Simulamos la y con base en los datos del art??culo y creamos una matriz de 
  # 22 columnas y 30 observaciones
  y<- (100 + 5.7*(matini[,1]) +6.2*(matini[,2]) + 6.6*(matini[,3])
       + 7*(matini[,7]) + 5.2*(matini[,8]) + 6.1*(matini[,12])+ rnorm(tambase, mean = 0, sd = 2))
  matini<- cbind(y,matini)
  colnames(matini)<- c(variables)
  matini<- as.data.frame(matini)
  #Definimos los datos de ridge para calcular lambda optima y extraemos lambda optima
  datosridge<- cbind(y,matini[,2],matini[,3],matini[,4],matini[,8],matini[,9],matini[,13])
  colnames(datosridge)<- c(variablesridge)
  datosridge2 <- as.data.frame(datosridge)
  alpha0 <- cv.glmnet(datosridge[,-1],y,type.measure = "mse", alpha=0,family= "gaussian", nfolds = 3)
  mejorlamda<- alpha0$lambda.min
  ##### Variables iniciales######
  # "maxfiv" nos ayuda a como una variables inicial de el valor que tiene el
  # factor de inflación de la varianza máximo.
  # "corridas" llevará la cuenta de cuantas veces se aumentó la matriz 
  # (tiene como máximo 16 aumentos).
  # indmax es unicamente un vector que ayuda a que exista un valor inicial para
  # los indices de las matrices de aumentación
  maxfiv<- 10
  indmax<- c(7,9,10)
  corridas<-1
  ###############################
  # Se definen las variables sugeridas en el artículo para el modelo inicial. 
  #Al cual se le aplicará el algoritmo, así como la version bruta y con ridge
  #####Ridge####
  modeloori<- lm(y ~ X1+ X2 + X3 + X1X2 + X1X3 + X2X3, data = matini)
  ridgeopt <- lm.ridge(y ~ X1+ X2 + X3 + X1X2 + X1X3 + X2X3, data = datosridge2, lambda = mejorlamda)
  ##############
  vecmodelo<- as.formula(paste("y","~",paste(".",collapse="+"),sep=""))
  modeloi<- lm(vecmodelo, data = matini)
  modeloi101<- lm(y ~ 1, data = matini)
  #Se agrega una sección para aliminar cualquier variable con NA en el modelo
  na_vars <- names(coef(modeloi))[is.na(coef(modeloi))]
  varssinNA <- setdiff(variables, na_vars)
  varssinNA <- varssinNA[2:length(varssinNA)]
  vecmodelosinNA <- as.formula(paste("y","~",paste(varssinNA,collapse="+"),sep=""))
  #se redefine modeloi para que ya no haya variables con NA
  modeloi <- lm(vecmodelosinNA, data = matini)
  stepreg1<- stepAIC(modeloi, direction = "backward", trace = FALSE)
  stepreg2<- stepAIC(modeloi101, direction = "forward", scope = formula(modeloi), trace = FALSE)
  nom1 <- names(stepreg1$coefficients)[-1]
  nom2 <- names(stepreg2$coefficients)[-1]
  gigantevarcerostep1[contador+1,1:length(stepreg1$coefficients[-1])]<- nom1
  
  nomprob <- nom1[nom1 %in% nom2]
  if(length(nomprob)==0){
    nomprob <- c(nom1,nom2)
  }else if(length(nomprob)==1){
    nomprob <- unique(c(variables[2:7],nomprob))
  } else{
    nomprob <- nom1[nom1 %in% nom2]
  }
  vecmodelo <- as.formula(paste("y","~",paste(nomprob,collapse="+"),sep=""))
  modeloi<- lm(vecmodelo, data = matini)
  #Aqui meta algo a nom2 en caso de que entre vacia, luego se lo quito con contaresta
  if(length(nom2)==0){
    gigantevarcerostep2[contador+1,1]<- nom1[1]
    donderestar[contaresta] <- contador+1
    contaresta <- contaresta +1
  }else{
    gigantevarcerostep2[contador+1,1:length(stepreg2$coefficients[-1])]<- nom2
  }
  
  stepreg<- stepAIC(modeloi, direction = "both", trace = FALSE)
  varmodelo<- names(stepreg$coefficients)[-1]
  if(length(varmodelo)<=1){
    varmodelo <- unique(c(variables[2:7],varmodelo))
    fivini <- vif(modeloi)
    maxfivini<-max(fivini)
  }else{
    varmodelo <- names(stepreg$coefficients)[-1]
    fivini <- vif(stepreg)
    maxfivini<-max(fivini)
    }
  modcompara <- varmodelo
  # EL VIF QUE EXTRAIGO SI ES DE LA BASE PERO NO CORRESONDE 
  # AL MODELO INICIAL en caso de que los VIFS sean <=5
  if(maxfivini<=5){
    nomprob <- unique(c(nomprob,"X1","X2","X3"))
    vecmodelotentativo<- as.formula(paste("y","~",paste(nomprob,collapse="+"),sep=""))
    modeloitentativo<- lm(vecmodelotentativo, data = matini)
    stepregtentativo<- stepAIC(modeloitentativo, direction = "both", trace = FALSE)
    if(length(names(stepregtentativo$coefficients)[-1])<=1){
      maxfivini <- max(vif(modeloitentativo))
    }else{maxfivini <- max(vif(stepregtentativo))}
  }
  
  # Da la estimación inicial de y antes de aplicar R3
  #gigantevarcero[containi,2:(length(modcompara)+1)] <- modcompara
  #gigantecoefcero[containi,2:(length(modcompara)+1)] <- stepreg$coefficients[-1]
  vecyini[containi] <- stepreg$coefficients[1]
  vecfivini[containi] <-maxfivini
  containi <- containi+1
  print(stepreg1)
  print(stepreg2)
  print(vecmodelo)
  print(vecyini[1])
  print(vecfivini[1])
  #checar si en vez de usaqr el varmodelo, que sucede si uso el tentativo para los casos de maxfivini <=5
  # Aquí inicia la aplicación del método. Se cuenta con 2 criterios para 
  # detener el algoritmo:
  # 1.- Que la matriz se aumenta más de 16 veces
  # 2.- Mientras el factor de inflacion de la varianza de X1, X2 o X3 sea 
  # mayor o igual a 5
  while(corridas<=10 && maxfiv>=5){
    
    vecmodelo<- as.formula(paste("y","~",paste(varmodelo,collapse="+"),sep=""))
    modeloi<- lm(vecmodelo, data = matini)
    #stepreg<- stepAIC(modeloi, direction = "both", trace = FALSE)
    stepreg<- stepAIC(modeloi, direction = "both", scope = list(upper = ~ X1+X2+X3+X4+X5+X6+X1X2+X1X3+X1X4+X1X5+X1X6+X2X3+X2X4+X2X5+X2X6+X3X4+X3X5+X3X6+X4X5 +X4X6+X5X6), trace = FALSE)
    
    #varmodelo<-unique(c(obligados,names(stepreg$coefficients[-1])))
    # Después de obtener las variables arrojadas por la regresi??n paso a paso 
    # se definen cuales e utilizaran para la siguiente iteración ( se agrega el 
    # código que para realizar, el método igual que en el artículo para 
    #verificar que no nos da los resultados deseados).
    varmodelo<- names(stepreg$coefficients)[-1]
    if(length(varmodelo)<=1){
      varmodelo <- unique(c(variables[2:7],varmodelo))
    }else{varmodelo <- names(stepreg$coefficients)[-1]}
    #Gaurdamos la aproximación inicial en una matriz#####################
    if(corridas==1){
      gigantevarcero[contador+1,1:length(stepreg$coefficients[-1])]<- varmodelo
      gigantecoefcero[contador+1,1:length(stepreg$coefficients[-1])]<- stepreg$coefficients[-1]
    }
    # Extrae el factor de inflación de la varianza de la regresión paso a paso
    if(length(stepreg$coefficients[-1])<=1){
      fiv <- vif(modeloi)
    }else{fiv<- vif(stepreg)}
    
    secu<- seq(1:6)
    # Se aplica la función "correla" y se obtiene un vector con indices que 
    #indican la posición del vector columna de la matriz "matmisra1" o 
    #"matmisra2", segun sea el caso, que se asignará para aumentar la matriz
    indpasado<-indmax
    indmax<-correla(matini,stepreg)
    topeinmax<- length(indmax)
    # Si se repite el indice cambiamos la matriz misra para inducir la 
    # ortogonalidad
    if(length(indpasado)==topeinmax){
      if(sum(indpasado==indmax)==topeinmax ){
        matmisra <- matmisra2
      }else{matmisra <- matmisra1}
    }else{matmisra <- matmisra1}
    
    # Un vector de ceros de 2x6 que se va a rellenar con los valores de las
    #matrices "matmisra" y "matdefault".
    dimvec <- 2
    aumen<- matrix(0,dimvec,6)
    indfijo<-secu[-indmax]
    topeinfijo<- length(indfijo)
    
    # Se rellena la matriz "aumen" utilizando "indmax" y "matmisra"
    for (i in 1:topeinmax) {
      aumen[,indmax[i]]<-matmisra[,i]
    }
    
    aumen <- decouple2f(indfijo,indmax,aumen)
    # Se realizan las interacciones de la matriz aumentada mediante la función 
    #"productos" para posteriormente construir a la variable dependiente 
    aumen2<- productos(aumen)
    aumen<- cbind(aumen,aumen2)
    y<- (100 + 5.7*(aumen[1:dimvec,1]) +6.2*(aumen[1:dimvec,2]) + 6.6*(aumen[1:dimvec,3])
         + 7*(aumen[1:dimvec,7]) + 5.2*(aumen[1:dimvec,8]) + 6.1*(aumen[1:dimvec,12])+ rnorm(dimvec, mean = 0, sd = 2))
    aumen<- cbind(y,aumen)
    colnames(aumen)<- c(variables)
    matini<- rbind(matini,aumen)
    # Actualizamos el valor de corridas y del factor de inflación de la varianza
    #máximo
    corridas<- corridas + dimvec
    
    #Secundarios son las variables x4 x5 y x6
    # Escojo el VIF de los efectos principales
    multi<- 1:length(fiv)
    if(sum(as.numeric(names(fiv) %in% obligados))>=1){
      maxfiv<-max(fiv[as.numeric(names(fiv) %in% obligados)*multi])
    }else if(sum(as.numeric(names(fiv) %in% secundarios))>=1){
      maxfiv<-max(fiv[as.numeric(names(fiv) %in% secundarios)*multi])
    }else{maxfiv<-max(fiv[1:sum(as.numeric(names(fiv) %in% obligados))])}
    print(indmax)
    print(indfijo)
    print(aumen)
    print(stepreg)
    print(vif(stepreg))
    print(maxfiv)
  }
  
  varmodelo<- sort(c(names(stepreg$coefficients)[-1]))

  tvarmod <- length(varmodelo)
  #Se verifica cuantos modelos fueron correctos
  if(tvarmod==6){
    if( sum(elreal==varmodelo)==6){
      correctos <- correctos + 1
      coeffinales <- stepreg$coefficients[-1][order(c(names(stepreg$coefficients[-1])))]
      coeficor[correctos,] <- coeffinales
      coeficor2[correctos,] <- c(stepreg$coefficients[1],corridas,maxfiv)
      coeficorini[correctos,1] <- maxfivini
      coeficorini[correctos,2:(length(modcompara)+1)] <- modcompara
    }
    
  }
  #Se actualizan los valores del número de simulaciones y se guardan los 
  #valores finales obtenidos para la i-ésima base de datos.
  contador<- contador +1
  #print((contador))
  #Aqui se crea el vector para extraer las estimaciones correctas, estimacion 
  #ridge y del modelo original
 if(correctos>cerocorrectos){
   indicecorrectos[correctos] <- contador
   cerocorrectos <- cerocorrectos + 1
 }
  gigante[contador,1:tvarmod] <- varmodelo
  vecyfinal[contador] <- stepreg$coefficients[1]
  vecfiv[contador]<- maxfiv
  ridgegigante[contador,] <- coef(ridgeopt)
  reggigante[contador,] <- coef(modeloori)
  veccorridas[contador]<- corridas - 1
}
toc()
print(correctos)
# 7 horas, 9 minutos y 34 segundos
# Aqui se reduce el vector de correctos para eliminar los ceros
indicecorrectos <- indicecorrectos[1:correctos]
#Verifica si alg??n factor de inflacion de la varianza fue mayor a 5
contador
#vecfiv[vecfiv>5]
length(vecfiv[vecfiv>5])/contador
#Verifica si se realizaron m??s de 10 aumentos a la matriz "matini"
#veccorridas[veccorridas>=10]
# N??mero de modelos correctos
correctos
correctos/contador

sort(table(gigante), decreasing = TRUE)[-1] 
sort(table(gigantevarcero), decreasing = TRUE)[-1] 
graffinal <- sort(table(gigante), decreasing = TRUE)[-1] 
grafinicial <- sort(table(gigantevarcero), decreasing = TRUE)[-1]
grafinicialback <- sort(table(gigantevarcerostep1), decreasing = TRUE)[-1]
grafinicialforw <- sort(table(gigantevarcerostep2), decreasing = TRUE)[-1]
#Al menos en este porcentage aparece una variable correcta
min(sort(table(gigante), decreasing = TRUE)[-1][1:6])/contador


#Graficar frecuencia
barplot(grafinicialback, col = "skyblue",width = 1.5,
        xlab = "Variables", ylab = "Frecuencia",
        cex.names = 0.8, las = 2 ) + abline(h = grafinicialback[7], col = "red", lty = 2, lwd = 2)

barplot(grafinicialforw, col = "skyblue",width = 1.5,
        xlab = "Variables", ylab = "Frecuencia",
        cex.names = 0.8, las = 2 ) + abline(h = grafinicialforw[7], col = "red", lty = 2, lwd = 2)

barplot(grafinicial, col = "skyblue",width = 1.5,
        xlab = "Variables", ylab = "Frecuencia",
        cex.names = 0.8, las = 2 ) + abline(h = grafinicial[7], col = "red", lty = 2, lwd = 2)

barplot(graffinal, col = "skyblue",width = 1.5,
        xlab = "Variables", ylab = "Frecuencia",
        cex.names = 0.8, las = 2 ) + abline(h = graffinal[7], col = "red", lty = 2, lwd = 2)

#Las primeras 6 variables aparecen un XX de veces
mean(grafinicialback[1:6])
#Mientras que las otras aparecen un XX de veces
mean(grafinicialback[7:21])
#Promedio de variables del modelo backward
numerovarback <- (420000-sum(gigantevarcerostep1[1:20000,]==0))/20000
numerovarback

#Las primeras 6 variables aparecen un XX de veces
mean(grafinicialforw[1:6])
#Mientras que las otras aparecen un XX de veces
mean(grafinicialforw[7:21])
#Promedio de variables del modelo forward
numerovarforw <- (420000-sum(gigantevarcerostep2[1:20000,]==0))/20000
numerovarforw

#Las primeras 6 variables aparecen un XX de veces
mean(grafinicial[1:6])
#Mientras que las otras aparecen un XX de veces
mean(grafinicial[7:21])
#Promedio de variables del modelo stepwise
numerovarstep <- (420000-sum(gigantevarcero[1:20000,]==0))/20000
numerovarstep

#Las primeras 6 variables aparecen un XX de veces
mean(graffinal[1:6])
#Mientras que las otras aparecen un XX de veces
mean(graffinal[7:21])
#Promedio de variables del modelo Final
numerovarfinal <- (420000-sum(gigante[1:20000,]==0))/20000
numerovarfinal


#En promedio se hicieron XX corridas
mean(veccorridas)/2
#Para estos se aumento la colinealidad
variacionVIF <- (vecfivini[1:maxiter]-vecfiv[1:maxiter])/vecfivini[1:maxiter]
length(variacionVIF[variacionVIF<0])/contador

length(vecfivini[vecfivini<=5])
lll <- vecfiv[vecfivini<=5]
# El metodo R3 aumento la colinealidad en este porcentage de la simulaci??n
#length(lll[lll>5])/length(vecfivini[vecfivini<=5])
# Este porcentage de la muestra presenta VIF mayores a 5
length(vecfiv[vecfiv>5])/contador
mean(vecfiv[vecfiv>5])

# Para aquellos modelos en los que NO aumento la colinealidad, esta se redujo en un ... porciento
mean(variacionVIF[variacionVIF>0])

# De los modelos correctos, se redujo la colinealidad en un ... porciento
#reducecoli<- (vecfivini[indicecorrectos]-vecfiv[indicecorrectos])/vecfivini[indicecorrectos]
#mean(reducecoli)

#################################################################################################################
coefsreales <- c(5.70,7,5.2,6.2,6.1,6.6)
coefssim <- c(mean(coeficor[1:correctos,1]),mean(coeficor[1:correctos,2]),mean(coeficor[1:correctos,3]),
              mean(coeficor[1:correctos,4]),mean(coeficor[1:correctos,5]),mean(coeficor[1:correctos,6]))
difabsoluta<- abs(coefsreales-coefssim)
difrelativa <- (difabsoluta/coefsreales)*100
names(difabsoluta)<- c(elreal)
names(difrelativa)<- c(elreal)

ridgefinales <- c(mean(ridgegigante[indicecorrectos,][,1]),mean(ridgegigante[indicecorrectos,][,2]),
                  mean(ridgegigante[indicecorrectos,][,5]),mean(ridgegigante[indicecorrectos,][,6]),
                  mean(ridgegigante[indicecorrectos,][,3]),mean(ridgegigante[indicecorrectos,][,7]),
                  mean(ridgegigante[indicecorrectos,][,4]))
difridgeabs <- abs(coefsreales-ridgefinales[-1])
difridgerel <- (difridgeabs/coefsreales)*100
names(difridgeabs)<- c(elreal)
names(difridgerel)<- c(elreal)

regbrutafinales <- c(mean(reggigante[indicecorrectos,][,1]),mean(reggigante[indicecorrectos,][,2]),
                  mean(reggigante[indicecorrectos,][,5]),mean(reggigante[indicecorrectos,][,6]),
                  mean(reggigante[indicecorrectos,][,3]),mean(reggigante[indicecorrectos,][,7]),
                  mean(reggigante[indicecorrectos,][,4]))
difbrutaabs <- abs(coefsreales-regbrutafinales[-1])
difbrutarel <- (difbrutaabs/coefsreales)*100
names(difbrutaabs)<- c(elreal)
names(difbrutarel)<- c(elreal)
difrelativa
difridgerel
difbrutarel
mean(difrelativa)
mean(difridgerel)
mean(difbrutarel)

ridgecompleto <- c(mean(ridgegigante[,2]),mean(ridgegigante[,5]),
                   mean(ridgegigante[,6]),mean(ridgegigante[,3]),
                   mean(ridgegigante[,7]),mean(ridgegigante[,4]))
difridgecomabs<- abs(coefsreales-ridgecompleto)
difridgecomrel <- (difridgecomabs/coefsreales)*100
names(difridgecomabs)<- c(elreal)
names(difridgecomrel)<- c(elreal)
mean(difridgecomrel)


mean(vecfivini[indicecorrectos])
sort(table(gigantevarcero[indicecorrectos,]), decreasing = TRUE)[-1] 
#VS
mean(vecyfinal[indicecorrectos])
mean(vecfiv[indicecorrectos])
sort(table(gigante[indicecorrectos,]), decreasing = TRUE)[-1]



#Varianzas en las estimaciones
varcoefssim <- c(var(coeficor[1:correctos,1]),var(coeficor[1:correctos,2]),var(coeficor[1:correctos,3]),
                 var(coeficor[1:correctos,4]),var(coeficor[1:correctos,5]),var(coeficor[1:correctos,6]))


