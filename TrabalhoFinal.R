#Etapas do código. 

#Statistical pre­processing
#Statistical pre­processing part II: modality and outliers 
#Statistical pre­processing part III: re­scaling.
#Statistical pre­processing part IV: correlation matrix.
#PCA Part I: the components to keep.
#PCA Part II: individuals as principal components.

borboletas <- read.csv("~/Documents/SIM-Trabalho/butterfly_three_temp.csv", sep = ";")



borboletas <- na.omit(borboletas) #Se houver NA's apaga-os


#Se existem missing values - Não.
#install.packages("missMDA")
#library(missMDA)
#Como não existem missing values, nao se vai fazer nada, mas se existisse corria-se este codigo:
#comp <- imputePCA(borboletas, ncp=5, scale=TRUE)
#borboletas <- comp$TrasectSize


dim(borboletas)[1] == dim(na.omit(borboletas))[1] #da TRUE (missing values)

dim(borboletas)[1] != dim(na.omit(borboletas))[1] #da FALSE (missing values)


#2.1.1 separar por subpopulação
temperatures <- unique(borboletas[,"Temperature"]) #variavel unica temperatures
ds1 <- borboletas[borboletas$Temperature==temperatures[1],] #sub população ds1
ds2 <- borboletas[borboletas$Temperature==temperatures[2],] #sub população ds2
ds3 <- borboletas[borboletas$Temperature==temperatures[3],] #sub população ds3

#verificar unimodalidade para todas as temperaturas
hist(ds1[,2], breaks = 30)
hist(ds1[,3], breaks = 30) #unimodal
hist(ds1[,4], breaks = 30)
hist(ds1[,5], breaks = 30)
hist(ds1[,6], breaks = 30)
hist(ds1[,7], breaks = 30)
hist(ds2[,2], breaks = 30)
hist(ds2[,3], breaks = 30)
hist(ds2[,4], breaks = 30)
hist(ds2[,5], breaks = 30)
hist(ds2[,6], breaks = 30)
hist(ds2[,7], breaks = 30)
hist(ds3[,2], breaks = 30)
hist(ds3[,3], breaks = 30)
hist(ds3[,4], breaks = 30)
hist(ds3[,5], breaks = 30)
hist(ds3[,6], breaks = 30)
hist(ds3[,7], breaks = 30)


install.packages("diptest")
library(diptest) #quando o <d> é <0.05 é unimodal
dip.test(ds1[,2])
dip.test(ds1[,3])
dip.test(ds1[,4])
dip.test(ds1[,5])
dip.test(ds1[,6])
dip.test(ds1[,7])
dip.test(ds2[,2])
dip.test(ds2[,3])
dip.test(ds2[,4])
dip.test(ds2[,5])
dip.test(ds2[,6])
dip.test(ds2[,7])
dip.test(ds3[,2])
dip.test(ds3[,3]) 
dip.test(ds3[,4])
dip.test(ds3[,5])
dip.test(ds3[,6])
dip.test(ds3[,7])

#source ("https://datascienceplus.com/rscript/outlier.R")
#os outliers sao removidos mas tem-se de substituir por NA para depois correr..
#Exemplo.. borboletas <- na.omit(borboletas)


colnames(borboletas) #mas já sabemos que são 6 variaveis, mas serve para ver os boxplots

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  #response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  #if(response == "y" | response == "yes"){
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  message("Outliers successfully removed", "\n")
  return(invisible(dt))
  #} else{
  #  message("Nothing changed", "\n")
  #  return(invisible(var_name))
  #}
}
# Exemplos para ver os ouliers em plot
#boxplot(borboletas)
#boxplot(ds1[,"TransectSize"] #ver grafico

ds1 <- as.data.frame(ds1) #script do outliers requer este comando dataframe

#remover outliers da subpopulacao ds1 = t15
boxplot(ds1[,"TransectSize"]) #Ver o plot com outliers
outlierKD(ds1, TransectSize)
ds1 <- na.omit(ds1)
boxplot(ds1[,"TransectSize"]) #Ver o plot sem outliners

#Para ver o plot dos outliers adicionar o boxplot como no exemplo acima.
#na.omit é executado depois de cada outlier removido, para previnir valores NA's
#imputePCA lida com os missing values

outlierKD(ds1, CellHeight)
ds1 <- na.omit(ds1)
outlierKD(ds1, WhiteDiameter)
ds1 <- na.omit(ds1)
outlierKD(ds1, BlackDiameter)
ds1 <- na.omit(ds1)
outlierKD(ds1, GoldDiameter)
ds1 <- na.omit(ds1)
outlierKD(ds1, TotalEyeDiameter)
ds1 <- na.omit(ds1)

#remover outliers da subpopulacao ds2 = t23
ds2 <- as.data.frame(ds2)
outlierKD(ds2, TransectSize)
ds2 <- na.omit(ds2)
outlierKD(ds2, CellHeight)
ds2 <- na.omit(ds2)
outlierKD(ds2, WhiteDiameter)
ds2 <- na.omit(ds2)
outlierKD(ds2, BlackDiameter)
ds2 <- na.omit(ds2)
outlierKD(ds2, GoldDiameter)
ds2 <- na.omit(ds2)
outlierKD(ds2, TotalEyeDiameter)
ds2 <- na.omit(ds2)

#remover outliers da subpopulacao ds3 = t29
ds3 <- as.data.frame(ds3)
outlierKD(ds3, TransectSize)
ds3 <- na.omit(ds3)
outlierKD(ds3, CellHeight)
ds3 <- na.omit(ds3)
outlierKD(ds3, WhiteDiameter)
ds3 <- na.omit(ds3)
outlierKD(ds3, BlackDiameter)
ds3 <- na.omit(ds3)
outlierKD(ds3, GoldDiameter)
ds3 <- na.omit(ds3)
outlierKD(ds3, TotalEyeDiameter)
ds3 <- na.omit(ds3)





#juntar as sub populações 
borboletas<-rbind(ds1,ds2,ds3)


#Rescale
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
borboletas[,2:7] <- as.data.frame(
  lapply(as.data.frame(borboletas[,2:7]), normalize))


library(corrplot)

#Fazer a correlação
M <-cor(borboletas[,2:7])
corrplot(M, type = "upper",method = "circle")

A <- cor (ds1[,2:7])
corrplot(A,type="upper", method="circle")

B <- cor (ds2[,2:7])
corrplot(B,type="upper", method="circle")

C <- cor (ds3[,2:7])
corrplot(C,type="upper", method="circle")


#Esta pronto pa aplicar o PCA


#PCA
library(FactoMineR)
res.pca<-PCA(borboletas, quali.sup = 1, graph = F)


summary(res.pca) #decidir quantas ficam
res.pca<-PCA(borboletas, quali.sup = 1, graph = F, ncp = 4)

#ver dimensões
dimdesc(res.pca, axes = 1:4)#ver contribuiçoes das variaveis para as dimenssoes
#mostrar os graficos
fviz_screeplot(res.pca, ncp=4) #desenha gŕafico

plot.PCA(res.pca, axes = c(1,2), habillage=1, label="none")
plot.PCA(res.pca, axes = c(1,3), habillage=1, label="none")
plot.PCA(res.pca, axes = c(1,4), habillage=1, label="none")

plot.PCA(res.pca, axes = c(2,3), habillage=1, label="none")
plot.PCA(res.pca, axes = c(2,4), habillage=1, label="none")
plot.PCA(res.pca, axes = c(3,4), habillage=1, label="none")


library(factoextra)

fviz_screeplot(res.pca, ncp=4) #desenhar grafico final

##FIM

