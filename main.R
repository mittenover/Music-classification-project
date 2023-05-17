######Projet final STA203 - Apprentissage supervisé########
###########################################################

# Nom1 = Limes    
# Nom2 = Perdrix

rm(list=objects()); graphics.off()

setwd("C:/Users/lucie/OneDrive/Documents/Documents/ENSTA/2A/STA203/PROJETSTA203")

#Package
library(ggplot2)
library(GGally)
library(corrplot)
library(caret)


#Importation des données
df=read.table("Music_2023.txt",dec='.',header=TRUE,sep=';')

##########################################################################################################################################
##########################################################################################################################################
##########################################################Partie I########################################################################
##########################################################################################################################################
##########################################################################################################################################

################
### Q1
################
df$GENRE=as.numeric(factor(df$GENRE))-1
####Analyse univarié
summary(df)  #Toutes les variables sauf la dernière sont numériques
str(df)


####Analyse bivariée 
corrplot(cor(df[,-192])) #Pas vraiment exploitable


####Proportion de chacun des genres
p_classique=sum(df$GENRE=="Classical")/length(df$GENRE)  #0.53
p_jazz=sum(df$GENRE=="Jazz")/length(df$GENRE)            #0.47

####PAR_SC_V, PAR_ASC_V
myplot = function(x,Y,xlab=""){
  plot(x,Y,xlab=xlab, col=Y+1,pch=Y+1);
  boxplot(x~Y,xlab=xlab,horizontal=TRUE)
}
par(mfcol=c(2,1))
summary(df$PAR_SC_V)
mean(df$PAR_SC_V)      #105222.8
myplot(log(df$PAR_SC_V),df$GENRE)

summary(df$PAR_ASC_V)
mean(df$PAR_ASC_V)     #0.4251288
myplot(log(df$PAR_ASC_V),df$GENRE)
#Le passage au log peut-être judicieux pour normaliser les valeurs
df$PAR_SC_V=log(df$PAR_SC_V)
df$PAR_ASC_V=log(df$PAR_ASC_V)

####D'après l'annexe, les paramètres 148 à 167 sont les mêmes que les paramètres 128 à 147 donc on peut les supprimer
df=cbind(df[1:147],df[168:192])


####Variable très corrélées 
C=cor(df[,-172])-diag(1,171)
cbind(which(C>0.99)%/%171 +1, which(C>0.99)%%171) 
c(C[36,37],C[71,72],C[160,164])
#0.9997459 0.9981005 0.9950279


#Suppression des variables
df=df[,-c(37,72,160)]

####Cas des variables mentionnées
#Ces variables représentent d'après l'énoncé des moyennes, elles n'apportent donc rien au modèle on peut les supprimer

df=df[,-c(37,71,100,125)]

dev.off()
####Définition du modèle logistique

model = function(formule,data)
{
  return(glm(formule, family=binomial(link="logit"),data=data))
}
#Binomial car on veut expliquer une variable binaire 

################
### Q2
################
set.seed(103)
n=nrow(df)
train=sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))

################
### Q3
################


####Modele0####
###############
formule0<-GENRE~PAR_TC + PAR_SC + PAR_SC_V 
Mod0<-model(formule0,data=df[train==TRUE,])

####ModeleT####
###############
formuleT<-GENRE~.
ModT<-model(formuleT,data=df[train==TRUE,])

####Modele1####
###############

####Récupération des variables significatives
library(stats)
ind_variables_sign=which(summary(ModT)$coefficients[,4]<=0.05)
names(df)[ind_variables_sign]

####Mis à jour du modèle en supprimant les variables significatives
formule1<-GENRE~PAR_TC+PAR_SC_V+PAR_ASE2+PAR_ASE3+PAR_ASE5+PAR_ASE13+PAR_ASE16+PAR_ASE17+PAR_ASE21+PAR_ASE22+PAR_ASE23+PAR_ASE25+PAR_ASE29+PAR_ASE31+PAR_ASE32+
  PAR_ASEV11+PAR_ASEV24+PAR_ASEV25+PAR_ASEV26+PAR_ASEV29+PAR_ASEV31+PAR_ASC+PAR_ASC_V+PAR_ASS+PAR_SFM1+PAR_SFM4+PAR_SFM7+PAR_SFM8+PAR_SFM10+PAR_SFM11+PAR_SFM12+PAR_SFM15+
  PAR_SFM17+PAR_SFM19+PAR_SFM20+PAR_SFM23+PAR_SFMV1+PAR_SFMV5+PAR_SFMV7+PAR_SFMV10+PAR_SFMV11+PAR_SFMV13+PAR_SFMV16+PAR_SFMV17+PAR_SFMV19+PAR_SFMV20+PAR_SFMV21+PAR_SFMV24+
  PAR_MFCC1+PAR_MFCC2+PAR_MFCC3+PAR_MFCC4+PAR_MFCC5+PAR_MFCC7+PAR_MFCC9+PAR_MFCC10+PAR_MFCC11+PAR_MFCC13+PAR_MFCC20+PAR_THR_1RMS_TOT+PAR_THR_2RMS_TOT+PAR_THR_2RMS_10FR_VAR+
  PAR_THR_3RMS_10FR_MEAN+PAR_THR_3RMS_10FR_VAR+PAR_PEAK_RMS10FR_VAR 
Mod1=model(formule1,data=df[train==TRUE,])
  
####Modele2####
###############

####Récupération des variables significatives
ind_variables_sign=which(summary(ModT)$coefficients[,4]<=0.2)
names(df)[ind_variables_sign]

####Mis à jour du modèle en supprimant les variables non-significatives
formule2<-GENRE~PAR_TC+PAR_SC_V+PAR_ASE2+PAR_ASE3+PAR_ASE4+PAR_ASE5+PAR_ASE6+PAR_ASE8+PAR_ASE13+PAR_ASE16+PAR_ASE17+PAR_ASE18+PAR_ASE19+PAR_ASE21+PAR_ASE22+PAR_ASE23+PAR_ASE24+
  PAR_ASE25+PAR_ASE28+PAR_ASE29+PAR_ASE31+PAR_ASE32+PAR_ASEV2+PAR_ASEV7+PAR_ASEV8+PAR_ASEV9+PAR_ASEV10+PAR_ASEV11+PAR_ASEV14+PAR_ASEV19+PAR_ASEV21+PAR_ASEV22+PAR_ASEV24+
  PAR_ASEV25+PAR_ASEV26+PAR_ASEV29+PAR_ASEV30+PAR_ASEV31+PAR_ASC+PAR_ASC_V+PAR_ASS+PAR_SFM1+PAR_SFM3+PAR_SFM4+PAR_SFM6+PAR_SFM7+PAR_SFM8+PAR_SFM9+PAR_SFM10+PAR_SFM11+PAR_SFM12+
  PAR_SFM15+PAR_SFM17+PAR_SFM19+PAR_SFM20+PAR_SFM23+PAR_SFMV1+PAR_SFMV5+PAR_SFMV7+PAR_SFMV10+PAR_SFMV11+PAR_SFMV13+PAR_SFMV16+PAR_SFMV17+PAR_SFMV18+PAR_SFMV19+PAR_SFMV20+PAR_SFMV21+
  PAR_SFMV22+PAR_SFMV24+PAR_MFCC1+PAR_MFCC2+PAR_MFCC3+PAR_MFCC4+PAR_MFCC5+PAR_MFCC7+PAR_MFCC9+PAR_MFCC10+PAR_MFCC11+PAR_MFCC12+PAR_MFCC13+PAR_MFCC17+PAR_MFCC19+PAR_MFCC20+PAR_THR_1RMS_TOT+
  PAR_THR_2RMS_TOT+PAR_THR_3RMS_TOT+PAR_THR_1RMS_10FR_VAR+PAR_THR_2RMS_10FR_VAR+PAR_THR_3RMS_10FR_MEAN+PAR_THR_3RMS_10FR_VAR+PAR_PEAK_RMS10FR_VAR 
Mod2=model(formule2,data=df[train==TRUE,])

####ModAIC####
###############
library(MASS)
ModAIC=stepAIC(glm(GENRE~.,family=binomial(link="logit"),data=df[train==TRUE,]),direction="both")


################
### Q4
################
library(ROCR)
library(pROC)

####Premier graph
#Echantillon apprentissage
pred1=predict(ModT)
ROC_T_train=roc(df$GENRE[train==TRUE],pred1)
plot(ROC_T_train, xlab="", col="blue",main="courbes ROC : ModT apprentissage")
segments(x0=1,y0=0,x1=1,y1=1,col="red")
segments(x0=1,y0=1,x1=0,y1=1,col="red")
legend(0.6,0.2,legend=c("ModT : Apprentissage","Règle aléatoire","Règle parfaite"),col=c("blue","grey","red"),lty=1)

#Echantillon test
pred2=predict(ModT,newdata=df[train==FALSE,])
ROC_T_test=roc(df$GENRE[train==FALSE],pred2)
plot(ROC_T_test, xlab="", col="purple",main="courbes ROC : Test")
segments(x0=1,y0=0,x1=1,y1=1,col="red")
segments(x0=1,y0=1,x1=0,y1=1,col="red")
legend(0.6,0.2,legend=c("ModT : Test","Règle aléatoire","Règle parfaite"),col=c("purple","grey","red"),lty=1)

#Superposition apprentissage / test
plot(ROC_T_train, xlab="", col="blue",main="courbes ROC : ModT")
lines(ROC_T_test, xlab="", col="purple",)
segments(x0=1,y0=0,x1=1,y1=1,col="red")
segments(x0=1,y0=1,x1=0,y1=1,col="red")
legend(0.6,0.3,legend=c("ModT : Apprentissage","ModT : Test","Règle aléatoire","Règle parfaite"),col=c("blue","purple","grey","red"),lty=1)

#Superpositions autres modèles
pred=predict(ModT,newdata=df[train==FALSE,])
ROC_T_test=roc(df$GENRE[train==FALSE],pred)
plot(ROC_T_test, xlab="", col=1,main="Superposition courbes ROC")

pred=predict(Mod1,newdata=df[train==FALSE,])
ROC_1_test=roc(df$GENRE[train==FALSE],pred)
lines(ROC_1_test, xlab="", col=2,main="Superposition courbes ROC")

pred=predict(Mod2,newdata=df[train==FALSE,])
ROC_2_test=roc(df$GENRE[train==FALSE],pred)
lines(ROC_2_test, xlab="", col=3,main="Superposition courbes ROC")

pred=predict(Mod0,newdata=df[train==FALSE,])
ROC_0_test=roc(df$GENRE[train==FALSE],pred)
lines(ROC_0_test, xlab="", col=4,main="Superposition courbes ROC")

pred=predict(ModAIC,newdata=df[train==FALSE,])
ROC_AIC_test=roc(df$GENRE[train==FALSE],pred)
lines(ROC_AIC_test, xlab="", col=4,main="Superposition courbes ROC")

legend(0.8,0.3,legend=c(paste("ModT :",toString(auc(ROC_T_test))),paste("Mod1 :",toString(auc(ROC_1_test))),paste("Mod2 :",toString(auc(ROC_2_test))),paste("Mod0 :",toString(auc(ROC_1_test))),paste("ModAIC :",toString(auc(ROC_AIC_test)))), col=c(1:5),lty=1)
png("Superpositionmodele.png")

################
### Q4
################
error_classif=function(data,modele,seuil=0.5)
{
  predproba = predict(modele,type="response",newdata=data)
  glm.pred = ifelse(predproba>seuil,1,0)
  return(1-mean(glm.pred==data$GENRE))
}
##ModT
#Apprentissage
error_classif(df[train==TRUE,],ModT,0.5) #0.07305936
#Test
error_classif(df[train==FALSE,],ModT,0.5) #0.09783368
##Mod0
#Apprentissage
error_classif(df[train==TRUE,],Mod0,0.5) #0.3463295
#Test
error_classif(df[train==FALSE,],Mod0,0.5) #0.3584906
##Mod1
#Apprentissage
error_classif(df[train==TRUE,],Mod1,0.5) #0.1306639
#Test
error_classif(df[train==FALSE,],Mod1,0.5) #0.1439553
##Mod2
#Apprentissage
error_classif(df[train==TRUE,],Mod2,0.5) #0.1060766
#Test
error_classif(df[train==FALSE,],Mod2,0.5) #0.1306778

##ModAIC
#Apprentissage
error_classif(df[train==TRUE,],ModAIC,0.5) #0.07727432
#Test
error_classif(df[train==FALSE,],ModAIC,0.5) #0.09923131



###########################################################################################################################################
##########################################################################################################################################
##########################################################Partie II#######################################################################
##########################################################################################################################################
##########################################################################################################################################
################
### Q1
################

################
### Q2
################
library(glmnet)
df=read.table("Music_2023.txt",dec='.',header=TRUE,sep=';')
grid = 10^seq(10,-2,length=100)
x = as.matrix(df[,-192])
y = as.numeric(factor(df$GENRE))-1
ridge.fit = glmnet(x,y,alpha=0,lambda=grid)

plot(ridge.fit)

################
### Q3
################


set.seed(314)
n=nrow(df)
train=sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))
CV=cv.glmnet(x,y,alpha=0,lambda=grid,nfolds=10)
lambda=CV$lambda.min #0.01

y.test = y[train==FALSE]
Mod=glmnet(x[train==TRUE,],y[train==TRUE],alpha=0,lambda=grid)

#Calcul de l'erreur de prédiction
ridge.pred = predict(Mod,s=lambda,newx=x[train==FALSE,])    
glm.pred = ifelse(ridge.pred>0.5,1,0)
mean(glm.pred!=y.test)   #0.09652236
mean((glm.pred-y.test)^2) #0.09652236


################
### Q4
################


set.seed(4658)
CV=cv.glmnet(x,y,alpha=0,lambda=grid,nfolds=10)
lambda=CV$lambda.min #0.01

Mod=glmnet(x[train==TRUE,],y[train==TRUE],alpha=0,lambda=lambda)

#Calcul de l'erreur de prédiction
ridge.pred = predict(Mod,s=lambda,newx=x[train==FALSE,])   
glm.pred = ifelse(ridge.pred>0.5,1,0)
mean(glm.pred!=y.test)   #0.0979418
mean((glm.pred-y.test)^2) #0.0979418



##########################################################################################################################################
##########################################################################################################################################
##########################################################Partie III######################################################################
##########################################################################################################################################
##########################################################################################################################################
################
### Q1
################

library(MASS)
library(class)
#Mise en forme données
df=read.table("Music_2023.txt",dec='.',header=TRUE,sep=';')
x=df[,-192]
xtrain=x[train==TRUE,]
xtest=x[train==FALSE,]
y=as.factor(df[,192])
ytrain=y[train==TRUE]
ytest=y[train==FALSE]

#k=1
modk1_train=knn(train=xtrain,test=xtrain,cl=ytrain,k=1)
err1_train=mean(modk1_train!=ytrain) #0

modk1_test=knn(train=xtrain,test=xtest,cl=ytrain,k=1)
err1_test=mean(modk1_test!=ytest) #0.3654787

#Choix du bon k
library(doParallel)
K=1:500
nK=length(K)

ErrTrain=rep(NA,length=nK)
ErrTest=rep(NA,length=nK)
cl<-makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)
for(i in 1:nK)
{
  k=K[i]
  
  modtrain=knn(xtrain,xtrain,k=k,cl=ytrain)
  ErrTrain[i]=mean(modtrain!=ytrain)
  modtest=knn(xtrain,xtest,k=k,cl=ytrain)
  ErrTest[i]=mean(modtest!=ytest)
  print(k)
}
stopCluster(cl)

K=K[1:500]
plot(K,ErrTest,type="b",col="blue",xlab="nb de voisins",ylab="erreurs train et erreurs test",pch=20,
     ylim=range(c(ErrTest,ErrTrain)))
lines(K,ErrTrain,type="b",col="red",pch=20)

legend("bottomright",lty=1,col=c("red","blue"),legend=c("train","test"))


which.min(ErrTrain)
which.min(ErrTest)  #k=1


#Le modèle avec le moins d'erreur est le modèle k=1.
#On a donc un cas d'underfitting (risque de biais). 
#On voit que le modèle est peu performant avec une erreur de test de 0.36

##########################################################################################################################################
##########################################################################################################################################
##########################################################Conclusion######################################################################
##########################################################################################################################################
##########################################################################################################################################

#D'après les résultats précédents le modèle le plus performant en généralisation est le modèle de régression ridge avec une erreur de classification 
#de 0.09652236.

test=read.table("Music_test.txt",dec='.',header=TRUE,sep=';')
x=as.matrix(test)
pred=predict(Mod,s=lambda,newx=x)
pred=ifelse(pred>0.5,1,0)
pred=ifelse(pred==1,"Jazz","Classical")
write.table(pred,"LIMES-PERDRIX_test.txt")
