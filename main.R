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

####Analyse univarié
summary(df)  #Toutes les variables sauf la dernière sont numériques
str(df)


####Analyse bivariée (à voir)
#ggpairs(df[,-192],ggplot2::aes(colour=df$GENRE))

#pairs(X[,-192])   #On enlève la première colonne pour l'analyse
#corrplot(cor(X[,-192]))


####Proportion de chacun des genres
p_classique=sum(df$GENRE=="Classical")/length(df$GENRE)  #0.53
p_jazz=sum(df$GENRE=="Jazz")/length(df$GENRE)            #0.47

####PAR_SC_V, PAR_ASC_V
summary(df$PAR_SC_V)
mean(df$PAR_SC_V)      #105222.8

summary(df$PAR_ASC_V)
mean(df$PAR_ASC_V)     #0.4251288
#Le passage au log peut-être judicieux pour normaliser les valeurs
df$PAR_SC_V=log(df$PAR_SC_V)
df$PAR_ASC_V=log(df$PAR_ASC_V)

####D'après l'annexe, les paramètres 148 à 167 sont les mêmes que les paramètres 128 à 147 donc on peut les supprimer
df=cbind(df[1:147],df[168:192])


####Variable très corrélées 
C=cor(df[,-172])

#Récupération des variables à supprimer(>.99)
highly_cor=findCorrelation(C,cutoff=0.99)

#Ajout d'un code poursupprimer la bonne variable dans le couple ? 

#Suppression des variables
df=df[,-highly_cor]

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

df$GENRE=as.numeric(factor(df$GENRE))-1
####Modele0####
###############
formule0<-GENRE~PAR_TC + PAR_SC + PAR_SC_V + PAR_ASE_M + PAR_ASE_MV + PAR_SFM_M + PAR_SFM_MV
Mod0<-model(formule0,data=df[train==TRUE,])

####ModeleT####
###############
formuleT<-GENRE~.
ModT<-model(formuleT,data=df[train==TRUE,])

####Modele1####
###############

####Récupérations des variables significatives
library(stats)
ind_variables_nonsign=which(summary(ModT)$coefficients[,4]>=0.05)

####Mis à jour du modèle en supprimant les variables significatives
formule_maj<-as.formula(paste("GENRE~.",paste(as.character(names(df)[-ind_variables_nonsign]), collapse="+")))
Mod1=model(formule_maj,data=df[train==TRUE,])

####Modele2####
###############

####Récupérations des variables significatives
ind_variables_nonsign=which(summary(ModT)$coefficients[,4]>=0.2)
nom_variables_nonsign=names(df)[ind_variables_nonsign]

####Mis à jour du modèle en supprimant les variables significatives
formule_maj<-as.formula(paste("GENRE~.",paste(names(df)[-ind_variables_nonsign], collapse="+")))
Mod2=model(formule_maj,data=df[train==TRUE,])

################
### Q4
################
library(ROCR)

pred=predict(ModT)
predictions_T_train=prediction(pred,df$GENRE[train==TRUE])
ROC_T_train = performance(predictions_T_train,"sens","fpr")  # prépare les infos pour la courbe ROC
plot(ROC_T_train, xlab="", col="blue",main="courbes ROC")

pred=predict(ModT,newdata=df[train==FALSE,])
predictions_T_test=prediction(pred,df$GENRE[train==FALSE])
ROC_T_test = performance(predictions_T_test,"sens","fpr")  # prépare les infos pour la courbe ROC
plot(ROC_T_test, xlab="", col="red",main="courbes ROC")
##########################################################################################################################################
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
x = as.matrix(df[,-ncol(df)])
y = as.numeric(factor(df$GENRE))-1
ridge.fit = glmnet(x,y,alpha=0,lambda=grid)



