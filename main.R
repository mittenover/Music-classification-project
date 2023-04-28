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


#Importation des données
df=read.table("Music_2023.txt",dec='.',header=TRUE,sep=';')

###Partie I###

################
### Q1
################

####Analyse univarié
summary(df)  #Toutes les variables sauf la dernière sont numériques
str(df)


####Analyse bivariée (à voir)



####Proportion de chacun des genres
p_classique=sum(df$GENRE=="Classical")/length(df$GENRE)  #0.53
p_jazz=sum(df$GENRE=="Jazz")/length(df$GENRE)            #0.47

####PAR_SC_V, PAR_ASC_V
summary(df$PAR_SC_V)
mean(df$PAR_SC_V)      #105222.8

summary(df$PAR_ASC_V)
mean(df$PAR_ASC_V)     #0.4251288
#Le passage au log peut-être judicieux pour normaliser les valeurs

#D'après l'annexe, les paramètres 148 à 167 sont les mêmes que les paramètres 128 à 147 donc on peut les supprimer
