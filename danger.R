df = read.table("danger.txt")
plot(df$x1,df$y3)
lm1 = lm(y1~x1,df)
lm2 = lm(y2~x1,df)
lm3 = lm(y3~x1,df)
lm4 = lm(y4~x1,df)
lm5 = lm(y5~x1,df)
summary(lm1)
abline(0.52,0.8087,col="red")
# Verif des hypotheses
par(mfrow=c(2,2))
plot(lm3)
#Graphique bas a gauche : verifier si la variance des residus est constante. Ici on voit
# une forme apparaitre donc ce n'est pas le cas
shapiro.test(residuals(lm3))
# Pour obtenir graphique avec distance de Cook : on compare les valeurs predites
# a celles qu'on obtiendrait en enlevant ce point. Ici un point a une distance elevee 
#(observation n°16) : effet levier important
plot(lm3,4)
#On enleve le point 16 des donnees
y3bis=df$y3[-16]
x1bis=df$x1[-16]
lmbis = lm(y3bis~x1bis)
summary(lmbis)

plot(df$x1,df$y4)
summary(lm4)
abline(0.52,0.80,col="red")
par(mfrow=c(2,2))
plot(lm4)
# Graphique 1 : "pas grand chose ne va"
# Graphique en haut a droite : residus a peu pres gaussiens
# Graphique 3 : variance pas tout a fait constante, ce qui impacte tous les autres graphiques
# Bas a droite : un point potentiellement aberrant

plot(lm2)
# HG : forme tres apparente, modele lineaire non adapte
plot(df$x1,df$y2) #confirme la non linearite
# on va pas tous les faire mais on montre l'importance de verifier les hypotheses du modele