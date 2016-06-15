rm(list=ls())
df = read.table('myo.txt',h=T)
# rep : concentration de noradrénaline, impacte la frequence cardiaque
plot(df)

lm1 = lm(df$rep~df$dose)
summary(lm1)
plot(lm1)

# On peut construire un modele qualitatif avec 4 niveaux de dose
dose = factor(df$dose)
lm2 = lm(df$rep~dose)
summary(lm2)
anova(lm2)
# on teste tous les ecarts entre coeffs et non les differences 2 à 2
# il y a au moins une dose dont la moyenne se distingue
# il y a un effet de la dose
# le deuxieme modele est plus souple que le premier : on impose pas de relier 
#les points entre differentes doses
# en comparant les deux modeles on peut tester la linearite de l'effet de la dose
anova(lm1,lm2)
# on retient le modele lineaire, plus simple : il explique aussi bien la variablilité