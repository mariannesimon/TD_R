rm(list=ls())
df = read.table("tournesol.txt",h=T)
df
# testeur : outil utilise pour obtenir la teneur en acides gras des tournesols
boxplot(df$teneur~df$origine)
boxplot(df$teneur~df$testeur)
coplot(df$teneur~df$origine|df$testeur)
#etudier les interactions
interaction.plot(df$testeur,df$origine,df$teneur)
# on n'obtient pas des droites paralleles : il y a interaction avec le testeur

ft = factor(df$testeur)
fo = factor(df$origine)

lm1 = lm(df$teneur~ft*fo)
summary(lm1)
tapply(df$teneur,interaction(df$testeur,df$origine),mean)
anova(lm1)
# on teste les interactions avec un risque de 10% et non 5%: donc il y a au moins
# un des interactions non nulle : on garde le modele avec interaction
lm2 = lm(df$teneur~fo,subset=df$testeur=="T2")
summary(lm2)

bartlett.test(df$teneur,interaction(ft,fo))
plot(lm1,4)
# Pas de valeur aberrante (seuil = 0.5) : on peut retenir le modele