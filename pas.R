df = read.table("pas.txt")
df
pairs(df)
lm1 = lm(df$pas~df$age)
summary(lm1)
plot(lm1)
anova(lm1)
lm2 = lm(df$pas~df$age)
summary(lm2)

lm3 = lm(df$pas~df$age+df$bmi+df$ttaille)
summary(lm3)
anova(lm3)
# une partie du tour de taille affectee au bmi (car covariance non nulle entre les deux
# attribuee au premier parametre rencontré dans le modele)

lm4 = lm(df$pas~df$age+df$ttaille)
summary(lm4)
plot(lm4)

# tout est significatif dans ce modele
anova(lm4)
anova(lm4,lm3)

lm5 = lm(df$pas~df$age+df$bmi)
summary(lm5)
# R^2 un peu meilleur pour le modele age+ttaille que pour age+bmi

#Le modele retenu est lm4 : age+ttaille grace a l'analyse modeles emboites avec lm3
# hypotheses a verifier sur le modele :
# 1) les variables explicatives sont controlees : tour de taille, age
# ici c'est bien le cas
# 2) variables independantes : patients differents
# 3) Graph 1 : residus independants des variables explicatrices : d'apres le graphique, pas de lien
# + residus tournent autour de 0, pas de forme : modele qui est adapté (modele lineaire)
# 4) Graph 2 : normalite des residus (verifiee ici), peut etre aussi verifie par Shapiro.test
# 5) Graph 3 : la variance des residus est constante
# 6) Graph distance de Cook : pas de valeur aberrante (verifie ici)