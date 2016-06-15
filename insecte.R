rm(list=ls())
df = read.table("insecte.txt",h=T)
champ = df$champ
prod = df$prod
rep = df$rep
boxplot(rep~prod)
boxplot(rep~champ)
coplot(rep~champ|prod)
#une seule mesure pour chaque coupe (c,prod) : on ne peut pas modeliser leur interaction

fchamp = factor(champ)
fprod = factor(prod)
lm1 = lm (rep~fchamp+fprod)
summary(lm1)
anova(lm1)

# modele : µ = B0 + B1I[c2] + B2I[c3] + B3I[c4] + B4I[c5] + A1I[X] + A2I[Y] + A3I[Z]
# interpretation de quelques coefs :
#  c1       c2       c3
#T B0     B0+B1     B0+B2
#X B0+A1  B0+B1+A1  B1+B2+A1
#Y B0+A2  B0+B1+A2  B0+B2+A2

tapply(rep,prod,mean)
# les coeffs en B du modeles sont obtenus en faisant les differences des moyennes par prod
tapply(rep,champ,mean)
# les coeffs en A du modeles sont obtenus en faisant les differences des moyennes par champ
# ddl residuals : ddl champ * ddl prod

F = (9.22/3)/(9.61/12)
# au moins une moyenne de produits se distingue des autres
# le resultat de l'anova contredit celui du summary, mais le risque alpha est superieur dans le summary
#le champ n'a pas d'influence

lm2 = lm(rep~prod)
summary(lm2)
anova(lm2)
# H0 : aucune moyenne se detache des autres
# ici p-value superieure a 5%, on rejette H0
# les modeles lm1 et lm2 sont emboites : on peut les comparer
anova(lm2,lm1)
# on ne rejette pas l'hypothese nulle : les deux modeles ne sont pas significativement differents
# on retient le modele le plus petit : lm2

plot(lm2)
# on verifie que les residus ne dependent pas des variables explicatrices