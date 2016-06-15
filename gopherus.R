df = read.csv("gopherus.csv",sep=";")
oeufs = df$oeufs
long = df$longueur
plot(long,oeufs)

lm1 = lm(oeufs~long)
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)

long2 = long^2
lm2 = lm(oeufs~long+long2)
summary(lm2)
plot(lm2)
plot(long,oeufs)
curve(-8.892*10^2 + 5.778*x - 9.285*10^(-3)*x^2,add=T,col="red")
# terme en long^2 : comment la pente varie quand la valeur augmente
# modele adapte
anova(lm2)
lm2 = lm(oeufs~long2+long)
anova(lm2)
# anova differente car correlation entre long et long2
cor(long,long2)

long2bis = residuals(lm(long2~long))
lm3 = lm(oeufs~long+long2bis)
summary(lm3)
plot(lm3)
predict(lm2)
predict(lm3)
anova(lm3)
anova(lm3,lm1)

lm4 = lm(oeufs~poly(long,2))
summary(lm4)
# genere deux nouvelles variables independantes l'une de l'autre
predict(lm4)

lm5 = lm(oeufs~poly(long,3))
summary(lm5)