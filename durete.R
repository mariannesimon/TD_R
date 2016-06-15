df = read.table("dure.txt",h=T)
# Variables explicatrices qualitatives et quantitatives

plot(df$durete,df$mort,col=factor(df$geo=='S'))
# ou
plot(df$durete,df$mort,col=as.numeric(df$geo))
# Plus de deces au Nord qu'au Sud ?
# Premier modele : pas de variable explicatrice, µ = B0
lm0 = lm(df$mort~1)
summary(lm0)
abline(h=1523.29)

# Deuxieme modele : modele avec durete en variable explicatrice
lm1 = lm(df$mort~df$durete)
summary(lm1)
# Mortalite quand la durete est nulle : B0 = 1670
# Pente B1 = -3 : mortalite diminue de 3 quand la durete augmente de 1
# Les effets des coeffs sont significatifs
abline(lm1,lty=2)
anova(lm1)
# p-value significative : la durete a un effet sur le nombre de deces
# durete : ddl = 1 car un seul coef associe a la variable explicatrice (B1)
# residus : ddl = 59-2 car 2 parametres dans le modele

# Troisieme modele : localisation geo
lm2 = lm(df$mort~factor(df$geo))
summary(lm2)
contrasts(factor(df$geo)) # on verifie que le modele s'ecrit µ = B0 + B1*I[geo=S]
# B0 : moyenne de mortalite au Nord
# B1 : difference de moyenne de mortalite entre S et N : mort plus faible au Sud car coef negatif
# pvalue significative donc coeff B1 non nul : presence d'un effet
abline(h=1628.71,lty=3) #Droite au Nord
abline(h=(1628.71-248.79),lty=4) #droite au Sud
anova(lm2)

# Modele 3 : durete + geo
lm3 = lm(df$mort~df$durete+factor(df$geo))
summary(lm3)
# µ = b0 + b1*durete + b2*I[geo=S]
# N : b0 + b1*durete
# S : b0 + b2 + b1*durete : intercept b0+b2, pente b1
# b0 : moyenne de deces au Nord a durete nulle
# b2 : difference de mortalite entre S et N a durete nulle ou comparable
# b1 : mort varie de b1 pour une diff de durete de 1 a localisation comparable (N ou S)
abline(a=1690,b=-1.99)
abline(a=1690-172,b=-1.99)
anova(lm3)
anova(lm3,lm2) # comparaison modeles emboites
# manuellement : ( SCEr2 - SCEr3 )/( p3 - p2 ) / SCER3/( N - p3 ) -> suit loi de Fischer (p3-p2,n-p3
# CCL : modeles differents, on garde le + grand (explique mieux)
anova(lm3,lm1)
# CCL : LM3 et LM1 differents, on garde LM3

# Modele 4 : modele avec interaction

lm4 = lm(df$mort~df$durete*factor(df$geo))
summary(lm4)
# Eq du modele : µ = b0 + b1*durete + b2*I[S] + b3*durete*I[S]
# coef associe a l'interaction : produit des deux indicatrices de classe
# N : b0 +b1*durete
# S : b0 + b2 + (b1 + b3)*durete
# b1 + b3 : variation de mortalite lorsque la durete augmente de 1 au S
# b3 : difference  de pente entre  S et N
# interaction non statistiquement significative
anova(lm4)
# Modele 4 et 3 sont emboites
anova(lm4,lm3)

# On retient lm3

lm5 = lm(df$mort~factor(df$geo)*df$durete)
summary(lm5)
anova(lm5)
# Il n'y a pas independance entre les variables durete et geo

par(mfrow=c(2,2))
plot(lm3)
shapiro.test(residuals(lm3))
plot(lm3,4)
