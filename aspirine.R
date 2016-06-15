rm(list=ls())
df = read.table("aspirine_formate.txt",h=T)
boxplot(df$activite~df$mol)
# D semble peu efficace, asp semble la plus efficace, ABC equivalentes
# Pour avoir les observations individuelles :
coplot(df$activite~df$mol|rep(1,30))
# Pour calculer les moyennes par groupe :
tapply(df$activite,df$mol,mean)
# Applique une fonction (mean) à une variable (activite) selon les niveaux (mol)
# Calcul des variances d'un groupe a l'autre :
tapply(df$activite,df$mol,var)
# On teste avec test de Bartlett pour plus de precision :
bartlett.test(df$activite,df$mol)
# Pas de difference de variance significative entre groupes
fmol = factor(df$mol) #remplacer une variable qualitative par une variable quantitative
lm1 = lm(df$activite~fmol)
summary(lm1)
model.matrix(lm1)
contrasts(fmol) # matrice de contraste
# intercept : moyenne pour variable A
#fmolasp : difference de moyenne entre asp et A, difference non significative
anova(lm1)
#fmol : sce expliquee par le modele : reflete la variance inter groupes
#4 ddl car on a 4 coeffs lies a la variabilite (pas l'intercept)
# variance residuelle : ddl = 30-5. P-value : au moins une molecule se distingue des autres

#on change la matrice de contraste pour utiliser asp comme reference
fmol2=fmol
contrasts(fmol2) = contr.treatment(5,base=2)
contrasts(fmol2)
lm2 = lm(df$activite~fmol2)
summary(lm2)
anova(lm2)
# au moins une moyenne se distingue des autres
# la variabilité intragroupe ne change pas par rapport à lm1
plot(lm2)
shapiro.test(lm2)

#Tous les tests sont correles car ils sont tous compares a l'aspirine
# recuperer les 4 pvalues de lm2
p = summary(lm2)$coefficients[2:5,4]
p.adjust(p,method="bonferroni")
# molecule b : on est proche de 5 %

# methode de dunnett
lm2d = glht(lm2, c('fmol21=0','fmol23=0','fmol24=0','fmol25=0'))
summary(lm2d)