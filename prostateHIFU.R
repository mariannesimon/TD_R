df = read.table("prostateHIFU.txt",h=T)
plot(log(df$psa_pre),df$lognadir)

logpsa = log(df$psa_pre)
lognadir = df$lognadir
lm1 = lm(lognadir~logpsa)
summary(lm1)
abline(lm1,col="red")

lm2 = lm(lognadir~df$ln_psa_quant)
lm3 = lm(lognadir~as.numeric(df$ln_psa_quant))

summary(lm2)
summary(lm3)

# les modeles lm2 et lm3 sont emboites, on peut les comparer par scer
anova(lm2,lm3)
# pvalue superieure a 5% : modeles non differents
# on privilegie donc le modele lineaire
