# variable a expliquer : clairance de la creatinine du rein chez des patients ayant eu
# un infarctus du myocarde
df = read.table("mi.txt",h=T)
age70 = (df$age-70)*I(df$age>70)
lm(df$clairance~df$age+age70)

# age <= 70 : µ = b0 + b1*age
# age > 70: µ = b0 + (b1+b2)*age - 70b2
# age = 70 : µ = b0 + 70b1

lm5 = lm(df$clairance~df$age*factor(df$hta))
summary(lm5)
predict(lm5)

# µ = b0 + b1*age + b2*I[hta=oui] + b3*age*I[hta=oui]
# hta = oui : µ = b0 + b1*age
# hta = non : µ = b0 + b2 + (b1+b3)*age

plot(df$age,df$clairance,col=factor(df$hta=="oui"))
boxplot(df$age,df$clairance)
