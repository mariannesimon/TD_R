df = read.table('stations2.txt',h=T)
plot(df)
boxplot(df$pollution~df$station)
tapply(df$pollution,df$station,mean)
tapply(df$pollution,df$station,var)

station = factor(df$station)
contrasts(station)
lm1 = lm(df$pollution~station)
summary(lm1)
anova(lm1)
# au moins une moyenne se distingue des autres
# mu = B0 + B1*I[s2] + B2*I*[s3] + B3*I*[s4]
mat = matrix(nrow=4,ncol=3,c(1,1,0,1,-1,0,-1,0,1,-1,0,-1),byrow=T)
mat
stat = station
contrasts(stat) = mat
contrasts(stat)
lm2=lm(df$pollution~stat)
summary(lm2)
anova(lm2)
#comparer s4 par rapport à {s1 s2 s3}