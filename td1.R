file = read.table("depression.txt",dec=",",header=T)
plot(file$monotonie,file$depression,xlab="Monotonie",ylab="Depression")


model = lm(depression ~ monotonie, data=file)
abline(model,col="red")
pre=predict(model,int="confidence")
pre
lines(sort(file$monotonie),pre[order(file$monotonie),2])
lines(sort(file$monotonie),pre[order(file$monotonie),3])
summary(model)
model
cor.test(file$monotonie,file$depression)
par(mfrow=c(2,2))
plot(model,4)
shapiro.test(residuals(model))
