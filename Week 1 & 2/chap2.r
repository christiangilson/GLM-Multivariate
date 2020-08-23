dose <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.861, 1.8839)
number <- c(59, 60, 62, 56, 63, 59, 62, 60)
killed <- c(6, 13, 18, 28, 52, 53, 61, 60)
prop <- killed/number
dose <- dose - 1.78
pesticide <- data.frame(dose, number, killed, prop)
pesticide.glm <- glm(prop ~ dose, family = binomial,
weights = number, data = pesticide)
summary(pesticide.glm)
e_b1<-exp(coef(pesticide.glm)[1])
e_b2<-exp(coef(pesticide.glm)[2])

x <- seq(-0.105,0.12,0.005)
plot(dose,prop,xlim=c(-0.11,0.12),ylim=c(0,1))
lines(x,predict(pesticide.glm,data.frame(dose=x),type="response"))


dose <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.861, 1.8839)
number <- c(59, 60, 62, 56, 63, 59, 62, 60)
killed <- c(6, 13, 18, 28, 52, 53, 61, 60)
prop <- killed/number
pesticide <- data.frame(dose, number, killed, prop)
pesticide.glm <- glm(prop ~ dose, family = binomial,
weights = number, data = pesticide)
summary(pesticide.glm)




