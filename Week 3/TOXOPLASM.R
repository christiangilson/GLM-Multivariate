city <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34)
no.tested <- c(4, 10, 5, 10, 2, 5, 8, 19, 6, 10, 24, 1, 30, 22, 1, 11, 1, 54, 9, 18, 12, 1, 11, 77, 51, 16, 82, 13, 43, 75, 13, 10, 6, 37)
no.pos <- c(2, 3, 1, 3, 2, 3, 2, 7, 3, 8, 7, 0, 15, 4, 0, 6, 0, 33, 4, 5, 2, 0, 8, 41, 24, 7, 46, 9, 23, 53, 8, 3, 1, 23)
rain <- c(1735, 1936, 2000, 1973, 1750, 1800, 1750, 2077, 1920, 1800, 2050, 1830, 1650, 2200, 2000, 1770, 1920, 1770, 2240, 1620, 1756, 1650, 2250, 1796, 1890, 1871, 2063, 2100, 1918, 1834, 1780, 1900, 1976, 2292)
rain.m <- rain/1000
rain.cor <- rain.m - mean(rain.m)
prop <- no.pos/no.tested
toxoplas <- data.frame(rain.cor, city, no.tested, no.pos)
toxoplas.glm <- glm(prop ~ rain.cor + I(rain.cor^2) + I(rain.cor^3) + I(rain.cor^4) + I(rain.cor^5), weights = no.tested, family = binomial, data = toxoplas)
summary(toxoplas.glm)

anova(toxoplas.glm)

pchisq(11.577, 3, lower.tail=F)

toxoplas.glm <- glm(prop ~ rain.cor + I(rain.cor^2) + I(rain.cor^3), weights = no.tested, family = binomial, data = toxoplas)
summary(toxoplas.glm)

x <- seq(-0.4,0.5,0.005)
plot(rain.cor,prop,xlim=c(-0.45,0.55),ylim=c(0,1))
lines(x,predict(toxoplas.glm,data.frame(rain.cor=x),type="response"))