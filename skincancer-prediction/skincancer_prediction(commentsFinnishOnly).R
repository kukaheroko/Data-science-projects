skincan <- read.table("skincan.txt", header=TRUE)
head(skincan)
attach(skincan)
# syopalaiset <- skincan[cancer==1,]
# head(syopalaiset)
# attach(syopalaiset)
# length(cancer)
# attach(skincan)
# ei_syopalaiset <- skincan[cancer==0,]
# head(ei_syopalaiset)
# attach(ei_syopalaiset)
# length(cancer)
# attach(skincan)
# dim(skincan[trt==0, ])
# 856+827
# gender
# dim(skincan[gender==0, ])
# dim(skincan[gender==1, ])
# dim(skincan[skin==0, ])
# dim(skincan[skin==1, ])
# mean(age)
# sd(age)
# median(age)
# min(age)
# max(age)
# mean(exposure)
# sd(exposure)
# median(exposure)
# min(exposure)
# max(exposure)

# plotataan Ihosyövän suhteelliset frekvenssit eri i'ille 
# suhteen

# suhteelliset frekvenssit
frek <- table(cancer, age)
sf <- frek[2,]/(frek[1,]+frek[2,])

# eri iät
lk <- sort(unique(age)) # tai as.numeric(names(sf))

plot(lk, sf, main="Ihosyöpään sairastumisen suhteelliset frekvenssit aiemmin sairastettujen ihosyöpien eri i'illä", xlab="Ikä", ylab="Ihosyöpään sairastumisen todennäköisyys")



# plotataan Ihosyövän suhteelliset frekvenssit eri i'ille 
# suhteen

# suhteelliset frekvenssit
frek <- table(cancer, exposure)
sf <- frek[2,]/(frek[1,]+frek[2,])

# eri iät
lk <- sort(unique(exposure)) # tai as.numeric(names(sf))

plot(lk, sf, main="Ihosyöpään sairastumisen suhteelliset frekvenssit aiemmin sairastettujen ihosyöpien eri lukumäärillä", xlab="Aiemmin sairastettujen ihosyöpien lukumäärä", ylab="Ihosyöpään sairastumisen todennäköisyys")
#main="Ihosyöpään sairastumisen suhteelliset frekvenssit aiemmin sairastettujen ihosyöpien eri lukumäärillä"

#ihosyöpädiagnoosin saaneet naiset
dim(skincan[gender==0 & cancer==1,])
#naiset jotka eivät saaneet ihosyöpädiagnoosia
dim(skincan[gender==0 & cancer==0,])
#aineistossa naisten todennäköisyys saada ihosyöpädiagnoosi
57/(57+466)
# = 10.9 %

#ihosyöpädiagnoosin saaneet miehet
dim(skincan[gender==1 & cancer==1,])
#miehet jotka eivät saaneet ihosyöpädiagnoosia
dim(skincan[gender==1 & cancer==0,])
252/(252+908)
# = 21.7 %


#ihosyöpädiagnoosin saaneet beetakaroteeniryhmässä
dim(skincan[trt==1 & cancer==1,])
#beetakaroteenilaiset ilman ihosyöpädiagnoosia
dim(skincan[trt==1 & cancer==0,])
#aineistossa beetakaroteenilaiseten todennäköisyys saada ihosyöpädiagnoosi
172/(172+684)
# = 20.1 %

#ihosyöpädiagnoosin saaneet plaseboryhmässä
dim(skincan[trt==0 & cancer==1,])
#placoboryhmäläiset ilman ihosyöpädiagnoosia
dim(skincan[trt==0 & cancer==0,])
#aineistossa plaseboryhmäläisten todennäköisyys saada ihosyöpädiagnoosi
137/(137+690)
# = 16.6 %

#ihosyöpädiagnoosin saaneet herkkänahkaiset
dim(skincan[skin==1 & cancer==1,])
#herkkänahkaiset ilman ihosyöpädiagnoosia
dim(skincan[skin==1 & cancer==0,])
#aineistossa herkkänahkaisten todennäköisyys saada ihosyöpädiagnoosi
174/(174+594)
# = 22.7 %

#ihosyöpädiagnoosin saaneet paksunahkaiset
dim(skincan[skin==0 & cancer==1,])
#paksunahkaiset ilman ihosyöpädiagnoosia
dim(skincan[skin==0 & cancer==0,])
#aineistossa paksunahkaisten todennäköisyys saada ihosyöpädiagnoosi
135/(135+780)
# = 14.8 %


#-----------------------------------------------------------------
# rakennetaan nyt malli

ihosyopa <- glm(cancer ~ trt + gender + skin + exposure + trt*skin, family = binomial(link = "logit"))
#ihosyopa <- glm(cancer ~ exposure*skin, family = binomial(link = "logit"))

#OR:t mallin kertoimille
b <- coef(ihosyopa)
exp(b)
# ja luottamusvalit
exp(confint(ihosyopa))

summary(ihosyopa)
invlogit <- function(x) { exp(x)/(1+exp(x)) }

invlogit(b[2])

sd <- sqrt(diag(vcov(ihosyopa)))
b <- coef(ihosyopa)
b[2] - 1.96 * sd[2]
b[2] + 1.96 * sd[2]
confint(ihosyopa)

b
exp(b[2])
exp(b[2] - 1.96 * sd[2])
exp(b[2] + 1.96 * sd[2])
exp(confint.default(ihosyopa))


malliA <- glm(cancer ~ trt + gender + skin + exposure + trt*skin, family = binomial(link = "logit"))
malliB <- glm(cancer ~ trt + gender + skin + exposure, family = binomial(link = "logit"))
summary(malliA)
summary(malliB)
derotus <- 1440.3 - 1442.3
p <- 1 - pchisq(derotus, 4)
p

derotus1 <- 443.74 - 437.26
p1 <- 1 - pchisq(derotus1, 3)
p1

derotus2 <- 452.92 - 443.74
p2 <- 1 - pchisq(derotus2, 3)
p2


ikamalli <- glm(cancer ~ age, family = binomial(link = "logit"))
summary(ikamalli)


pred <- ihosyopa$linear.predictors
freq <- table(cancer, pred)
prob <- freq[2,] / (freq[1,] + freq[2,])
plot(sort(unique(pred)),prob, xlab="Lineaarinen prediktori", ylab="Suhteelliset frekvenssit ja mallin ennustama ihosyövän todennäköisyys", pch=21, bg="gray")
lines(sort(unique(pred)),invlogit(sort(unique(pred))), lwd=2)

error.rate <- function(y, mod){
mean((fitted(mod) > 0.5 & y == 0) | (fitted(mod) < 0.5 & y == 1) )}
error.rate(cancer, ihosyopa)
