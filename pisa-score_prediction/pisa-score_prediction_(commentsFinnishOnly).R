pisa <- read.table("pisadata.txt", header = TRUE)
head(pisa)
summary(pisa)
pisa$poju <- as.numeric(pisa$sukup=="poika")
head(pisa)
attach(pisa)

# kuvailevat statistiikat

# mpist
summary(mpist)
sd(mpist)

# matem
summary(matem)
sd(matem)

# aidink
summary(aidink)
sd(aidink)

# HISEI
summary(HISEI)
sd(HISEI)

# SES
summary(SES)
sd(SES)

# sukup
summary(sukup)

# motiv
summary(as.factor(motiv))

# koulusij
koulusij

# koulualue
summary(koulualue)

# id
summary(id)
matmed <- tapply(mpist, id, median)
nouseva <- sort(matmed)
pienimmat <- nouseva[1:10]
suurimmat <- nouseva[126:135]
idmediaanit <- c(pienimmat, suurimmat)
idmediaanit
length(sort(unique(id)))
sort(id)
barplot(id)

# kuvio
par(mfrow=c(3, 3))
plot(matem, mpist, xlab="matematiikan arvosana", ylab="Matematiikan pistemäärä PISA-kokeessa")
plot(aidink, mpist, xlab="äidinkielen arvosana", ylab="Matematiikan pistemäärä PISA-kokeessa")
plot(HISEI, mpist, xlab="vanhempien ammatillinen status", ylab="Matematiikan pistemäärä PISA-kokeessa")
plot(SES, mpist, xlab="perheen sosioekonominen status", ylab="Matematiikan pistemäärä PISA-kokeessa")
boxplot(mpist ~ sukup, ylab="Matematiikan pistemäärä PISA-kokeessa", names=c("poika", "tyttö"))
boxplot(mpist ~ motiv, ylab="Matematiikan pistemäärä PISA-kokeessa", names=c("muut", "motivoituneet"))
boxplot(mpist ~ koulusij, ylab="Matematiikan pistemäärä PISA-kokeessa")
boxplot(mpist ~ koulualue, ylab="Matematiikan pistemäärä PISA-kokeessa", names=c("Etelä-Suomi", "Itä-Suomi", "Länsi-Suomi", "Pohjois-Suomi"))
barplot(nouseva, mpist, xaxt = 'n', ylab="Matematiikan pistemäärän mediaani PISA:ssa", xlab="koulut järjestyksessä huonoimmasta parhaimpaan")
dev.off()

# plot(matem, mpist, xlab="matematiikan arvosana", ylab="PISA-kokeen pistemäärä matematiikassa", main="PISA-kokeen matematiikan pistemäärien jakautuminen matematiikan arvosanojen mukaan")
# plot(aidink, mpist, xlab="äidinkielen arvosana", ylab="PISA-kokeen pistemäärä matematiikassa", main="PISA-kokeen matematiikan pistemäärien jakautuminen äidinkielen arvosanojen mukaan")
# plot(HISEI, mpist, xlab="vanhempien ammatillinen status", ylab="PISA-kokeen pistemäärä matematiikassa", main="PISA-kokeen matematiikan pistemäärien jakautuminen vanhempien ammatillisen statuksen mukaan")
# plot(SES, mpist, xlab="perheen sosioekonominen status", ylab="PISA-kokeen pistemäärä matematiikassa", main="PISA-kokeen matematiikan pistemäärien jakautuminen perheen sosioekonomisen statuksen mukaan")
# boxplot(mpist ~ sukup, ylab="PISA-kokeen pistemäärä matematiikassa", names=c("poika", "tyttö"), main="PISA-kokeen matematiikan pistemäärien jakautuminan sukupuolittain")
# boxplot(mpist ~ motiv, ylab="PISA-kokeen pistemäärä matematiikassa", names=c("muut", "motivoituneet"), main="PISA-kokeen matematiikan pistemäärien jakautuminen motivaatiotasoittain")
# boxplot(mpist ~ koulusij, ylab="PISA-kokeen pistemäärä matematiikassa", main="PISA-kokeen matematiikan pistemäärien jakautuminen koulusijainnittain")
# boxplot(mpist ~ koulualue, ylab="PISA-kokeen pistemäärä matematiikassa", names=c("Etelä-Suomi", "Itä-Suomi", "Länsi-Suomi", "Pohjois-Suomi"), main="PISA-kokeen matematiikan pistemäärien jakautuminen koulualueittain")
# barplot(nouseva, mpist, xaxt = 'n', ylab="PISA-kokeen matematiikan pistemäärän mediaani", xlab="koulut järjestyksessä huonoimmasta parhaimpaan", main="PISA-kokeen matematiikan pistemäärän mediaani kouluittain")

# kuvaillaan enemman 
cor(mpist, matem)
cor(mpist, aidink)
cor(mpist, HISEI)
cor(mpist, SES)

median(mpist[sukup == "poika"])
median(mpist[sukup == "tytto"])
sd(mpist[sukup == "poika"])
sd(mpist[sukup == "tytto"])

median(mpist[motiv == 1])
median(mpist[motiv == 0])
sd(mpist[motiv == 1])
sd(mpist[motiv == 0])

median(mpist[koulusij == "kaupunki"])
median(mpist[koulusij == "maaseutu"])
sd(mpist[koulusij == "kaupunki"])
sd(mpist[koulusij == "maaseutu"])


median(mpist[koulualue == "Ita-Suomi"])
median(mpist[koulualue == "Etela-Suomi"])
median(mpist[koulualue == "Lansi-Suomi"])
median(mpist[koulualue == "Pohjois-Suomi"])
sd(mpist[koulualue == "Ita-Suomi"])
sd(mpist[koulualue == "Etela-Suomi"])
sd(mpist[koulualue == "Lansi-Suomi"])
sd(mpist[koulualue == "Pohjois-Suomi"])


# ajetaan mallit kullekin muuttujalle

HISEI2 <- HISEI^2

malli <- lm(mpist ~ I(matem - 8) + I(aidink - 8) + I(SES - 0.3) + poju)
summary(malli)
confint(malli)
b <- coef(malli)
plot(fitted(malli), residuals(malli), xlab="sovitteet", ylab="jäännökset")
anova(malli)
plot(mpist, fitted(malli), main="Vasteet sovitteiden suhteen", xlab="sovitteet", ylab="vasteet")
abline(0,1)
qqnorm(resid(malli)/sqrt(var(resid(malli))), main="Jäännösten normaalisuus", xlab="teoreettisen kvantiilit", ylab="otoskvantiilit")
abline(0,1)



zalli <- lm(mpist ~ id)
summary(zalli)
