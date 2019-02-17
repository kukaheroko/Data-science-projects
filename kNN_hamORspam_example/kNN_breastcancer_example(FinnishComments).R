#chapter3 example from the book:
# Machine Learning with R: Brett Lantz

#before downloading data, you should choose right working space,
#or give perfect path for data.
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1]
str(wbcd)
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#transformation: normalizating numeric data
normalize <- function(x){
    return ((x - min(x)) / (max(x) - min(x)))
}

#test
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalisoidaan loput aineiston featuret
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

# datan valmistelu: luodaan treeni- ja testiaineistot (siis setit)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
# normaalisti jouduttaisiin hyodyntamaan randomisointia,
# nyt valmiiksi sekoitettu, joten voitiin ottaa suoraan


# tarvitaan labelit talteen diagnosista(targetti)
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]


# R:n perusluokittelufunktiot loytyvat class-paketista
install.packages("class")
# otetaan se kayttoon tassa sessiossa
library(class)

# suoritetaan testidatan luokittelu
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 21)
# luokittelun onnistuneisuuden arvionti onnistuu
# ristiintaulukoinnilla joka loytyy gmodels-paketista
library(gmodels)
# ei oteta mukaan chi-square arvoja nyt kun ei tarvita
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)


# koitetaan parantaa luokittelun tarkkuutta
# ensin z-standardoimalla featuret ja sen jalkeen muuttamalla k:ta

# z-standardointiin voidaan kayttaa scale-funktiota
# ei standardoida diagnosisia (siksi [-1])
wbcd_z <- as.data.frame(scale(wbcd[-1]))

summary(wbcd_z$area_mean)
# z-standardoidun keskiarvo tulisi olla 0, ja rangen melko kompakti
# alle -3 tai yli 3 arvot tosi ekstreemeja
wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)


# testataan nyt k:n vaikutusta (k=1, k=5, k=11, k=15, k=21, k=27)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k = 27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)