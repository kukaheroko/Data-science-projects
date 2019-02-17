#Example from the book:
# Machine learning with R: Brett Lantz

#before downloading data choose right working space or
#give perfect path for the data
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)

# muutetaan type_feature character vektorista faktoriksi
sms_raw$type <- factor(sms_raw$type)

str(sms_raw$type)
table(sms_raw$type)

# processing text data for analysis

# asennetaan ensin asioita helpottava text mining-paketti
install.packages("tm")
# ja otetaan se kayttoon
library(tm)

# let's build a corpus that contains SMS messages in the training data
# first Corpus function creates an R object to store text documents
# it uses parameter to specify the format of text documents to be loaded
# VectorSource() tells Corpus to use messages in sms_raw$text vector
sms_corpus <- Corpus(VectorSource(sms_raw$text))
# HUOM! corpus toimii myos esim pdf ja word dokumenttien kanssa

print(sms_corpus)

# let's inspect for example first, second and third messages
inspect(sms_corpus[1:3])

# first corpus needs to be cleaned
# tm_map provides a method for mapping a tm corpus.

# let's concert all messages to lowercase
corpus_clean <- tm_map(sms_corpus, tolower)

# let's remove all numbers
corpus_clean <- tm_map(corpus_clean, removeNumbers)

# let's remove all stop words, eg. and, but, or...; with stopwords function
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())

# let's remove punctuation
corpus_clean <- tm_map(corpus_clean, removePunctuation)

# edelliset funktiot korvaavat poistettavan merkin valilyonnilla
# nyt nama ylimaaraiset valilyonnit poistetaan, jotta sanojen 
# valiin jaa vain yksi valilyonti
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# nyt verrataan aiemmin tehtyyn inspectioniin
inspect(corpus_clean[1:3])
# siivoamattomat sai aiemalla tavalla inspect(sms_corpus[1:3])


# now it's time for tokenization; a token is a single element of
# text string; in this case tokens are words
# DocumentTermMatrix() function luo corpuksesta "sparse matriisin",
# jossa kukin rivi kuvaa yhta dokumenttia (tassa message) ja sarakkeet
# kuvaavat termeja (tassa sanoja). Jokaisessa solussa on numero joka
# kuvaa kyseisen sarakkeen termin esiintymisten maaran kyseisen rivin
# dokumentissa. 
# "sparse matriisissa" on tyypillisesti paljon nollia, koska tietyn
# sanan esiintyminen missa tahansa dokumentissa on loppupeleissa 
# varsin harvinaista
sms_dtm <- DocumentTermMatrix(corpus_clean)


# harjoitus- ja testiaineistojen luominen

# jaetaan nyt 75% (harkka) ja 25% (testi)
# koska messaget nyt valmiiksi randomissa jarjestyksessa, voidaan
# napata suoraan 75% ensimmaista messagea harkka-aineistoon ja 
# loput 25% testaukseen

# jateaan ensin raw data frame
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5574, ]
# sitten document-term matrix
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5574, ]
# sitten corpus
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5574]
# tarkistetaan, etta harjoitus- ja testiaineisto ovat molemmat 
# riittavan edustava otos koko message-datasta
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
# molemmissa spammien suhde (13 %) riittavan samanlainen, eli ok


# VISUALIZING TEXT DATA - WORD CLOUDS
# a word cloud is a way to visually depict the frequency at which
# words appear in text data
# asennetaan ensin tarvittava paketti
install.packages("wordcloud")
# otetaan kayttoon
library(wordcloud)

# luodaan sanapilvi
# random.order = FALSE; tarkoittaa, etta pilvi jarestetaan siten, etta
# useimmin toistuvat sanat asetetaan lahemmas keskustaa
# min.freq; maaraa vahimmaisesiintymiskertojen maaran sanalle corpuksessa,
# jotta se paasee mukaan pilveen
# scalella voidaan vaikuttaa fonttiin
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)
# koitetaan myos muilla settingeilla
# wordcloud(sms_corpus_train, min.freq = 40, random.order = TRUE)
# wordcloud(sms_corpus_train, min.freq = 50, random.order = FALSE)
# wordcloud(sms_corpus_train, min.freq = 100, random.order = FALSE)
# yleensa tapana on aloittaa asettamalla min.freq 10% corpuksessa
# olevien dokumenttien maarasta

# nyt vertaillaan spam-pilvea ja ham-pilvea
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")

# nyt piirretaan molemmille subseteille omat pilvet
# max.words rajoittaa pilveen otettavien sanojen maaraan, nyt 40 yleisinta
# scalella voi vaihtaa fonttia
par(mfrow=c(1,2))
wordcloud(spam$text, max.words = 40, scale = c(1, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(1, 0.5))
# tassa on hyodynnetty wordcloud funktion ominaisuutta, jossa
# se suorittaa automaattisesti transformaatiot raakatekstille
# ennen kuin se konstruoi corpuksen ja nayttaa sanapilven


# Luodaan indikaattori featuret yleisille sanoille
dim(sms_dtm_train)
# finFreqTerms() (tm-paketti) palauttaa character-vektorin joka
# sisaltaa sanat jotka esiintyvat vahintaan parametrina maaritellyt kerrat
findFreqTerms(sms_dtm_train, 5)
# otetaan harkka ja testidatoihin mukaan vain yleisimmat sanat
sms_dict <- as.vector(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, 
                                list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, 
                                list(dictionary = sms_dict))

# koska naive Bayes classifier treenataan tyypillisesti 
# luokittelufeatureilla, taytyy tassa aineistossa vaihtaa
# soluissa olevat sanojen esiintymiskerrat vaihtaa yksinkertaisesti
# faktoriin joka indikoi "yes" tai "no" oliko sana dokumentissa.
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

# nyt hyodynnetaan convert_countsia applyn kautta kaikkiin dataframen
# kolumneihin (MARGIN = 2; sarakkeet, 1 = rivit)
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)


# TRAINING MODEL ON THE DATA
# we use naive Bayes from e1071 package
# also klaR-package includes NaiveBayes() that is nearly identical
install.packages("e1071")
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

# EVALUATING MODEL PERFORMANCE
sms_test_pred <- predict(sms_classifier, sms_test)

# to compare predicted values to the actual values, we'll use CrossTable()
# dnn (dimension names) relabels rows and columns
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


# IMPROVING MODEL PERFORMANCE
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, 
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
