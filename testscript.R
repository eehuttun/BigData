library(e1071)
library(textcat)

eng <- c("Wikipedia is a free-access, free-content Internet encyclopedia, supported and hosted by the non-profit Wikimedia Foundation. Those who can access the site can edit")
fin <- c("Wikipedia on Internetiss� julkaistava ilmainen vapaan sis�ll�n tietosanakirja, joka perustuu MediaWiki-tekniikkaan. Tietosanakirja on virtuaalinen, eik� sit� julkaista painettuna versiona. Wikipediaa kirjoitetaan")
swe <- c("Wikipedia �r en wiki och ett m�ngspr�kigt webbaserat uppslagsverk med i huvudsak fritt och �ppet inneh�ll som utvecklas av sina anv�ndare (ofta ben�mnda wikipedianer). Wikipedia lanserades den")

prof_fin <- textcat_profile_db(fin)
prof_eng <- textcat_profile_db(eng)
prof_swe <- textcat_profile_db(swe)

fi <- as.matrix(prof_fin)[1:10]
en <- as.matrix(prof_eng)[1:10]
sw <- as.matrix(prof_swe)[1:10]

lang <- rbind(en, fi, sw)
lang <- data.frame(cbind(lang, c(1,2,3)))

svm.model <- svm(x11~., lang)

test <- c("Saamelaisten alkuperäisessä uskomusmaailmassa kaikella elollisella oli sielu. Ihmisellä oli useampia ")
prof_test <- textcat_profile_db(test)
ts <- as.matrix(prof_test)[1:10]
ts <- data.frame(t(ts))

svm.pred <- predict(svm.model, ts[,-11])
print(svm.pred)
