library(e1071)
library(RTextTools)

setwd("/home/vaimi/Lataukset/wikidumps")

languages.loc=list.files(path = "1000")
languages=gsub("_.*","",languages.loc)

input = data.frame(text=character(), lang=character())
for (i in 1:length(languages.loc)) {
  text <- lapply(paste("1000/", languages.loc[i], sep=""),readLines)
  rows <- data.frame(t(matrix(unlist(text), nrow=length(text), byrow=T)))
  rows["lang"] <- languages[i]
  colnames(rows) <- c("text", "lang");
  input <- rbind(input, rows)
}

dtMatrix <- create_matrix(input["text"])
container <- create_container(dtMatrix, input$lang, trainSize=1:length(input[,1]), virgin=FALSE)
model <- train_model(container, "SVM", kernel="linear")

predictionData <- list("Regionala varianter med rötter i äldre lokala dialekter talas fortfarande, men både talspråk och särskilt skriftspråk är i hög grad standardiserade i fråga om grammatik och ordförråd. Även mer särpräglade dialekter talas i mindre utsträckning. De är inte direkt hotade av språkdöd, men har varit på tillbakagång sedan början av 1900-talet, trots att många är välutforskade och att användningen idag inte längre motarbetas.", "sulautuneet. Neutrin epämääräinen artikkeli on ett, ei-neutrin en, ja vastaavasti neutrin määräiset muodot loppuvat t:hen ja ei-neutrin n:ään. Koska kielissä, joissa on erikseen maskuliini ja feminiini, ovat miehiä tarkoittavat sanat yleensä maskuliineja ja naisia tarkoittavat sanat feminiinejä, on ruotsin kielen yksinkertaistuminen johtanut siihen, että lähes kaikki ihmisiä tarkoittavat sanat kuuluvat yhteen sukuun")
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix) #https://github.com/timjurka/RTextTools/issues/4

predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

results <- classify_model(predictionContainer, model)
results
