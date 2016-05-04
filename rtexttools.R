library(e1071)
library(RTextTools)
  
#this SHOULD be changed later!

path <- "/home/matias/koulu/big_data/harjotustyo/harjotustyo_git/BigData"
source(file = paste(path,"/langIdentification.r",sep = ""))

setwd("/home/matias/koulu/big_data/harjotustyo/harjotustyo_git/data/")
  
languages.loc=list.files(path = "1000")
languages=gsub("_.*","",languages.loc)
#define variable types
#
input = data.frame(text=character(), lang=character())

#TODO: USE ONLY FACTORS
#input2 = data.frame(text=character(), lang=factor())
#
#languages = as.factor(languages)

  
#this could be done with mapreduce
for (i in 1:length(languages.loc)) {
    text <- lapply(paste("1000/", languages.loc[i], sep=""),readLines)
    rows <- data.frame(t(matrix(unlist(text), nrow=length(text), byrow=T)))
    rows["lang"] <- languages[i]
    colnames(rows) <- c("text", "lang");
    input <- rbind(input, rows)
}

input$lang = as.factor(input$lang)
#weighting=tm::weightTfIdf
dtMatrix <- create_dtm_matrix(textColumns_ = input["text"])  

#virgin was FALSE
container <- create_container(matrix = dtMatrix, 
                                labels = input$lang, trainSize=1:length(input[,1]), 
                                virgin=FALSE #FALSE = labels are known
) 
#type on testi
#probability oli ennen FALSE
#cross = k-fold cross validation
model <- train_model(container = container, algorithm = "SVM", probability = TRUE,
                     type="C-classification")

trace("create_matrix", edit=T)
predictionData <- list("Regionala varianter med rötter i äldre lokala dialekter talas fortfarande, men både talspråk och särskilt skriftspråk är i hög grad standardiserade i fråga om grammatik och ordförråd. Även mer särpräglade dialekter talas i mindre utsträckning. De är inte direkt hotade av språkdöd, men har varit på tillbakagång sedan början av 1900-talet, trots att många är välutforskade och att användningen idag inte längre motarbetas.", 
                         "sulautuneet. Neutrin epämääräinen artikkeli on ett, ei-neutrin en, ja vastaavasti neutrin määräiset muodot loppuvat t:hen ja ei-neutrin n:ään. Koska kielissä, joissa on erikseen maskuliini ja feminiini, ovat miehiä tarkoittavat sanat yleensä maskuliineja ja naisia tarkoittavat sanat feminiinejä, on ruotsin kielen yksinkertaistuminen johtanut siihen, että lähes kaikki ihmisiä tarkoittavat sanat kuuluvat yhteen sukuun")

#is textColumns_ in the right format?
#add labels?
predMatrix <- create_dtm_matrix(textColumns_ = predictionData, originalMatrix_ = dtMatrix)

#https://github.com/timjurka/RTextTools/issues/4
predSize <- length(predictionData)

#TODO: add labels here
y <- c('sv','fi')
y <- as.factor(y) #?
predictionContainer <- create_container(
    matrix = predMatrix,
    #labels = c('sv','fi'),
    #labels = rep(0,predSize),
    labels = y,
    testSize = 1:predSize,
    #trainSize left blank because we are using model
    virgin = FALSE #false when labels are known
)

#correct classes
y <- c('sv','fi')

results <- classify_model(predictionContainer, model)
results
  
accuracy <- compute_accuracy(results,y)

str(accuracy)

help("create_analytics")
analytics <- create_analytics(predictionContainer,results)
summary(analytics)
