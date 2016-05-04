library(e1071)
library(RTextTools)
library(tm)

#computes the accuracy of a classifier given its results
compute_accuracy <- function(results,labels) {
    n <- length(labels)
    no_correct <- sum(results$SVM_LABEL == labels)
    accuracy <- no_correct/n
    
    return(accuracy)
}

#creates document term matrix
create_dtm_matrix <- function(textColumns_, lang_ = NA, minDocFreq_ = 1,
maxDocFreq_ = Inf, maxWordLength_ = Inf, ngramLength_ = 1, originalMatrix_ = NULL,
removeNumbers_ = TRUE, removePunctuation_ = TRUE, removeStopWords_ = FALSE, 
stemWords_ = FALSE, stripWhiteSpace_ = TRUE, toLower_ = TRUE, weighting_ = weightTf) {
    dtMatrix <- create_matrix(
        textColumns = textColumns_, lang = lang_, minDocFreq = minDocFreq_, 
        maxDocFreq = maxDocFreq_, maxWordLength = maxWordLength_, 
        ngramLength = ngramLength_, originalMatrix = originalMatrix_,
        removeNumbers = removeNumbers_, removePunctuation = removePunctuation_,
        removeStopwords = removeStopWords_, stemWords = stemWords_,
        stripWhitespace = stripWhiteSpace_, toLower = toLower_,
        weighting = weighting_
    )

    return(dtMatrix)
}