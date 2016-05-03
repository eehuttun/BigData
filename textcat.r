library(textcat)
library(tm)
library(hash)

setwd("/home/vaimi/Lataukset/wikidumps")
docs <- Corpus(DirSource(directory = "1000",encoding = "UTF-8",mode = "text"),
readerControl = list(reader = readPlain))

#Remove numbers
docs <- tm_map(docs, removeNumbers)

#Add language meta data to each of the documents in the Corpus
languages=list.files(path = "1000")
languages=gsub("_.*","",languages)
for (i in 1:length(docs)) {
  DublinCore(docs[[i]], "language") <- languages[i]
}

#Create language profiles from the training set using the 300 most frequent n-grams in the languages
trainsize=800;
h=hash()
for (i in 1:length(docs)){
  train_text=docs[[i]]$content[1:trainsize]
  lang=docs[[i]]$meta$language
  .set(h, lang, train_text)
}
train_list=as.list.hash(h)
lang_profiles = textcat_profile_db(train_list,id=names(train_list),size=300)

#Create test sets with 100 sentences per language
testsize=200;
h=hash()
for (i in 1:length(docs)){
  train_text=docs[[i]]$content[(trainsize+1):(trainsize+testsize)]
  lang=docs[[i]]$meta$language
  .set(h, lang, train_text)
}
test_list=as.list.hash(h)

#Evaluate accuracy
h=c()
for (i in 1:length(test_list)){
  for (j in 1:length(test_list[[i]])){
    #categorize sentence
    d = textcat_xdist(test_list[[i]][j], p=lang_profiles, method="CT")
    lang=colnames(d)[which(d == min(d), arr.ind = TRUE)[2]]
    orig_lang=names(test_list)[i]
    h=rbind(h,c(orig_lang,lang))
    #print the counter to check how long the process will take
    print(i)
  }
}
colnames(h)=c("original","predicted")
res = split(h[,'predicted'], h[,'original']) 

acc=c()
for (i in 1:length(res)){
  ac=(sum(res[[i]]==names(res[i])))/(length(res[[i]]))
  acc=rbind(acc,c(names(res[i]),ac))
}

#Sorting the distances for languages so see the closest ones
res=t(apply(d,1,sort))

