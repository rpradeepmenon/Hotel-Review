setwd("~/AscottReview")
library(dplyr)
library(lubridate)
library(ggplot2)
library(tm)
library(scales)
library(topicmodels)

hotellist=c("bangkok","singapore","shanghai","beijing")

dfrating.l=lapply(hotellist,
                  function(x) {
                    filenm=paste("dfrating_",x,".Rda",sep="")
                    load(filenm)
                    return(list(dfrating=dfrating,
                                first3records=head(dfrating,3),
                                numrevs=nrow(dfrating),
                                freqRating=table(dfrating$rating)))
                  })
names(dfrating.l)=hotellist

# Explore top level quotes for each rating

# function to get document-term matrix from hotel review data for a given hotel
getDTM=function(dftxt){
  
  # code adapted from http://www.rdatamining.com/examples/text-mining
  txtcorpus=Corpus(VectorSource(dftxt))
  #inspect(txtcorpus[1:5])
  

  txtcorpus.cl <- tm_map(txtcorpus,content_transformer(tolower))
  txtcorpus.cl <- tm_map(txtcorpus,content_transformer(removePunctuation))
  txtcorpus.cl <- tm_map(txtcorpus,content_transformer(removeNumbers))

  
  mystopwords=c(stopwords("english"),"more","staff","room","rooms","bangkok","sathorn","conference","ascot","business","beijing","stay","stayed","huai hai","ascott","shanghai","apartment","apartments","singapore","weekend")
  txtcorpus.cl=tm_map(txtcorpus.cl,removeWords,mystopwords)
  #dictCorpus=txtcorpus.cl
  #txtcorpus.cl=tm_map(txtcorpus.cl,stemDocument)
  #txtcorpus.cl=tm_map(txtcorpus.cl,stemCompletion,dictionary=dictCorpus)
  
  dtm <- DocumentTermMatrix(txtcorpus.cl)
  dtm
  dtm.m <- as.matrix(dtm)
  
  return(dtm.m)
}

getTopTerms=function(hotel){
  
  # get review data
  dfrating=dfrating.l[[hotel]]$dfrating
  
  minrating=min(dfrating$rating)
  maxrating=max(dfrating$rating)
  tfreq.l=as.list(rep(NA,maxrating-minrating+1))
  
  # of frequent words to retain
  numterms=20
  
  rating=maxrating+1
  
  for(i in 1:(maxrating-minrating+1)){
    
    rating<-rating-1
    #print("Processing data for %d stars",rating)
    
    dftxt<-dfrating$quote[dfrating$rating==rating]
    
    dtm.m=getDTM(dftxt)
    
    tfreq=colSums(dtm.m)
    tfreq.l[[i]]=names(sort(tfreq,decreasing=TRUE)[1:numterms])
  }
  
  topTerms=do.call(cbind,tfreq.l)
  colnames(topTerms)=paste(seq(maxrating,minrating)," star")
  
  return(list(dtm.m=dtm.m,topTerms=topTerms))
  
}
topTerms.l=lapply(hotellist,function(x) getTopTerms(x))
names(topTerms.l)=hotellist


#commands
#dfrating.l[[pickhotel]]$first3records
#dfrating.l[[pickhotel]]$numrevs
#dfrating.l[[pickhotel]]$freqRating

#topTerms.l[[pickhotel]]$topTerms
#save(topTerms.l,file="topTerms.Rda")