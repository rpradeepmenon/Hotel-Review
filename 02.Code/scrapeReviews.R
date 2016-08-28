
# main url for the hotel search
urlmainlist <- c(
  bangkok <- "https://www.tripadvisor.com.sg/Hotel_Review-g293916-d619165-Reviews-Ascott_Sathorn_Bangkok-Bangkok.html",
  singapore <-"https://www.tripadvisor.com.sg/Hotel_Review-g294265-d1083872-Reviews-Ascott_Raffles_Place_Singapore-Singapore.html",
  shanghai<- "https://www.tripadvisor.com.sg/Hotel_Review-g308272-d1745751-Reviews-Ascott_Huai_Hai_Road_Shanghai-Shanghai.html",
  beijing <- "https://www.tripadvisor.com.sg/Hotel_Review-g294212-d308420-Reviews-Ascott_Beijing-Beijing.html"
  )

# counter used along with main url for additional search pages for a hotel
morepglist=list(
  bangkok=seq(10,430,10),
  singapore=seq(10,210,10),
  shanghai=seq(10,160,10),
  beijing=seq(10,120,10)
)

# pick hotel for which review data is to be extracted
# choices: bangkok,singapore,shanghai,beijing
pickhotel="shanghai"

# get list of urllinks corresponding to different pages

# url link for first search page
urllinkmain=urlmainlist[pickhotel]
# counter for additional pages
morepg=as.numeric(morepglist[[pickhotel]])

urllinkpre=paste(strsplit(urllinkmain,"Reviews-")[[1]][1],"Reviews",sep="")
urllinkpost=strsplit(urllinkmain,"Reviews-")[[1]][2]

urllink=rep(NA,length(morepg)+1)

urllink[1]=urllinkmain
for(i in 1:length(morepg)){
  urllink[i+1]=paste(urllinkpre,"-or",morepg[i],"-",urllinkpost,sep="")
}
head(urllink)


# get summary content with rating

# note: there are few format that are not captured in scraping
# that is reflected in some reviews for Conrad not being extracted

dfrating.l=as.list(rep(NA,length(morepg)+1))

for(i in 1:(length(morepg)+1)){
  dfrating.l[[i]]=getOnePage(urllink[i])
}
dfrating=do.call(rbind,dfrating.l)
head(dfrating)

# save to Rdataset
filenm=paste("dfrating_",pickhotel,".Rda",sep="")
save(dfrating,file=filenm)

