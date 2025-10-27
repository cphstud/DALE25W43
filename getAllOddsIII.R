library(jsonlite)
library(dplyr)
library(httr)
library(stringr)
source("util.R")

baseurl <- "https://opensky-network.org/api"
# various endpoints
endpoints <- "/states/all"
endpointFl <- "/flights/aircraft?icao24="
endpointTrack <- "/tracks/all?icao24="

begin=as.integer(Sys.time())-86400
end=as.integer(Sys.time())

#icaos for test
icao <- "480c42"
icao <- "4ab821"

#flightquery
query <- paste0(baseurl,endpointFl,icao,"&begin=",begin,"&end=",end)
#trackquery
query <- paste0(baseurl,endpointTrack,icao,"&time=",end)

# run the testqueries
res=GET(url=query, add_headers(Authorization=paste("Bearer",token)) )
res$status_code
resdf=as.data.frame(fromJSON(content(res,as = "text")))

# auth with token
#token=Sys.getenv("TOKEN")
token=getToken()


#### NORTH SEA ####
#-6.065140,52.055129,4.305954,56.196869

lamin=52.055129
lamax=56.196869
lomin=-6.065140
lomax=4.305954

token=getToken()
fullurl=paste0("https://opensky-network.org/api/states/all?lamin=",lamin,"&lomin=",lomin,"&lamax=",lamax,"&lomax=",lomax)
resNS <- GET(
  fullurl,
  add_headers(Authorization = paste("Bearer", token))
)
resNS$status_code
rescontent <- httr::content(resNS, as="text")
resretval <- jsonlite::fromJSON(rescontent)
resretvalNS <- as.data.frame(resretval$states)

# get all icaos
icaoNS=resretvalNS$V1
icaoNS[12]
icaoNS=icaoNS[1:85]
icaoNS

# COLLECT IN DATAFRAME
dficaos=data.frame(icao=icaoNS,sdcourse=rep(0,length(icaoNS)),isOff=rep(0,length(icaoNS)))
# get status from tracks pr icao
#dficaos$sdcourse=lapply(dficaos$icao,FUN=getTracks)

testdd=getTracks(icaoNS[12])

# isOdd-functions
nv2=paste0(nv,collapse = ",")
nv2
token=getToken()
# getTracks-functions
getTracks <- function(icao) {
  standdev=0
  colnv=c("icao24","callsign","startTime","endTime","time","lat","lng","alt","crs","grd")
  resretvaldf=NULL
  baseurl <- "https://opensky-network.org/api"
  endpointTrack <- "/tracks/all?icao24="
  end=as.integer(Sys.time())
  end=0
  query <- paste0(baseurl,endpointTrack,icao,"&time=",end)
  query
  res <- GET(
    query,
    add_headers(Authorization = paste("Bearer", token))
  )
  res$status_code
  if(res$status_code == 200) {
    rescontent <- httr::content(res, as="text")
    resretval <- jsonlite::fromJSON(rescontent)
    resretvaldf=as.data.frame(resretval)
  } 
  tryCatch(
    {
  colnames(resretvaldf)=colnv
  resdfA <- resretvaldf %>% filter(alt > 300)
  standdev=sd(resdfA$crs)
  #return(resretvaldf)
    }, error = function(e) {
      print(e)
    }
  )
  return(standdev)
}

circdf=readRDS("circjet7.rds")
ndf=readRDS("normjet1.rds")
plot(resretvaldf$lat,resretvaldf$lng)
plot(resretvaldf$time,resretvaldf$crs)
plot(ndf$lat,ndf$lng)
plot(ndf$time,ndf$crs)
plot(circdf$time,circdf$crs)
plot(circdf$lat,circdf$lng)

getFL <- function(icao,ts) {
  baseurl="https://opensky-network.org/api/states/all?icao24="
  toturl=paste0(baseurl,icao,"&time=",ts)
  rawres= GET(url=toturl, add_headers(Authorization = paste("Bearer", token)))
  if (rawres$status_code != 200) {
    print(toturl)
    print(rawres$status_code)
  } else {
    rawcontent=rawres$content
    rawcontent=httr::content(rawres, as = "text")
    realcontent=fromJSON(rawcontent)
    retval=realcontent
  }
  return(retval)
}

