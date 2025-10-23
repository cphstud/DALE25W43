library(dplyr)
library(tidyr)
library(ggplot2)

# den lille tabel
colldf = as.data.frame(matrix(data=NA,nrow = 10,ncol = 10))
# 
# lav en sekvens som du looper med.
for (i in 1:10) {
  for( j in 1:10) {
    colldf[i,j]=i*abs(11-j)
  }
}


### MALE RUN ###
names=c("Bjarne","Ib","Verner","Otto")
namesv=rep(names,times=3)

resultater=as.integer(runif(12,20,35))
now=Sys.Date()
tidspunkt=c(now-7,now-14,now-21)
tidspunktv=rep(tidspunkt,each=4)
tidspunktv

rundf=data.frame(deltagere=names,løb=tidspunktv,resultater=resultater)


### ADD FEMALE ###
# stamdata
names=c("Lone","Kaja","Bjarne","Ib","Verner","Otto")
#names=c("Bjarne","Ib","Verner","Otto")
gender=rep(c("M","K"),times=3)
age=c(34,32,45,21,22,21)
vej=c("SMVej","Villavej","Tornevej","Sidevej","Hovedvej","Pivot Drive")
zip=c("1800","1800","2300","2100","2100","8000")

stamdf=data.frame(navne=names,alder=age,vej=vej,postnr=zip)
#stamdf=data.frame(gender=gender,navne=names,alder=age,vej=vej,postnr=zip)

### JOIN RUN AND STAM ###
runWithStam = left_join(rundf,stamdf, by=c("deltagere"="navne"))
#runWithOnlyStam = inner_join(rundf,stamdf, by=c("deltagere"="navne"))
#goneRunners=anti_join(rundf,stamdf, by=(c("deltagere"="navne")))

# average løbetid på løberne
avrundf <- runWithStam %>% group_by(deltagere) %>% 
  summarise(avtid=mean(resultater))



### PLAYING WITH THE NEW DATATYPE: LISTS ###
# lav en liste over mulige hold med 1 til 4 deltagere
testliste=list()
testliste['names']=list(c("Kurt","Anton"))
testliste['parts']=list(c("Anton"))
testliste['girls']=list(c("Mona","Ib","Anton"))
testliste['scores']=list(1:200)

for(element in testliste) {
  print(element)
}

# loop igennem og gør noget ved hvert element
lapply(testliste, function(x) length(x))


### CONTINUE RUNNERS USING COMBN TO GENERATE TEAMS ###
### 4 STEPS ILLUSTRATING THE SCATEBOARD APPROACH ###

# kombinationer af hold i en liste
df2=combn(names,2,simplify = F)

teamlist=list()
for(i in 1:length(names)) {
  #lav en kombination af i holdstørrelse  
  df2=combn(names,i,simplify = F)
  # put ind i listen
  teamlist[i]=list(df2)
}

#step 1
for (i in (1:length(teamlist))) {
  i=3
  print(teamlist[[i]])
  print(length(teamlist[[i]]))
  print(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[1]]])
  print(mean(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[1]]]))
  #print(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[2]]])
}

# step 2
for (i in (1:length(teamlist))) {
  #print(teamlist[[i]])
  i=1
  cat("Leng ",length(teamlist[[i]]))
  for (j in (1:length(teamlist[[i]]))) {
    j=2
    print(teamlist[[i]][[j]])
    print(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[j]]])
    print(mean(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[j]]]))
    # gem informationen på holdet
  }
}
# step 3 Gem løbsinfo på hvert hold
dfcoll=as.data.frame(matrix(nrow = 15, ncol = 2))
for (i in (1:length(teamlist))) {
  #print(teamlist[[i]])
  cat("Leng ",length(teamlist[[i]]))
  for (j in (1:length(teamlist[[i]]))) {
    print(teamlist[[i]][[j]])
    print(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[j]]])
    print(mean(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[j]]]))
    tmpspeed=mean(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[j]]])
    # gem informationen på holdet
    teamlist[[i]][[j]]['speed']=tmpspeed
  }
}
# step 4 Gem løbsinfo på hvert hold
dfcoll=as.data.frame(matrix(nrow = 1, ncol = 2))
colnames(dfcoll)=c("deltagere","speed")
for (i in (1:length(teamlist))) {
  for (j in (1:length(teamlist[[i]]))) {
    tmpspeed=mean(avrundf$avtid[avrundf$deltagere %in% teamlist[[i]][[j]]])
    nv=paste(teamlist[[i]][[j]], collapse = " ")
    dfcoll=rbind(dfcoll,data.frame(deltagere=nv,speed=tmpspeed))
  }
}
# test-sample
teamlist[[2]][[4]]['speed']

# step 5 Læg hold og avtid ind i en ny dataframe
dfcoll=as.data.frame(matrix(nrow = 15, ncol = 2))
for (i in (1:length(teamlist))) {
  print(teamlist[[i]])
  tl=teamlist[[i]]
  sl=sapply(tl,function(x) x['speed'])
  dfcoll=rbind(dfcoll,)
}






