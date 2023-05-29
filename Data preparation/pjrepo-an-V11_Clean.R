library(readr)

selec0<-read_csv("test3.csv")
selec0<-as.data.frame(selec0)
selec<-selec0[,1:ncol(selec0)] 

j23<-selec$j23
jpo<-selec$jpo
an<-selec$an
id<-selec$id
age<-selec$age
agc<-selec$agc
agp<-selec$agp
agpc<-selec$agpc
exp0<-selec$exp0
expc<-selec$expc

st<-selec$st
stc<-selec$stc
pj<-selec$pj
nb<-selec$nb
perf<-selec$perf
sav<-selec$sav
qualfa<-selec$qualfal

arrival0<- read_csv("arrival.csv")
arrival0<-as.matrix(arrival0)
arrival<-arrival0[, 2:ncol(arrival0)]

an0<- read_csv("an.csv")
an0<-as.matrix(an0)

an<-an0[,2:ncol(an0)]

condition <- ( an <=2022)

hdv_f2000_f<- subset(arrival, condition)

an_2000_f<- subset(an, condition)


test6<-subset(selec, condition)


k<- an_2000_f

N <- nrow(test6)
T <- ncol(hdv_f2000_f) 
A <- max(k)-min(k)+1 



mi<-min(an_2000_f)

ma<-max(an_2000_f)


Yr<-an_2000_f-mi+1

save(hdv_f2000_f,N, T, A, ma, Yr, file = "anV11.RData" )



