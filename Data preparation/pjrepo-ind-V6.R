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
arrival0<-as.data.frame(arrival0)
arrival<-arrival0[, 2:ncol(arrival0)]

id0<- read.csv("id.csv")
id0<- as.data.frame(id0)
id<- id0[,2:ncol(id0)]

an0<- read_csv("an.csv")
an0<-as.matrix(an0)
an<-an0

condition <- (an<= 2022)



hdv_f2000_f<- subset(arrival, condition)
an_2000_f<- subset(an, condition)
ind_2000_f<- na.omit(subset(id$V2, condition))
age_2000_f<- subset (agc, condition)


test6<-subset(selec, condition)

k<- an_2000_f

N <- nrow(hdv_f2000_f)
T <- ncol(hdv_f2000_f)
A <- max(k)-min(k)+1 

U_No<- unique(ind_2000_f)
U_NoNA<- na.omit(U_No)
I <- length(U_NoNA)

MaxAge<- max(age_2000_f)


ct<- cbind(U_NoNA, c(1:I))

ctb<- vector(length = length(ind_2000_f))

for (i in (1:length(ind_2000_f))){
  Ind<- ind_2000_f[i]
  a<- ct[ct[,1] == Ind, 2]
  ctb[i]<- a[1]
  
}


ct_code <- cbind((ind_2000_f), ctb)

g<- c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)


save(hdv_f2000_f,N, T, A, I, ct_code, MaxAge, age_2000_f,g, file = "IdAV6.RData" )

