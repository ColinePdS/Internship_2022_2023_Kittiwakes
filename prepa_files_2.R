
library(readr)
test1 <- read_csv("PJREPRO2.CSV")
test1<-as.data.frame(test1)

#### s?lection des donn?es ####
#que les ann?es >=84 (? cause de la mise au point du protocole sur le terrain)
test2<-subset(test1, an>1983)
#dim(test2) 16841



#que les lignes avec pj connu
test3<-subset(test2, pj!=0)



test3<- subset(test3, sx!= 9)# que les sexes connus
#dim(test3b)
write_csv(test3, "test3.csv")



#selection voulue affectée à l'objet selec
selec0<-read_csv("test3.csv")


selec<-as.data.frame(selec0[,1:ncol(selec0)])                 

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

#summary(pj)

###########################################################################
#Histoire de vie des individus ----

## d?coupage de l'ann?e en periode de 7 jours

julien <- matrix(ncol = 2, nrow = 274)#matrice contenant tous les jours jusqu'à fin septembre (normalement les oiseaux partent debut sept maxi)
julien <- as.data.frame(julien)
names(julien)[names(julien)=="V1"]<- "JJ"#jour julien
names(julien)[names(julien)=="V2"]<- "reg"#regroupement
julien$JJ<- c(1:274)

nweek<- function(vector1, vector2){
  for (i in (1:length(vector1))){  #encore une boucle ...
    reste<- (vector1[i]-1)/7# pour le jour 7 -> (7-1)/7 = 0
    vector2[i]<- floor(reste)+1#0+1 = 1 er regroupement
    
    
  }
  return (vector2)
}

julien$reg <- nweek (julien$JJ, julien$reg)

#head(julien)

#cr?ation d'une matrice "Histoire de Vie"
hdv <- matrix(ncol = (max(julien$reg)+4), nrow = nrow(selec))#matrice avec ncol = nonbre de semaines + 4 (id *2+an + sex), nrow = nb de lignes dasn le tableau de base (test4)
hdv<- as.data.frame(hdv)
hdv$V1<- c(1:nrow(selec))#numero de ligne
hdv$V2<- selec$id#id de l'individu
hdv$V3<- selec$an#annee t
hdv$V4<- selec$sx#sex
hdv$V5 <- selec$sx # on code le sexe sur deux colonnes

#head(hdv)

sexe_binaire <- function(L1, L2){ # function qui remplace FF par 10 et MM par 01,
 if (L1 == 'F'|L1 == 'f'|L2 == 'F'|L2=='f'){# de ce fait, il y a tj une colonne ou le sexe est ?gal ? 0
   L1<- 1                                  #?a va avoir son importance dans le mod?le (n?cessit? d'un 0 pour annulation d'une partie de l'?quation.)
   L2<- 0
 }
 else if (L1 == 'M'|L1 == 'm'| L2 == 'M'|L2 == 'm'){
   L1<- 0
   L2<- 1
 }
 return(((L1: L2)))
}

sexe_binaire('F','F')#test

sx<- matrix(nrow =nrow(hdv), ncol = 2 )# on stocke le sexe binaire ici, ce qui ?vite des histoires de classes de variables

for (i in (1:nrow(hdv))){
  
 sx[i,1]<- sexe_binaire(hdv[i,4], hdv[i,5])[1]
 sx[i,2]<- sexe_binaire(hdv[i,4], hdv[i,5])[2]
 
}

hdv

sx<- as.data.frame(sx)

for (i in (1:max(hdv$V2))){#i in 1: le numero d'identification individuel maximum
  for (j in (1:nrow(hdv))){#pour chaque ligne du tableau
    if (i == hdv$V2[j]){ # si l'ndividu i existe dans le subset hdv
      
      week<- julien$reg[julien$JJ==selec[j,12]]# on ajoute un 1 lors de son apparition
      hdv[j,(week+5)]<- 1# attention aux cinqs colonnes d'id
    }
  }
}


#hdv
#dim(hdv) 16713

#fonction permettant de determiner la position de la premi?re occurence d'une valeur differente de NA dans un vecteur
apparition <- function (vector){
  i = 1
  while(is.na(vector[i]) == TRUE) {
    i=i+1
  }
  return (i)
}


# fonction permettant de remplacer les NA pr?c?dent 1 par 0
NAto0 <- function(matrix){
  
  ap <- 0
  
  for(i in 1:nrow(matrix))
    
  {
    ap <- apparition (matrix[i,])
    for (j in 1:(ap-1))
    {
      matrix[i,j]<- 0#des 0 avant le 1 d'apparition
    }
    for (j in ap:(ncol(matrix)))
    {
      matrix[i,j]<- 1#et des 1 une fois que l'animal est l?
    }
    
  }

  return (matrix)
}


hdv2<- hdv[,-(1:5)] #retire les 5 colonnes d'identification
hdv2<- as.matrix((hdv2))

hdv2<-NAto0 (hdv2)# histoire de vie /!\ mets quelques temps ? tourner

#hdv2

id<- (cbind(hdv[,1:5]))#5 colonnes d'id
hdv_f<- cbind (id, hdv2)#on concatene le tout

#hdv_f #data_frame_final 
#dim(hdv_f)
#dim(test4)

arrival <- (hdv_f)[,6:43]
#arrival

#library("readr")

write_csv(arrival, file='arrival.csv')
write_csv(sx[1], file='sex.csv')
write_csv(id[3], file='an.csv')
################################################

