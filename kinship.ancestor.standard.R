### g indicate the number of generations to look up
### id is the NHSB id of the target individual

kinship.ancestor<-function(g,id,individual,fa,mo){
  individual<-as.character(individual)
  fa<-as.character(fa)
  mo<-as.character(mo)
  allped<-data.frame(individual,fa,mo)
  colnames(allped)<-c("individual","sire","dam")
  try(if(g<2) stop("generation number should bigger than 1"))
  self<-allped[which(allped$individual==id),]
  fa<-allped[which(allped$individual==as.character(self$sire)),]
  mo<-allped[which(allped$individual==as.character(self$dam)),]
  ped<-rbind(self,fa,mo)
  if (g == 1) {
    return(ped)
    } else {
  for (g in 2:g) {
    number_individual<-nrow(ped)
    for (i in 1:number_individual) {
      fa<-allped[which(allped$individual==as.character(ped[i,2])),]# get info of every father in ped
      mo<-allped[which(allped$individual==as.character(ped[i,3])),]# get info of every mother in ped
      ped<-rbind(ped,fa,mo)
      
    }
    ped<-unique(ped)
  }
  ped<-apply(ped, 2, function(y) as.character(y))
  ped[!(ped[,2] %in% ped[,1]),2:3]<-NA
  ped[!(ped[,3] %in% ped[,1]),2:3]<-NA
  return(ped)
 }
}
