### find full siblings and half-siblings

kinship.sibling<-function(id,individual,fa,mo){
  
  individual<-as.character(individual)
  fa<-as.character(fa)
  mo<-as.character(mo)
  allped<-data.frame(individual,fa,mo)
  colnames(allped)<-c("individual","sire","dam")
  
  self<-allped[which(allped$individual==id),]
  moID<-as.character(allped[which(allped$individual==id),3])
  faID<-as.character(allped[which(allped$individual==id),2])
  
  sibling_fa<-allped[which(allped$sire==faID),]
  
  sibling_mo<-allped[which(allped$dam==moID),]
  
  sibling<-unique(rbind(self,sibling_fa,sibling_mo))
  
  fa<-unique(as.character(sibling$sire))
  mo<-unique(as.character(sibling$dam))
  fa_mo<-data.frame("individual"=c(fa,mo),"sire"=rep(NA,length(c(fa,mo))),"dam"=rep(NA,length(c(fa,mo))))
  #fa_mo<-apply(fa_mo, 2, function(y) as.character(y))
  final_ped<-rbind(sibling,fa_mo)
  return(final_ped)
}

