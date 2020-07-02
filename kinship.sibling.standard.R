### find full siblings and half-siblings
### return the pedigree of sibling, including the full and half siblings, the father and mother as founders.
### sib.type can be full, half, and all, means full-sibling only, half-sibling only and all sibling respectively.

kinship.sibling<-function(id,individual,fa,mo){
  
  individual<-as.character(individual);fa<-as.character(fa);mo<-as.character(mo)
  n <- length(individual)
  if (length(fa)!=n) stop("Mismatched length, individual and fa")
  if (length(mo)!=n) stop("Mismatched length, individual and mo")
  allped<-data.frame(individual,fa,mo)
  colnames(allped)<-c("individual","sire","dam")
  allped$individual<-as.character(allped$individual);allped$sire<-as.character(allped$sire);allped$dam<-as.character(allped$dam)
  
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
kinship.sibling<-function(id,individual,fa,mo,sib.type="all"){
  
  individual<-as.character(individual);fa<-as.character(fa);mo<-as.character(mo)
  n <- length(individual)
  if (length(fa)!=n) stop("Mismatched length, individual and fa")
  if (length(mo)!=n) stop("Mismatched length, individual and mo")
  allped<-data.frame(individual,fa,mo)
  colnames(allped)<-c("individual","sire","dam")
  allped$individual<-as.character(allped$individual);allped$sire<-as.character(allped$sire);allped$dam<-as.character(allped$dam)
  
  self<-allped[which(allped$individual==id),]
  moID<-as.character(allped[which(allped$individual==id),3])
  faID<-as.character(allped[which(allped$individual==id),2])
  if (sib.type=="all"){
    sibling_fa<-allped[which(allped$sire==faID),]
    
    sibling_mo<-allped[which(allped$dam==moID),]
    
    sibling<-unique(rbind(self,sibling_fa,sibling_mo))
    
    fa<-unique(as.character(sibling$sire))
    mo<-unique(as.character(sibling$dam))
    fa_mo<-data.frame("individual"=c(fa,mo),"sire"=rep(NA,length(c(fa,mo))),"dam"=rep(NA,length(c(fa,mo))))
    #fa_mo<-apply(fa_mo, 2, function(y) as.character(y))
    final_ped<-rbind(sibling,fa_mo)
    return(final_ped)
  } else if (sib.type=="full"){
    sibling_full<-allped[which(allped$sire==faID & allped$dam==moID),]
    sibling<-unique(rbind(self,sibling_full))
    
    fa<-unique(as.character(sibling$sire))
    mo<-unique(as.character(sibling$dam))
    fa_mo<-data.frame("individual"=c(fa,mo),"sire"=rep(NA,length(c(fa,mo))),"dam"=rep(NA,length(c(fa,mo))))
    #fa_mo<-apply(fa_mo, 2, function(y) as.character(y))
    final_ped<-rbind(sibling,fa_mo)
    return(final_ped)
  } else if (sib.type=="half"){
    sibling_fa<-allped[which(allped$sire==faID & allped$dam!=moID),]
    
    sibling_mo<-allped[which(allped$dam==moID & allped$sire!=faID),]
    
    sibling<-unique(rbind(self,sibling_fa,sibling_mo))
    
    fa<-unique(as.character(sibling$sire))
    mo<-unique(as.character(sibling$dam))
    fa_mo<-data.frame("individual"=c(fa,mo),"sire"=rep(NA,length(c(fa,mo))),"dam"=rep(NA,length(c(fa,mo))))
    #fa_mo<-apply(fa_mo, 2, function(y) as.character(y))
    final_ped<-rbind(sibling,fa_mo)
    return(final_ped)
  }
}
