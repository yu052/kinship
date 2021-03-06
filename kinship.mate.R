
### id of the target ancestor individual


kinship.mate<-function(id,individual,fa,mo){
  individual<-as.character(individual);fa<-as.character(fa);mo<-as.character(mo)
  n <- length(individual)
  if (length(fa)!=n) stop("Mismatched length, individual and fa")
  if (length(mo)!=n) stop("Mismatched length, individual and mo")
  allped<-data.frame(individual,fa,mo)
  colnames(allped)<-c("individual","sire","dam")
  allped$individual<-as.character(allped$individual);allped$sire<-as.character(allped$sire);allped$dam<-as.character(allped$dam)
  #allped<-get(wholeped)
  self<-allped[which(allped$individual==id),]
  
  if((id %in% fa)&(id %in% mo) ){
    stop(paste(id," is both male and female, gender wrong"))
  }else if((id %in% fa) & !(id %in% mo)){
    ### id is a father
    mate_list<-allped[which(allped$sire==id),3]
    
  }else if (!(id %in% fa) & (id %in% mo)){
    ### id is a mother
    mate_list<-allped[which(allped$dam==id),2]
  }
  mate_list<-mate_list[!is.na(mate_list)]
  mate_list<-unique(mate_list)
  return(mate_list)
}
