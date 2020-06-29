
### id of the target ancestor individual
### wholeped is the complete pedigree records, given in string
#wholeped<-"allped"
kinship.descendent.standard<-function(id,individual,fa,mo,g=99999){
  individual<-as.character(individual)
  fa<-as.character(fa)
  mo<-as.character(mo)
  allped<-data.frame(individual,fa,mo)
  colnames(allped)<-c("individual","sire","dam")
  allped$individual<-as.character(allped$individual);allped$sire<-as.character(allped$sire);allped$dam<-as.character(allped$dam)
  #allped<-get(wholeped)
  self<-allped[which(allped$individual==id),]
  moNHSB<-as.character(allped[which(allped$individual==id),3])
  faNHSB<-as.character(allped[which(allped$individual==id),2])
  nrow_raw<-nrow(self)
  if (!(id %in% c(allped$sire,allped$dam))){
    self<-self
    print(paste(id,"no descendents"))
  }else{
      for (iter in 1:g) {
    for (id in self$individual ) {
      fa<-allped[which(allped$sire==id),]
      mo<-allped[which(allped$dam==id),]
      self<-rbind(self,fa,mo)
      self<-data.frame(apply(self, 2, function(y) as.character(y)))
      self<-unique(self)
    }
    self<-unique(self)
    nrow_new<-nrow(self)
    if (nrow_new==nrow_raw) {
      break # break when the size of the data frame don't increase, this mean the end of the descendents
    }
    nrow_raw<-nrow_new
  }
  }

  mo<-allped[which(allped$individual==moNHSB),]
  fa<-allped[which(allped$individual==faNHSB),]
  mo[1,2:3]<-NA;fa[1,2:3]<-NA
  self<-rbind(self,mo,fa)
  final_ped<-self
  return(final_ped)
}
