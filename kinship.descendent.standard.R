
### id of the target ancestor individual
### g is the number of generation down
###


kinship.descendent.standard<-function(id,individual,fa,mo,g=99999){
  
  n <- length(individual)
  if (length(fa)!=n) stop("Mismatched length, individual and fa")
  if (length(mo)!=n) stop("Mismatched length, individual and mo")
  allped<-data.frame(individual,fa,mo)
  colnames(allped)<-c("individual","sire","dam")
  allped$individual<-as.character(allped$individual);allped$sire<-as.character(allped$sire);allped$dam<-as.character(allped$dam)
  
  self<-allped[which(allped$individual==id),]
  mo<-as.character(allped[which(allped$individual==id),3])
  fa<-as.character(allped[which(allped$individual==id),2])
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
        print(paste(iter, "is the largest number of generation down of ",id ))
        break # break when the size of the data frame don't increase, this mean the end of the descendents
      }
      nrow_raw<-nrow_new
    }
  }
  
  moped<-allped[which(allped$individual==mo),]
  faped<-allped[which(allped$individual==fa),]
  moped[1,2:3]<-NA;faped[1,2:3]<-NA
  self<-rbind(self,moped,faped)
  final_ped<-self
  return(final_ped)
}
