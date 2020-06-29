### sort the pedigree not in a format of kinship2, to extract the founders and make the records of the founders
### give the id, faid, moid, return a compact pedigree with three columns, individual, sire, dam
kinship.make.ped<-function(individual,fa,mo){
  individual<-as.character(individual)
  fa<-as.character(fa)
  mo<-as.character(mo)
  allped<-data.frame(individual,fa,mo)
  colnames(allped)<-c("individual","sire","dam")
  allped$individual<-as.character(allped$individual);allped$sire<-as.character(allped$sire);allped$dam<-as.character(allped$dam)
  fa_founder<-allped[-which(allped$sire %in% allped$individual),2]
  mo_founder<-allped[-which(allped$dam %in% allped$individual),3]
  individual_founder<-c(fa_founder,mo_founder)
  founder<-data.frame("individual"=individual_founder,
                      "sire"=rep(NA,length(individual_founder)),
                      "dam"=rep(NA,length(individual_founder)))
  allped<-rbind(allped,founder)
  allped<-allped[!is.na(allped$individual),]
  return(allped)
}