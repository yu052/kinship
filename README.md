Few R functions to help to manage the pedigree records.  
kinship.make.ped is function to convert the normal pedigree records to a format needed by Kinship2 package in R.  
original pedigree records structure:
11 22 33
pedigree format of kinship2:
22 NA NA
33 NA NA
11 22 33  

kinship.ancestor.standard is the function to extract the given generations of ancestors of the given individual.  

kinship.descendant.standard is the function to extract the given number of generations descendants of a given individual.  

kinship.sibling.standard is a function to find all the siblings and half-siblings of a given individual.  

kinship.mate is a function to find all the individuals mated with a given individual.  

By playing around with these functions, you can manage you pedigree and generate pedigrees of individuals you are interested in in a big pedigree which is not possible to plot out.  For instance, if you would like to plot all the descendants of all sibling and half-sibling of a targeted individual, you could achieve it like this:
  sibling<-kinship.sibling.standart(id,individual,fa,mo)
  ### extract the descendants of all siblings
  sibling_descent_all<-data.frame()
  for (i in sibling[,1] ) {
    sibling_descent<-kinship.descendant.standard(i,g=1,individual,fa,mo)
    sibling_descent_all<-rbind(sibling_descent_all,sibling_descent)
  }
  sibling_descent_all<-unique(sibling_descent_all)
  ### if you would like to add a few generation of ancestor as well, you could use kinship.ancestor.standard(g,id,individual,fa,mo) function
  ### find founders
  sibling_descent_all<-kinship.make.ped(indidvidual,fa,mo)
  ### then add the sex, affection and other information. remove the duplicates, then it is ready for the pedigree() function of Kinship2 package.
