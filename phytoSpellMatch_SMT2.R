### Code to correct spelling errors in taxa names 
# modified from vppatil/phytoDiv/phytoSpellMatch.r
# Susie Theroux Feb 2018 

setwd("~/Documents/R/PhytoDiv/")

# example data file with small errors in both genus and species
nametest<-read.csv('nametest.csv') # formatted with one column "Name" 

# accepted names file 
acceptednames<-read.csv("accepted.names.csv") # formatted with one column "FinalIDassigned", no duplicates 

# function for spelling matches. maxErr refers to maximum acceptable number of insertions/deletions/etc.
# first argument is the name to check, and the second is a vector of correctly spelled names
# second argument will eventually come from returns from an algaebase search.

# if two accepted names are equally different from the one you are checking, the function will not try to differentiate between them
bestmatch=function(enteredName,possibleNames,maxErr=3)
{
  for(i in 0:maxErr)
  {
    match=agrep(enteredName,possibleNames,max.distance=i,value=T)
    #if(length(match)==0) {return('noMatch')}
    if(length(match)==1) {return(match)}
    if(length(match)>1) {return('multiplePartialMatch')}
    nomatch=agrep(enteredName,possibleNames,max.distance=3,value=T)
    if(length(nomatch)==0) {return('noMatch')}
  }
}

# creating columns for the best matches found
nametest$Result=''

# run all names 
for(i in 1:dim(nametest)[1]) {
  nametest$Result[i]=bestmatch(nametest$Name[i],acceptednames$FinalIDassigned)
}



