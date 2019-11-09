library(arules)
library(tidyverse)
# default support=0.1, confidence=0.8, minlen=1, maxlen=10
data = read.transactions(file="Marketbasket.txt",format="basket",sep=",",cols=c(1))
inspect(data)
# mine the frequent itemsets
FreqItemsets <- apriori(data,parameter=list(support=0.3,target="frequent itemsets"))
inspect(FreqItemsets)
# mine the maximal frequent itemsets
MaxFreqItemsets <- apriori(data,parameter=list(support=0.3,target="maximally frequent itemsets"))
inspect(MaxFreqItemsets)
# mine the closed frequent itemsets
ClosedFreqItemsets <- apriori(data,parameter=list(support=0.3,target="closed frequent itemsets"))
inspect(ClosedFreqItemsets)
#define a functiion to find item in sepecial Itemsets
DetectItem<-function(item,set){
  signal=FALSE;
  for (var in set) {
    var=substr(var, 2, nchar(var)-1) 
    var=gsub(',','',var)
    if(item==var) {
      signal=TRUE
      break;
      }
  }
  return(signal)
}
Fi<-as.character(inspect(FreqItemsets)$items)
Mi<-as.character(inspect(MaxFreqItemsets)$items)
ci<-as.character(inspect(ClosedFreqItemsets)$items)
library(readxl)
Itemsetlattice<- read_excel("Itemsetlattice.xlsx")
result<-tibble(Itemset=NA,Label=NA)  #save all items and labels 
for(item in  Itemsetlattice$Item) {
  if (DetectItem(item,Mi) & !DetectItem(item,ci)){
    result<-add_row(result, Itemset = item,Label="M")
    next;
  }
  else if (DetectItem(item,ci) & !DetectItem(item,Mi)){
    result<-add_row(result, Itemset = item,Label="C")
    next;
  }
  else if (DetectItem(item,ci) & DetectItem(item,Mi)){
    result<-add_row(result, Itemset = item,Label="C,M")
    next;
  }
  else if (DetectItem(item,Fi)& !DetectItem(item,Mi)& !DetectItem(item,ci)){
    result<-add_row(result, Itemset = item,Label="F")
    next;
  }
  else{
    result<-add_row(result, Itemset = item,Label="I")
    next;
  }
}
result<-na.omit(result) 
 
