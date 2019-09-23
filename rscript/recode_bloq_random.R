

load("C:/Users/Denise Laroze/Documents/GitHub/Mortgages-SERNAC/rscript/new.RData")


bdata2<-bdata
names(bdata2$x$genderQ)<-"age_cat_recode"


names(bdata2$x) <-  c("QID","age_cat_recode", "econQ", "Tr" )    


bdata2$trn<-c("1,2", "1,3", "1,4",  "2,3", "2,4" , "3,4")
bdata2$ncv <-c("age_cat_recode", "econQ")  
bdata2$ocv <-c("age_cat_recode", "econQ")    

bdata2$tr.sort<-c("1,2", "1,3", "1,4",  "2,3", "2,4" , "3,4")
names(bdata2$orig)<-c("QID","age_cat_recode", "econQ", "Tr" )  


part.data2<-part.data

names(part.data2)<-c("QID","age_cat_recode", "econQ")

bdata<-bdata2
part.data<-part.data2

rm(bdata2, part.data2)