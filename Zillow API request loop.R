#libraries and functions
library(ZillowR)
library(dplyr)
library(XML)

EmptytoNA <- function(x){
  if(identical(x,character(0))||is.null(x)){
    x<-NA
  }
  return(x)
}

#set up Zillow API keyes
zwsid<-c()
zwsid[1]<-'X1-ZWz1hetbmyoqa3_13bwv'
zwsid[2]<-'X1-ZWz1hh7hbjqzgr_2hwdv'
zwsid[3]<-'X1-ZWz17jazmvvy17_2jayc'
zwsid[4]<-'X1-ZWz1hh7l9lz3ez_2kpit'
zwsid[5]<-'X1-ZWz1hh7t5qfbbf_2qbsp'
z<-1

set_zillow_web_service_id(zwsid[z])

#load address frame and sample
frame<-readRDS("Address Frame.RData")
set.seed(Sys.Date())

smp_frame<-sample_n(frame,250)
stop<-FALSE

# start the loop
for(j in 1:nrow(smp_frame)){
  number<-seq(min(smp_frame$FROMHN[j],smp_frame$TOHN[j]),max(smp_frame$FROMHN[j],smp_frame$TOHN[j],by=2))
  table_zlw_j<-tibble()
  n<-1
  for(n in 1:length(number)){
    address<-paste(number[n],smp_frame$FULLNAME[j])
    Result<-GetDeepSearchResults(address, citystatezip = 'Flint, MI',rentzestimate=TRUE)
    if (Result$message$code==7&z==length(zwsid)){
      stop = TRUE
      break
    }
    if (Result$message$code==7){
      z<-z+1
      print(Result$message$text)
      set_zillow_web_service_id(zwsid[z])
    }
    if (Result$message$code==0){
      basic_info<-Result[["response"]][["results"]][["result"]]
      zpid<-basic_info[["zpid"]] %>% xmlValue()
      address_resp<-basic_info[["address"]][["street"]] %>% xmlValue()
      type<-basic_info[["useCode"]] %>% xmlValue()%>% EmptytoNA()
      taxAssessment<-basic_info[["taxAssessment"]] %>% xmlValue()%>% EmptytoNA()
      taxAssess_year<-basic_info[["taxAssessmentYear"]] %>% xmlValue()%>% EmptytoNA()
      yearBuilt<-basic_info[["yearBuilt"]] %>% xmlValue()%>% EmptytoNA()
      lotSizeSqFt<-basic_info[["lotSizeSqFt"]] %>% xmlValue()%>% EmptytoNA()
      finishedSqFt<-basic_info[["finishedSqFt"]] %>% xmlValue()%>% EmptytoNA()
      bathrooms<-basic_info[["bathrooms"]] %>% xmlValue()%>% EmptytoNA()
      bedrooms<-basic_info[["bedrooms"]] %>% xmlValue()%>% EmptytoNA()
      lastSoldDate<-basic_info[["lastSoldDate"]] %>% xmlValue()%>% EmptytoNA()
      lastSoldPrice<-basic_info[["lastSoldPrice"]] %>% xmlValue()%>% EmptytoNA()
      
      Zestimate_data<-Result[["response"]][["results"]][["result"]][["zestimate"]] 
      zestimate<-Zestimate_data[["amount"]] %>% xmlValue() %>% EmptytoNA()
      zest_last_updated<-Zestimate_data[["last-updated"]] %>% xmlValue() %>% EmptytoNA()
      zest_valueChange<-Zestimate_data[["valueChange"]] %>% xmlValue() %>% EmptytoNA()
      zest_upper_range<-Zestimate_data[["valuationRange"]][["high"]] %>% xmlValue() %>% EmptytoNA()
      zest_lower_range<-Zestimate_data[["valuationRange"]][["low"]] %>% xmlValue() %>% EmptytoNA()
      
      Rent_data<-Result[["response"]][["results"]][["result"]][["rentzestimate"]]
      rentzestimate<-Rent_data[["amount"]] %>% xmlValue() %>% EmptytoNA()
      rent_last_updated<-Rent_data[["last-updated"]] %>% xmlValue() %>% EmptytoNA()
      rent_valueChange<-Rent_data[["valueChange"]] %>% xmlValue() %>% EmptytoNA()
      rent_upper_range<-Rent_data[["valuationRange"]][["high"]] %>% xmlValue() %>% EmptytoNA()
      rent_lower_range<-Rent_data[["valuationRange"]][["low"]] %>% xmlValue() %>% EmptytoNA()
      
      Address_data<-Result[["response"]][["results"]][["result"]][["address"]]
      Lat<-Address_data[["latitude"]] %>% xmlValue() %>% EmptytoNA()
      Long<-Address_data[["longitude"]] %>% xmlValue() %>% EmptytoNA()
      
      table_zlw_temp<-tibble(address,address_resp<-as.character(address_resp),ZPID=as.numeric(zpid),type=as.character(type),taxAssessment=as.numeric(taxAssessment),
                             taxAssess_year=as.character(taxAssess_year),yearBuilt=as.numeric(yearBuilt),lotSizeSqFt=as.numeric(lotSizeSqFt),
                             finishedSqFt=as.numeric(finishedSqFt),bathrooms=as.numeric(bathrooms),bedrooms=as.numeric(bedrooms),
                             lastSoldPrice=as.numeric(lastSoldPrice),lastSoldDate=as.character(lastSoldDate),
                             zestimate=as.numeric(zestimate),zest_valueChange=as.numeric(zest_valueChange),
                             zest_upper_range=as.numeric(zest_upper_range),zest_lower_range=as.numeric(zest_lower_range),zest_last_updated=as.character(zest_last_updated),
                             rentzestimate=as.numeric(rentzestimate),rent_valueChange=as.numeric(rent_valueChange),
                             rent_upper_range=as.numeric(rent_upper_range),rent_lower_range=as.numeric(rent_lower_range),
                             rent_last_updated=as.character(rent_last_updated),
                             lat=as.character(Lat),long=as.character(Long),zip=as.character(smp_frame$ZIP[j]),download_date=Sys.Date())
      table_zlw_j<-rbind(table_zlw_j,table_zlw_temp)
    }
  }
  if (stop){break}
  table_zlw<-rbind(table_zlw,table_zlw_j)
  print(c(paste("z=",z),paste("j=",j),paste("nrow=",nrow(table_zlw))))
}

save.image(paste("Zillow data",Sys.Date,".Rdata"))


# data check
length(unique(table_zlw$ZPID))
  #5843
nrow(unique(table_zlw))
  #6022
table_zlw %>% 
  select(-address) %>% 
  unique() %>% 
  nrow()
  #6022
table_zlw_nondup<-table_zlw %>% 
  distinct(ZPID,.keep_all=TRUE)
saveRDS(table_zlw_nondup,"Zillow data with distinct ZPID.Rdata")
