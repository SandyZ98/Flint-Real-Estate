load("Zillow API loop data.RData")
library(ZillowR)
library(dplyr)
library(XML)

set_zillow_web_service_id('X1-ZWz1hetbmyoqa3_13bwv')

table_zlw<-tibble()
nrow(table_addr_clean)
EmptytoNA <- function(x){
  if(identical(x,character(0))||is.null(x)){
    x<-NA
  }
  return(x)
}
j <- 1
for(j in 1:2){
  table_addr_clean$FULLNAME[j]
  number <-table_addr_clean$FROMHN[j]
  table_zlw_j<-tibble()
  for(number in table_addr_clean$FROMHN[j]:table_addr_clean$TOHN[j]){
    address<-paste(number,table_addr_clean$FULLNAME[j])
    Result<-GetDeepSearchResults(address, citystatezip = 'Flint, MI',rentzestimate=TRUE)
    if (Result$message$code==0){
      basic_info<-Result[["response"]][["results"]][["result"]]
      zpid<-basic_info[["zpid"]] %>% xmlValue()
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
      
      Zestimate_data<-Result[["response"]][["results"]][["result"]][["zestimate"]] %>% EmptytoNA()
      zestimate<-Zestimate_data[["amount"]] %>% xmlValue() %>% EmptytoNA()
      zest_last_updated<-Zestimate_data[["last-updated"]] %>% xmlValue() %>% EmptytoNA()
      zest_valueChange<-Zestimate_data[["valueChange"]] %>% xmlValue() %>% EmptytoNA()
      zest_duration<-Zestimate_data[["valueChange"]] %>% xmlGetAttr("duration") %>% EmptytoNA()
      zest_upper_range<-Zestimate_data[["valuationRange"]][["high"]] %>% xmlValue() %>% EmptytoNA()
      zest_lower_range<-Zestimate_data[["valuationRange"]][["low"]] %>% xmlValue() %>% EmptytoNA()
      
      
      Rent_data<-Result[["response"]][["results"]][["result"]][["rentzestimate"]]
      rentzestimate<-Rent_data[["amount"]] %>% xmlValue() %>% EmptytoNA()
      rent_last_updated<-Rent_data[["last-updated"]] %>% xmlValue() %>% EmptytoNA()
      rent_valueChange<-Rent_data[["valueChange"]] %>% xmlValue() %>% EmptytoNA()
      rent_duration<-Rent_data[["valueChange"]] %>% xmlGetAttr("duration") %>% EmptytoNA()
      rent_upper_range<-Rent_data[["valuationRange"]][["high"]] %>% xmlValue() %>% EmptytoNA()
      rent_lower_range<-Rent_data[["valuationRange"]][["low"]] %>% xmlValue() %>% EmptytoNA()
      
      Address_data<-Result[["response"]][["results"]][["result"]][["address"]]
      Lat<-Address_data[["latitude"]] %>% xmlValue() %>% EmptytoNA()
      Long<-Address_data[["longitude"]] %>% xmlValue() %>% EmptytoNA()
      
      
      table_zlw_temp<-tibble(address,ZPID=as.numeric(zpid),type=as.character(type),taxAssessment=as.numeric(taxAssessment),
                             taxAssess_year=as.character(taxAssess_year),yearBuilt=as.numeric(yearBuilt),lotSizeSqFt=as.numeric(lotSizeSqFt),
                             finishedSqFt=as.numeric(finishedSqFt),bathrooms=as.numeric(bathrooms),bedrooms=as.numeric(bedrooms),
                             lastSoldPrice=as.numeric(lastSoldPrice),lastSoldDate=as.character(lastSoldDate),
                             zestimate=as.numeric(zestimate),zest_valueChange=as.numeric(zest_valueChange),zest_duration=as.numeric(zest_duration),
                             zest_upper_range=as.numeric(zest_upper_range),zest_lower_range=as.numeric(zest_lower_range),zest_last_updated=as.character(zest_last_updated),
                             rentzestimate=as.numeric(rentzestimate),rent_valueChange=as.numeric(rent_valueChange),
                             rent_duration=as.numeric(rent_duration),rent_upper_range=as.numeric(rent_upper_range),rent_lower_range=as.numeric(rent_lower_range),
                             rent_last_updated=as.character(rent_last_updated),
                             lat=as.character(Lat),long=as.character(Long),zip=as.character(table_addr_clean$ZIP[j]))
      table_zlw_j<-rbind(table_zlw_j,table_zlw_temp)
    }
}AQ
  table_zlw<-rbind(table_zlw,table_zlw_j)
}
save.image("Zillow data.Rdata")
