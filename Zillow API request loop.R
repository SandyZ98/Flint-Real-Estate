j <- 1
table_zlw_j<-data.frame()
nrow(table_addr_clean)
for(j in 1:5){
  table_addr_clean$FULLNAME[j]
  number <-table_addr_clean$FROMHN[j]
  table_zlw_j<-data.frame()
  for(number in table_addr_clean$FROMHN[j]:table_addr_clean$TOHN[j]){
    address<-paste(number,table_addr_clean$FULLNAME[j])
    Result<-GetSearchResults(address, citystatezip = 'Flint, MI')
    if (Result$message$code==0){
      zpid<-xmlToDataFrame(nodes = getNodeSet(Result$response, "//zpid"), stringsAsFactors = FALSE)
      Zestimate<-xmlToDataFrame(nodes = getNodeSet(Result$response, "//zestimate//amount"), stringsAsFactors = FALSE)
      Lat<-xmlToDataFrame(nodes = getNodeSet(Result$response, "//latitude"), stringsAsFactors = FALSE)
      Long<-xmlToDataFrame(nodes = getNodeSet(Result$response, "//longitude"), stringsAsFactors = FALSE)
      upper_range<-xmlToDataFrame(nodes = getNodeSet(Result$response, "//high"), stringsAsFactors = FALSE)
      lower_range<-xmlToDataFrame(nodes = getNodeSet(Result$response, "//low"), stringsAsFactors = FALSE)
      xpathSApply(Result$response, '//valueChange',xmlGetAttr,name = 'duration')
      table_zlw_temp<-data.frame(address,zpid,Zestimate,Lat,Long,upper_range,lower_range,table_addr_clean$ZIP[j])
      table_zlw_j<-rbind(table_zlw_j,table_zlw_temp)
    }
  }
  table_zlw<-rbind(table_zlw,table_zlw_j)
}

class(Result$response)
