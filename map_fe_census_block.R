library(jsonlite)

coords<-as.data.frame(fe%>%
  select(lat, long)%>%
  mutate("FIPS_block" = NA,
         "FIPS_county" = NA,
         "FIPS_state" = NA,
         "STname" = NA,
         "API_status" = NA))

for(i in 1:nrow(coords)){
  url<-paste("https://geo.fcc.gov/api/census/block/find?latitude=",
             coords[i, 1],
             "&longitude=",
             coords[i, 2],
             "&showall=true&format=json",
             sep="")
  
  temp<-fromJSON(url)
  print(i)
  print(temp$status)
  
  coords[i, 3:7]<-c(temp$Block$FIPS,
                    temp$County$FIPS,
                    temp$State$FIPS,
                    temp$State$code,
                    temp$status)
  
}

write.csv(coords,
          "./data/block_map.csv", row.names = FALSE)