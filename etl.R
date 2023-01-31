db=data.frame()
for(i in c(2012:2020)){
  temp <- readxl::read_excel(paste0("data/iras_",i,".xlsx"))
  db=dplyr::bind_rows(temp,db)
}

db$TIPO.INFEC=ifelse(db$TIPO.INFEC=="IPCSC","IPCSL",db$TIPO.INFEC)

vars=c("UF","ANO","MES","DATA","TIPO.INFEC","Unidade Hospitalar","CNES")

write.csv(db[,vars],"database.csv",row.names = F)
