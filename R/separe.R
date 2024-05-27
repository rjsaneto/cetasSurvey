#' Função para separar os dados faltantes

#'@export
separe<-function(dataSurv){

  # criando um back up dos NA
  databkup<-dataSurv[is.na(dataSurv$Tempo),]

  #deixar apenas os dados completos
  completo<-dataSurv[!is.na(dataSurv$Tempo),]

  #definir os faltantes
  faltam<-setdiff(databkup$CodCETAS,completo$CodCETAS)

  #filtrar os faltantes
  falta<-NULL
  for(i in 1:length(faltam)){
    temp<-databkup[databkup$CodCETAS==faltam[i],]
    if(nrow(temp)==1){
      falta<-rbind(falta,temp)
    }else{
      temp[order(temp$arquivo,decreasing = T),]
      falta<-rbind(falta,temp[1,])
    }
  }

  ### finaliza com lista
  results<-list(completo=completo,faltantes=falta)
  return(results)
}
