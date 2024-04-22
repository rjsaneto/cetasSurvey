maisclasses<-function(colunas,param2){
  receptora<-colunas[,1]
  doadora<-colunas[,2]
  for(i in 1:length(param2)){
    receptora[doadora==param2[i]]<-param2[i]
  }
  return(receptora)
}

menosclasses<-function(coluna,param1,param2){
  receptora<-coluna
  for(i in 1:length(param1)){
    receptora[receptora==param1[i]]<-param2
  }
  return(receptora)
}
