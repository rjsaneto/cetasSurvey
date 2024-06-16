#'Função para transformar tempo em variável numérica

#'@export
crieTempo<-function(data,split=NULL,m=2,a=1){
  if(is.null(split)){
    split="-"
  }
  tempo<-strsplit(data,split = split)
  results<-NULL
  for(i in 1:length(tempo)){
    results<-rbind(results,c(mes=as.numeric(tempo[[i]][m]),ano=as.numeric(tempo[[i]][a])))
  }
  return(data.frame(results))
}
