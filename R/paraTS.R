paraTS<-function(vetorNum,lista,funcao,inicio=NULL,frequencia=NULL,nomes=NULL){
  dados<-tapply(X = vetorNum, INDEX = lista, FUN = funcao)
  dados[is.na(dados)]<-0

  dimensao<-dim(dados)
  if(is.null(frequencia)){
    frequencia<-dimensao[1]
  }
  baseTS<-ts(data=1:prod(dimensao[1:2]),start = inicio,frequency = frequencia)
  results<-baseTS

  sequencia<-seq(0,length(dados),length(baseTS))
    for(i in 2:length(sequencia)){
      temp<-dados[(sequencia[(i-1)]+1):sequencia[i]]
      tempTs<-baseTS
      tempTs[1:length(baseTS)]<-temp
      results<-cbind(results,tempTs)
    }

  if(!is.null(nomes)){
    attr(results,"dimnames")[[2]]<-c("base",nomes)
  }
  return(results)
}
