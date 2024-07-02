TabelaSintese<-function(Dados,Quantidade,ordem,funcao=sum,ordene=T,arquivo=NULL){
  if(is.character(Quantidade)){
    Quantidade<-which(names(Dados)==Quantidade)
  }
  for(i in 1:length(ordem)){
    if(is.character(ordem[i])){
      ordem[i]<-which(names(Dados)==ordem[i])
    }
  }
  ordem<-as.numeric(ordem)
  tabelona<-tapply(X = Dados[,Quantidade],INDEX = unclass(Dados[,ordem]),FUN = sum)
  tabelona[is.na(tabelona)]<-0
  maior<-unique(Dados[,ordem[length(ordem)]])
  if(is.null(arquivo)){
    abre<-""
  }else{
    abre<-file(arquivo,"w")
  }
  for(i in 1:length(maior)){
    cat(paste("Classe ",maior[i],"\n",sep=""),file = abre)
    temp<-Dados[Dados[,ordem[length(ordem)]]==maior[i],]
    meio<-unique(temp[,ordem[length(ordem)-1]])
    for(j in 1:length(meio)){
      cat(paste("Ordem ",meio[j],"\n",sep=""),file = abre)
      temp<-tabelona[,meio[j],maior[i]]
      temp<-temp[temp>0]
      if(ordene){
        temp<-sort(temp,decreasing=T)
      }
      #names(temp)<-"Total"
      cat(paste("Espécie\tTotal\n",sep=""),file = abre)
      for(k in 1:length(temp)){
      nome<-names(temp[k])
      cat(paste(nome,"\t",sep=""),file = abre)
      cat(paste(temp[k],"\n",sep=""),file=abre)
      }
    }
  }
  if(!is.null(arquivo)){
    close(abre)
  }
}
