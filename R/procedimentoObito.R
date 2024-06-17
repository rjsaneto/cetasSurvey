computeSaidas<-function(dadosO){
  dados<-NULL
  nomes<-c("transf","soltura","obito","fuga","outro")
  for(i in 1:nrow(dadosO)){
    teste<-dadosO[i,nomes]
    temp<-dadosO[i,]
    if(length(teste[teste>0])>1){
      quais<-which(teste>0)
      temp[nomes[quais]]<-0
      for(j in 1:length(quais)){
        dados<-rbind(dados,temp)
        dados[nrow(dados),nomes[quais[j]]]<-teste[quais[j]]
      }
    }else{
      dados<-rbind(dados,temp)
    }
  }
  dados$obcat<-"não"
  dados$obcat[dados$obito>0]<-"sim"
  dados$t.s<-dados$transf+dados$soltura
  dados$O.F.O.<-dados$obito+dados$fuga+dados$outro
  dados$t.s.ofo<-dados$t.s+dados$O.F.O.
  dados$t.s.ob<-dados$obito+dados$t.s
  return(dados)
}

