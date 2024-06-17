computeObito<-function(dadosO){
  dados<-dadosO
  dados$obcat<-"não"
  for(i in 1:nrow(dadosO)){
    if(dados$obito[i]>0){
      if(dados$O.F.O.[i]>0 | dados$t.s.ofo[i]>0){
        temp<-dados[i,]
        dados$obito[i]<-0
        dados<-rbind(dados,temp)
        dados$O.F.O.[nrow(dados)]<-0
        dados$t.s.ofo[nrow(dados)]<-0
        dados$obcat[nrow(dados)]<-"sim"
      }else{
        dados$obcat[i]<-"sim"
      }
    }
  }
  return(dados)
}
