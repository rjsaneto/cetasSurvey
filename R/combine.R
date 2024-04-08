#' Função para puxar e agregar todos os arquivos de uma pasta

#'@export
combine<-function(path,pattern="txt",...){
  arq<-dir(path = path,pattern = pattern)
  results<-NULL
  for(i in 1:length(arq)){
    temp<-read.table(paste(path,"/",arq[i],sep = ""),...)
    #replace capitalization
    #arq<-gsub(pattern = "(\\w)(\\w*)", replacement = "\\L\\1\\L\\2", x = arq, perl=TRUE)
    temp$arquivo<-crieCodigo(tolower(arq[i]))
    results<-rbind(results,temp)
  }
  return(results)
}
