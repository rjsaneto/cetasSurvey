#'Função para criar códigos da coluna arquivo

#'@export
crieCodigo<-function(char,patterns=c(".xls",".tsv")){
  temp<-char
  for(i in 1:length(patterns)){
    temp<-strsplit(temp,patterns[i])[[1]]
  }
  temp<-strsplit(temp,split = "")[[1]]
  first<-temp[1]
  numeros<-suppressWarnings(as.numeric(temp))
  numeros<-numeros[!is.na(numeros)]
  codigo<-paste(c(first,numeros),collapse="")
}
