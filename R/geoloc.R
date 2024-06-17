#'Função para Inserir dados em Banco de Dados SIG
#'@export
geoloc<-function(dados,codigo_municipio = "BA",ano = 2022,coluna,retiraZeros=T){
  #Carrega ou instala o pacote geobr
  vamos<-try(library(geobr),silent = T)
  if(!is.null(attributes(vamos))){
    cat(paste("Instalando Pacote geobr\n"))
    install.packages(geobr)
    library(geobr)
  }
  #Tratamento dos dados para remoção dos pontos
  while(any(grepl(pattern = ".",x = dados$Destino,fixed = T),na.rm = T)){
    dados$Destino<-sub(pattern=".",replacement = " ",fixed=T,x = dados$Destino)
  }

  #Carrega ou instala o pacote gtools
  vamos<-try(library(gtools),silent = T)
  if(!is.null(attributes(vamos))){
    cat(paste("Instalando Pacote gtools\n"))
    install.packages("gtools")
    library(gtools)
  }

  #Tratamento dos dados para remoção dos pontos
  while(any(grepl(pattern = ".",x = dados$Destino,fixed = T),na.rm = T)){
    dados$Destino<-sub(pattern=".",replacement = " ",fixed=T,x = dados$Destino)
  }

  #Carregamento do mata no geobr e comparação dos nomes dos municípios
  mapa<-NULL
  for(i in 1:length(codigo_municipio)){
    mapa<-rbind(mapa,read_municipality(code_muni = codigo_municipio[i],year = ano))

  }
  nomes<-sort(unique(dados$Destino))
  nomesFora<-setdiff(nomes,mapa$name_muni) #nomes não encontrados
  emComum<-intersect(nomes,mapa$name_muni) #nomes em comum que serao usados

  # Inserir os dados no mapa
  final<-ncol(mapa)
  if(is.character(coluna)){
    teste<-intersect(coluna,names(dados))
    if(!all(teste==coluna)){
      stop("O Parâmetro coluna precisa ter os nomes das colunas do Data.Frame")
    }
    num<-NULL
    for(i in 1:length(coluna)){
      num<-c(num,which(names(dados)==coluna[i]))
    }
  }else{
    if(is.integer(coluna) & all(coluna>0)){
      if(max(coluna)>ncol(dados)){
        stop("O Parâmetro coluna precisa ser menor ou igual o número de colunas do Data.Frame")
      }
      num<-coluna
    }else{
      stop("Parâmetro coluna precisa ser número inteiro maior que zero ou texto")
    }
  }

  #criar lista com categorias unicas
  unicos<-list()
  maior<-0
  for(i in 1:length(num)){
    unicos[[i]]<-names(table(dados[,num[i]]))
    if(length(unicos[[i]])>maior){maior<-length(unicos[[i]])}
  }

  #criar matriz de combinacoes possiveis
  matrizComb<-permutations(n = maior,r = length(unicos),repeats.allowed = T)
  if(length(unicos)>1){
    for(i in 1:length(unicos)){
      matrizComb<-matrizComb[matrizComb[,i]<=length(unicos[[i]]),]
    }
  }

  #filtrar todas as possibilidades e computar tudo depois
  for(i in 1:nrow(matrizComb)){
    temp<-dados
    novo<-"var"
    for(j in 1:ncol(matrizComb)){
      temp<-temp[temp[,num[j]]==unicos[[j]][matrizComb[i,j]],]
      novo<-paste(novo,".",unicos[[j]][matrizComb[i,j]],sep="")
    }
    if(!all(is.na(temp))){
      mapa[,novo]<-0
      for(k in 1:length(emComum)){
        quantos<-sum(temp$TOTAL[which(temp$Destino==emComum[k])])
        mapa[(mapa$name_muni==emComum[k])&(mapa$abbrev_state==unique(temp$UF[temp$Destino==emComum[k]&!is.na(temp$Destino)])),novo]<-as.numeric(mapa[(mapa$name_muni==emComum[k])&(mapa$abbrev_state==unique(temp$UF[temp$Destino==emComum[k]&!is.na(temp$Destino)])),novo])[1]+quantos
      }
    }

  }

  #colocar NA no lugar dos zeros
  if(retiraZeros){
    for(i in (final+1):ncol(mapa)){
      for(j in 1:nrow(mapa)){
        if(as.data.frame(mapa)[j,i]==0){mapa[j,i]<-NA}
      }
    }
  }

  #o retorno nao sera o mapa, mas uma lista incluindo o mapa
  attr(mapa,"MunicipiosFora")<-nomesFora
  return(mapa)
}

#'Função para Inserir dados em Banco de Dados SIG
#'@export
salvaMapa<-function(mapa,file,...){
  sf::write_sf(mapa,file,...)
}
