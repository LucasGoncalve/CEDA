###################################################################################
### Curso de R
## Script
## Abril/2021
###############################################################################
# https://data.ceda.ac.uk/neodc/gosat/data/ch4/nceov1.0/CH4_GOS_OCPR/2009
# Dados de Metano 

#install.packages(ncdf4)
#install.packages(reshape2)
#install.packages(tidyverse)
library(ncdf4)
library(reshape2)
library(tidyverse)

#onde esta, diret�rio. como ver
#getwd()

setwd("C:/ProjetosR/CEDA/script") # Especifica o diretório e direciona 
flist <- list.files(path = "../dados1/") # Caminho de busca da pasta com arquivos .nc (.h5, etc)

# pode usar ao inv�s do 1:length
#seq_along()

for(i in 1:length(flist)){ # Cria um laço ou seja vai percorrer a pasta e anexar todos os arquivos
  if(i == 1){
    nc = nc_open(paste0("../dados1/", flist[i])) # depende do tipo de arquivo muda a forma de abrir
    ch4 <- ncvar_get(nc, attributes(nc$var)$names[17]) # criar nova coluna com variavel metano
    date <- ncvar_get(nc, attributes(nc$var)$names[29]) # data da variável 
    Lat <- ncvar_get(nc, attributes(nc$var)$names[11]) # geolocalização latitude
    Long <- ncvar_get(nc, attributes(nc$var)$names[4]) # geolocalização longitude
    dados<-data.frame(Lat,Long,ch4, as.POSIXlt.POSIXct(date)) # criar uma dataframe 
  }else{    # as.POSIXlt.POSIXct super imporante para poder configurar a data.
    nc = nc_open(paste0("../dados1/", flist[i]))
    ch4 <- ncvar_get(nc, attributes(nc$var)$names[17])
    date <- ncvar_get(nc, attributes(nc$var)$names[29])
    Lat <- ncvar_get(nc, attributes(nc$var)$names[11]) 
    Long <- ncvar_get(nc, attributes(nc$var)$names[4])
    #da<-data.frame(Lat,Long,ch4,as.POSIXlt.POSIXct(date)) 
    da<-data.frame(Lat,Long,ch4,as.POSIXlt.POSIXct(date))
    dados<-rbind(dados,da) # Combina linhas e colunas do dataframe, vetor, matriz, etc
  }
}
#### Aqui vale destacar algumas observaçoes:
### 1. Processamento de dados (capacidade do seu PC) e memória do R
### 2. Sempre faça testes ou simulações com dados menores.
### 3. Normalmente esses sites tem decrição dos dados, leitura antes

head(dados) # Demonstra os dados na primeiras linhas 
tail(dados) # Demonstra os dados nas ultimas linhas

# write.csv2(dados, "teste.txt", append = FALSE)

ks.test(dados$ch4, "pnorm",mean(dados$ch4),sd(dados$ch4))
summary(dados)
agricolae::skewness(dados$ch4)# Coeficiente de assimetria (cauda esquerda ou direita)
agricolae::kurtosis(dados$ch4) # Coeficiente de curtose (achamento da curva)
#dados <- dados %>% 
  #filter (ch4> 1750)

#dados <- dados %>% 
  #filter (ch4<1849)
x11()
ggplot(dados, aes(x= as.factor(as.POSIXlt.POSIXct.date.), y=ch4))+ 
  geom_point()+
  ggtitle("XCH4 GOSAT Proxy (mundi)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Date")+
  ylab("XCH4 (ppb)")
x11()
ggplot(dados, aes(x= as.factor(as.POSIXlt.POSIXct.date.), y=ch4,  fill=as.POSIXlt.POSIXct.date.))+ 
  geom_point()+
  ggtitle("XCH4 GOSAT Proxy (mundi)")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Date")+
  ylab("XCH4 (ppb)")

x11()
hist(dados$ch4, main="xch4(ppb)",nclass=7,freq=F,xlab="Classes",   # slide 5
     ylab="Densidade",col="gray")
x11()
plot(ecdf(dados$ch4),main="Função Densidade Acumulada Empírica", # slide 7
     xlab="Silte(%)",ylab="Probabilidade",ylim=c(0,1))

x11()
qqnorm(dados$ch4,main="Gráfico Q-Q normal (xch4)",xlab="Quantis teóricos",
       ylab="Quantis Amostrais",pch="*",col="blue")
qqline(dados$ch4,col="red",lwd=2)

########################################################
