library(tidyverse)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("geobr")
library(geobr)
#install.packages("sp")
library(sp)
#install.packages("trend")
library(trend)
#install.packages("lubridate")
library(lubridate)
#install.packages("gridExtra")
library(gridExtra)

setwd("C:/ProjetosR/CEDA/dados") # Especifica o diret�rio e direciona 
dados <- read.csv("dados_MS.csv")
head(dados)
tail(dados)

#shapiro.test(dados$ch4)  ### Error in shapiro.test(dados$ch4) : sample size must be between 3 and 5000
# TESTE DE NORMALIDADE de Kolmogorov-Smirnov Test
# Esses testes v�o pegar o banco de dados e ver se tem uma ribui��o Normaldist
ks.test(dados$ch4, "pnorm",mean(dados$ch4),sd(dados$ch4))
agricolae::skewness(dados$ch4) ## valor de assimetria
agricolae::kurtosis(dados$ch4)  ### valor de curtose

# Histograma da Variável metano
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

dados$Date <- as.Date(dados$Date, "%m/%d/%Y")

#teste média por mes

dados1 <- dados %>% 
  group_by(month=floor_date(Date, "month")) %>%   
  summarise(ch4 = mean(ch4))
dados1
dados2 <- dados %>% 
  group_by(year) %>%   
  summarise(ch4 = mean(ch4))
dados2

# rodar a partir do ggplot
g1 <- ggplot(dados1, aes(month, ch4))+ 
  geom_line(lwd=1, color="darkgreen")+
  xlab("")+
  ylab("xch4 (ppb")+
  theme_classic()+
  geom_smooth(color="red",linetype = "dotdash")
# rodar a partir do ggplot
g2 <- ggplot(dados, aes(x=as.factor(year), y=ch4))+ 
  geom_boxplot(color="blue",
               fill="blue",
               alpha=0.2,
               notch=TRUE,
               notchwidth = 0.8,
               outlier.colour="red",
               outlier.fill="red",
               outlier.size=3
  )+
  xlab("Years")+
  ylab("xch4 (ppb")
x11()
grid.arrange(g2,g1,nrow = 2)
# Teste de Mann Kendall na Serie temporal
mk.test(dados1$ch4)
# Teste de Pettit da Série Temporal
pettitt.test (dados1$ch4)
#########################################################
# Teste MAnn KenKall e Pettit média anual
mk.test(dados2$ch4)
# Teste de Pettit da Série Temporal
pettitt.test (dados2$ch4)

g1 <- ggplot(dados2) + 
  geom_point(aes(year, ch4))+
  xlab(" ")+
  ylab("xCH4 GOSAT Proxy (ppb)")


g2 <- ggplot(dados, aes(x=as.factor(year), y=ch4))+ 
  geom_boxplot(color="blue",
               fill="blue",
               alpha=0.2,
               notch=TRUE,
               notchwidth = 0.8,
               outlier.colour="red",
               outlier.fill="red",
               outlier.size=3
  )+
  theme_classic()+
  xlab("Years")+
  ylab("xCH4 GOSAT Proxy (ppb)")
  
g3 <- ggplot(dados2, aes(year, ch4))+ 
  geom_line(lwd=1, color="darkgreen")+
  xlab("Years")+
  ylab("xCH4 GOSAT Proxy (ppb)")+
  theme_classic()+
  geom_smooth(color="red",linetype = "dotdash")
grid.arrange(g2,g3,nrow = 2)
