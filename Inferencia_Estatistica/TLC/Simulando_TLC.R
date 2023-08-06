####################################################################################################################################
##################################           SIMULANDO O TEOREMA DO LIMITE CENTRAL           ####################################### 
####################################################################################################################################


######         O teorema do limite central: as médias de amostras grandes e aleatórias são aproximadamente normais,           ######
######                                    INDEPENDENTEMENTE de suas distribuições                                             ######
  
#install.packages("ggplot2")
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

# X é variável aleatória da estatura dos brasileiros. Seja X com distribuição normal média 170 cm e variância de 25cm
Pop<- data.frame(rnorm(100000,170,5))

colnames(Pop)<-"Estatura"
mean(Pop$Estatura)

G1<-ggplot(data=Pop,aes(x=Estatura))+
  geom_histogram(fill='blue',color='black',bins=100)+
  ggtitle("Estatura dos Brasileiros")
G1

# Extração de 1000 amostras de tamanho 64 e cálculo da média de cada amostra
Media_Amostral=data.frame()

for(i in 1:1000){
  x=sample(Pop$Estatura,64)
  x_barra=mean(x)
  Media_Amostral=rbind(Media_Amostral,x_barra)
}

colnames(Media_Amostral)<- "Estatura"

#Média das médias amostrais
mean(Media_Amostral$Estatura)
sd(Media_Amostral$Estatura)

G2<-ggplot(data=Media_Amostral,aes(x=Estatura))+
  geom_histogram(fill='pink',color='black',bins=20)+
  ggtitle("Média Amostral da Estatura dos Brasileiros")
G2

# X é variável aleatória do tempo de atendimento em um call center. Seja X com exponencial com média 240 segundos.
Pop<-data.frame(rexp(100000,1/240))

colnames(Pop)<-"Estatura"
mean(Pop$Estatura)

G3<-ggplot(data=Pop,aes(x=Estatura))+
  geom_histogram(fill='blue',color='black',bins=100)+
  ggtitle("Tempo De Atendimento")
G3

# Extração de 1000 amostras de tamanho 64 e cálculo da média de cada amostra
Media_Amostral=data.frame()

for(i in 1:1000){
  x=sample(Pop$Estatura,64)
  x_barra=mean(x)
  Media_Amostral=rbind(Media_Amostral,x_barra)
}

colnames(Media_Amostral)<- "Estatura"

#Média das médias amostrais
mean(Media_Amostral$Estatura)
sd(Media_Amostral$Estatura)

G4<-ggplot(data=Media_Amostral,aes(x=Estatura))+
  geom_histogram(fill='pink',color='black',bins=20)+
  ggtitle("Média Amostral da Tempo de Atendimento")
G4


grid.arrange(G1,G2,G3,G4, ncol=2, nrow = 2)
dev.copy(png, file = "grafico_TLC.png", width = 800, height = 600)
dev.off()