                    ###################################
                    ####     MONOGRAFIA-LEILIANE   ####
                    ###################################
                                                                                     
####################      Funções e Pacotes Necessários     ###################

#Pacotes necessários#  
install.packages("TSA")
install.packages("nortest")
install.packages("car")
library(TSA)                          
library(nortest)                    
library(car)  

######################## Funçoes para Cálculo dos erros #######################
erros<- function (serie,intervalo=c(1,length(serie)),previsao) {
	a=intervalo[1]
	b=intervalo[2]
	p=previsao[a:b]
	n=length(p)	
	s=serie[a:b]
 	mape<-(1/n)*sum(100*(abs((s-p)/s)))
  mse<-(1/n)* sum((s-p)^2)
  mae<-(1/n)*sum(abs(s-p))
  mpe<-(1/n)*sum(100*((s-p)/s))
  erro<-matrix(c(mape,mse,mae,mpe),1,4)
  colnames(erro)<-c("MAPE","MSE","MAE","MPE")
  return(erro)}

####################### Funções para geração de gráficos #######################
graf=function(serie,prev,nome){
  par(mfrow=c(1,1))
  plot(serie,lwd=2,type='l',ylim=c(min(serie,prev[-c(1,2)]),
                                 max(serie,prev[-c(1,2)])
), main=nome, xlab="Tempo") 
lines(prev,col="red",lwd=2) 
legend("topright",c("Série","Previsão")
,col=c("black","red"),lty=1)} 

graf2=function(modelo){   
  par(mfrow=c(2,1))
  plot(modelo[,2],type="l",lty=1,lwd=2,ylim=c(min(modelo[,2],
                                                  modelo[,3],modelo[,4]),
                                              max(modelo[,2],modelo[,3],
                                                  modelo[,4])),
       xlim=c(-1,length(modelo[,1])))
  lines(modelo[,3],lty=2,lwd=2)
  lines(modelo[,4],lty=3,lwd=2)
  legend("bottomleft",c("P1","P2","P3")
         ,lty=c(1,2,3),cex=.7)
  plot(modelo[,1],type="l",lwd=2,ylim=c(min(modelo[,1]-3),max(modelo[,1])))
  legend("bottomleft",c("Constante"),lty=1,cex=.7) }

graf3=function(pesos){   
  plot(pesos[1,],type="l",lty=1,lwd=2,ylim=c(min(pesos[1,],
                                                 pesos[2,],pesos[3,]),
                                             max(pesos[1,],pesos[2,],
                                                 pesos[3,])),
       xlim=c(-1,length(pesos[1,])))
  lines(pesos[2,],lty=2,lwd=2)
  lines(pesos[3,],lty=3,lwd=2)
  legend("bottomleft",c("P1","P2","P3")
         ,lty=c(1,2,3),cex=.7) }

######################## Função para Previsão de Holt ########################
holt.function<- function (serie,intervalo,nome){  
#nome=nome da Série tem que ser entre ""
  a<- b<- holt<- c()
  menor1=10000000000000000000000000000
  a[1]<-mean(serie[1:4])
  b[1]<-(serie[4]-serie[1])/3
  for(alpha in seq(0.05,0.95,by=0.05)){
    for(beta1 in seq(0.05,0.95,by=0.05)){
      for (i in 2: (length(serie)-1)) {                 
        a[i]<- alpha*serie[i]+ (1-alpha)*(a[i-1]+b[i-1])
        b[i]<- beta1*(a[i]-a[i-1])+(1-beta1)*b[i-1]
        holt[i+1]<- a[i]+b[i]}
      erro<-erros(serie,intervalo,holt)
      if (menor1>erro[1,2]) {
        menor1<-erro[1,2]
        param=matrix(c(alpha,beta1),1,2)
        prev<-holt}   }   }
  colnames(param)=c("Alpha","Beta")     
  print(erros(serie,intervalo,prev))
  print(param)
  nome=c("Previsão por Holt da Série",nome)
  plot(serie,lwd=2,type='l',ylim=c(min(serie,holt[-c(1,2)]),
    max(serie,holt[-c(1,2)])), main=nome, xlab="Tempo")
  lines(prev,lwd=2,col='red')
   legend("topright",c("Série","Previsão"),col=c("black","red"),lty=1)
  return (prev)}

#################### Função para a escolha do modelo ARIMA ####################
ARIMA.function=function(serie,a) {  #a=lim inf para análise de erros
  b=length(serie)
  TAB=c()
  for (i in 0:1){
    for(j in 0:2){
      for(k in 0:2){
        ARIMA<-arimax(serie,order=c(j,i,k))                 
          prev_ARIMA=fitted(ARIMA) 
          Erro_ARIMA=erros(serie,intervalo=c(a,b),prev_ARIMA)  
          aux=as.vector(ARIMA$residuals[a:b])              
          D1=durbinWatsonTest(aux)  #correlação
          T1=t.test(ARIMA$residuals[a:b])$p.value   ##média nula
          LT1=lillie.test(ARIMA$residuals[a:b])$p.value     
          A=c(Erro_ARIMA[1:2],ARIMA$aic,D1,T1,LT1)
          TAB=rbind(TAB,A)
          }}}
          rownames(TAB)=c("ARIMA000","ARIMA001","ARIMA002","ARIMA100",
          "ARIMA101","ARIMA102","ARIMA200","ARIMA201","ARIMA202","ARIMA010",
          "ARIMA011","ARIMA012","ARIMA110","ARIMA111","ARIMA112","ARIMA210",
          "ARIMA211","ARIMA212")
          colnames(TAB)=c("MAPE","MSE","AIC","DW","T","LT")
          return(TAB) }

#ARIMA2 deve ser usada em séries nãoo estacionárias
ARIMA2.function=function(serie,a) {
  b=length(serie)
  TAB=c()
  i=1
  for(j in 0:2){
    for(k in 0:2){
      ARIMA<-arimax(serie,order=c(j,i,k))                  
      prev_ARIMA=fitted(ARIMA) 
      Erro_ARIMA=erros(serie,intervalo=c(a,b),prev_ARIMA)  
      aux=as.vector(ARIMA$residuals[a:b])              
      D1=durbinWatsonTest(aux)  #correla??o
      T1=t.test(ARIMA$residuals[a:b])$p.value   ##m?dia nula
      LT1=lillie.test(ARIMA$residuals[a:b])$p.value     
      A=c(Erro_ARIMA[1:2],ARIMA$aic,D1,T1,LT1)
      TAB=rbind(TAB,A)
    }}
  rownames(TAB)=c("ARIMA010","ARIMA011","ARIMA012","ARIMA110","ARIMA111",
                  "ARIMA112","ARIMA210","ARIMA211","ARIMA212")
  colnames(TAB)=c("MAPE","MSE","AIC","DW","T","LT")
  return(TAB) }

######################### Funções do Modelo Estrutural #########################
# Nas funções abaixo temos as seguintes convenções:
#Thp = vetores de estado: vetores [mu b] 
# Rp =  matrizes de covariância 
#Thp(t): Theta(t|t-1)    (Theta 'previsto')
#Thc(t): Theta(t|t)      (Theta 'corrigido')
#A mesma convenção vale para o R: Rp, Rc  
#Thps,Thcs,Rps,Rcs,es: vetores contendo as séries de Thp,Thc,Rp,Rc,e

####### apriori -Calcula média e variância apriore
apriori=function(Thc,Rc,G,W) { 
  Thp=G%*%Thc
  Rp=G%*%Rc%*%t(G)+W
  mat=cbind(Thp,Rp)
  return(mat)}

####### aposteriori - Calcula média e variância aposteriori
aposteriori=function(Thp,Rp,FF,V,e){
  ss= Rp%*%t(FF)%*%solve(V+FF%*%Rp%*%t(FF))
  Thc= Thp + ss%*%e
  Rc = Rp  - ss%*%FF%*%Rp
  mat=cbind(Thc,Rc)
  return(mat)}

####### fk_forward ####### 
#% INPUT   
#% serie :          valores observados           (nx1)
#% F,G :        matrizes multiplicadoras     (1xd, dxd)  
#% V :          variância do erro            (1x1)
#% M :          variância do erro w          (dxd)
#% estado:      vetor de estado              (dx1)
#%
#% OUTPUT (dx8)                                         
#% Formato :  [y Thcs Rcs] onde as colunas são
#% coluna  1:   valores observados (x)             (dx1)
#% colunas 2-3: vetores de estado (Thcs) [mu b]    (dx2)
#% colunas 4-7: matrizes de covariância (Rcs)      (dx4)
#%              na forma [s11 s12 s21 s22]
#% coluna  8:   valores previstos (yp)             (dx1)
#%
#% Usa funções 'apriori', 'aposteriori'.
fk_forward=function(serie,FF,G,v,W,estado){ 
#% Loading e inicialization
n=dim(serie)[1]
d=dim(FF)[2] # número de colunas 
Thc=rbind(estado)
Rc=diag(1,2) 

xx=apriori(Thc,Rc,G,W)
Thp=cbind(xx[,1])
Rp=xx[,2:3]
yp=matrix(c(FF%*%Thp),1,1) 
e=serie[1,1]- FF%*%Thp
es=matrix(c(e),1,1)

xx=aposteriori(Thp,Rp,FF,v,e)
Thc=cbind(xx[,1])
Rc=xx[,2:3]
Thcs=cbind(t(Thc))
Rcs=matrix(c(Rc[1,],Rc[2,]),1,4)
#%-------------------------------- LOOP -------------------------------------- 
for(i in 2:n){
  xx=apriori(Thc,Rc,G,W)
  Thp=cbind(xx[,1])
  Rp=xx[,2:3]
  yp=cbind(yp,FF%*%Thp)        
  e=serie[i,1]-yp[1,i]  
  es=cbind(es,e)       
 
 xx=aposteriori(Thp,Rp,FF,v,e)
 Thc=cbind(xx[,1])
 Rc=xx[,2:3]
 Thcs=rbind(Thcs,t(Thc))
 aux=matrix(c(Rc[1,],Rc[2,]),1,4)
 Rcs=rbind(Rcs,aux)}      
 saida=cbind(serie,Thcs,Rcs,t(yp))
 
return(saida)}

####### Var_Mod_Est_Op ####### 
# Otimiza a variância do ME.
# os parâmetros a e b são os limites do trecho da série para  analisar os erros.
#z é a série a ser estudada, sigs é o vetor das variâncias iniciais do modelo.
#param é o vetor de variâncias do modelo.
Var_Mod_Est_Opt=function(z,a,b,sigs){
  Mod_Est=function(param){ #função onde param será otimizado 
    mu=z[1,1]                                      
    bet=0 
    sig_a=param[1]
    sig_b=param[2]
    sig_c=param[3]       
    #% definição das matrizes do modelo. 
    FF=matrix(c(1,0),1,2)
    G=matrix(c(1,0,1,1),2,2)                    
    v=sig_a^2    
    estado=matrix(c(mu,bet),2,1)
    W=matrix(c(sig_b^2,0,0,sig_c^2),2,2)
    #amortecimento usando as equaçõess de Infield
    v=1
    lambda=(sig_c/sig_a)^2
    Qr=matrix(0,2,2)
    Qr[2,2]=lambda 
    x=fk_forward(z,FF,G,v,Qr,estado)
    prevs=x[,8] 
    # c?lculo dos erros 
    Z=z[,1]
    erro=erros(Z,c(a,b),prevs) 
    return(erro[2])}
  op=optim(sigs,Mod_Est,method="BFGS")  #otimizando                      
  return(op)}  

#######  Mod_Est_opt ####### 
#Função para previsão onde param já está otimizado
#nome=nome da série tem que ser entre ""
 Mod_Est_opt=function(z,param,nome){ 
    mu=z[1,1]                                      
    bet=0 
    sig_a=param[1]
    sig_b=param[2]
    sig_c=param[3]       
    #% definição das matrizes do modelo. 
    FF=matrix(c(1,0),1,2)
    G=matrix(c(1,0,1,1),2,2)                    
    v=sig_a^2    
    estado=matrix(c(mu,bet),2,1)
    W=matrix(c(sig_b^2,0,0,sig_c^2),2,2)
    #amortecimento usando as equações de Infield
    v=1
    lambda=(sig_c/sig_a)^2
    Qr=matrix(0,2,2)
    Qr[2,2]=lambda
    x=fk_forward(z,FF,G,v,Qr,estado)
    prevs=x[,8]
    Z=z[,1]
    nome=c("Previsão por Modelo Estrutural de Tendência Local da Série",nome)
    plot(Z,lwd=2,type='l',ylim=c(min(Z,prevs[-c(1,2)]),max(Z,prevs
    [-c(1,2)])), main=nome, xlab="Tempo")
    lines(prevs,col="red",lwd=2)
    legend("topright",c("Série","Previsão"),col=c("black","red"),lty=1)
    return(prevs)}    

####### Matriz de peso para Optimal ####### 
#A variável Ind indaga se é para ser Optimal
#com independência, default é sem independência.
Peso.function=function(serie,serie_holt,serie_arima,serie_ME,Ind=FALSE){
  Erro_serie=cbind(serie-serie_holt,serie-serie_arima,serie-serie_ME)
  S=cov(Erro_serie[c(-1,-2),])
  E=cbind(rep(1,3))
  if (Ind==TRUE){
    v=diag(S)
    v=rbind(v,v,v)
    S=v*diag(3)} 
  w=(solve(S)%*%E)/as.numeric((t(E)%*%solve(S)%*%E))  #matrix de pesos.
  rownames(w)=list("Peso_holt","Peso_arima","Peso_ME")
  return(w)}             
 
################################################################################ 
############################# Carregando as séries #############################
X=read.table("cow.txt")
cow=X[,1]     
X=read.table("buffsnow.txt")
buffsnow=X[,1]
X=read.table("calfem.txt")
calfem=X[1:120,1]
X=read.table("DowJones.txt")
DowJones=X[,1]
X=read.table("globtp.txt")
globtp=X[,1]
X=read.table("HURON.txt")
HURON=X[,1]
X=read.table("SHEEP.txt")
SHEEP=X[,1]
X=read.table("summer.txt")
summer=X[1:104,1]
X=read.table("ausgundeaths.txt")
ausgundeaths=X[,4]
X=read.table("Carga.txt")
Carga=X[,1]
rm(X)
########################### Gráfico das Séries #################################
#par(ask=TRUE)
plot(cow,type="l",lwd=2,main="Série Cow",xlab="Tempo")
plot(buffsnow,type="l",lwd=2,main="Série Buffsnow",xlab="Tempo")
plot(calfem,type="l",lwd=2,main="Série CalFem",xlab="Tempo")
plot(DowJones,type="l",lwd=2,main="Série DowJones",xlab="Tempo")
plot(globtp,type="l",lwd=2,main="Série Globtp",xlab="Tempo")
plot(HURON,type="l",lwd=2,main="Série HURON",xlab="Tempo")
plot(SHEEP,type="l",lwd=2,main="Série SHEEP",xlab="Tempo")
plot(summer,type="l",lwd=2,main="Série Summer",xlab="Tempo")
plot(ausgundeaths,type="l",lwd=2,main="Série Ausgundeaths",xlab="Tempo")
plot(Carga,type="l",lwd=2,main="Série Carga",xlab="Tempo")

################################################################################
########################## Previsão individual #################################

##################################### Holt #####################################
cow_holt=holt.function(cow,c(16,60),"COW")
buffsnow_holt=holt.function(buffsnow,c(13,51),"Buffsnow")
calfem_holt=holt.function(calfem,c(25,96),"Calfem")
DowJones_holt=holt.function(DowJones,c(16,63),"DowJones")
globtp_holt=holt.function(globtp,c(22,85),"Globtp")
HURON_holt=holt.function(HURON,c(20,79),"HURON")
SHEEP_holt=holt.function(SHEEP,c(15,59),"SHEEP")
summer_holt=holt.function(summer,c(21,84),"Summer")
ausgundeaths_holt=holt.function(ausgundeaths,c(19,72),"Ausgundeaths")
Carga_holt=holt.function(Carga,c(19,73),"Carga")
                               
#################################### ARIMA #################################### 
#colocar na função arima: xreg = 1:length(serie) para aparecer o intercepto em
#arima com série diferenciada
#transformação do intercepto cte=intercepto*(1- os demais coeficientes de AR)

########  Cow ########
#Estacionariedade:           
mean(cow[1:15])
mean(cow[16:60])
mean(cow[61:75])   
acf(cow, type="correlation",lwd=2,col="blue",main="FAC da Série Cow")
pacf(cow,lwd=2,col="blue",main="FACP da Série Cow")          
#Escolha do modelo
serie=cow[1:60]
ARIMA.function(serie,16) 
#o melhor modelo é: 
prev_cow=arimax(cow[1:60],order=c(1,0,2))
tsdiag(prev_cow,60)                    
#Previsão:
int=prev_cow$coef[4]*(1-prev_cow$coef[1])
phi1=prev_cow$coef[1]
theta1=prev_cow$coef[2]
theta2=prev_cow$coef[3]
cow_arima=c()                 
cow_arima[1]=fitted(prev_cow)[1] #usar os valores previstos pelo R = 56.30899
cow_arima[2]=fitted(prev_cow)[2] #usar os valores previstos pelo R = 58.14536
for (i in 3:length(cow)){
  cow_arima[i]=int+phi1*cow[i-1]+theta1*(cow[i-1]-cow_arima[i-1])
  +theta2*(cow[i-2]-cow_arima[i-2])} 
graf(cow,cow_arima,"Previsão da Série Cow por ARIMA")      

######## buffsnow ########
#Estacionariedade:           
mean(buffsnow[1:12])
mean(buffsnow[13:51])
mean(buffsnow[52:63])  
acf(buffsnow, type="correlation",lwd=2,col="blue",main="FAC da Série Buffsnow")
pacf(buffsnow,lwd=2,col="blue",main="FACP da Série Buffsnow")                
#Escolha do modelo
serie=buffsnow[1:51]
ARIMA.function(serie,13) 
#o melhor modelo é: 
prev_buffsnow=arimax(buffsnow[1:51],order=c(1,0,1))                  
tsdiag(prev_buffsnow,51)  
#Previsão:
int=prev_buffsnow$coef[3]*(1-prev_buffsnow$coef[1])
phi1=prev_buffsnow$coef[1]
theta1=prev_buffsnow$coef[2]
buffsnow_arima=c()                
buffsnow_arima[1]= fitted(prev_buffsnow)[1] #usar valores previstos pelo R = 83.57727
for (i in 2:length(buffsnow)){
  buffsnow_arima[i]=int+phi1*buffsnow[i-1]+theta1*(buffsnow[i-1]
  -buffsnow_arima[i-1])} 
graf(buffsnow,buffsnow_arima,"Previsão da Série Buffsnow por ARIMA") 
                                                                          
######## Calfem ########
#Estacionariedade:           
mean(calfem[1:24])
mean(calfem[25:96])
mean(calfem[97:120])   #Parece  ser estacionário
acf(calfem, type="correlation",lwd=2,col="blue",main="FAC da Série CalFem")
pacf(calfem,lwd=2,col="blue",main="FACP da Série CalFem")         
#Escolha do modelo
serie=calfem[1:96]
ARIMA.function(serie,25) 
#o melhor modelo é: 
prev_calfem=arimax(calfem[1:96],order=c(0,0,0)) 
tsdiag(prev_calfem,96) 
#Previsão:
calfem_arima=c(rep(mean(calfem[1:96]),120))
graf(calfem,calfem_arima,"Previsão da Série Calfem por ARIMA")     
                                                                              
######## DowJones ########
#Estacionariedade:           
mean(DowJones[1:15])
mean(DowJones[16:63])
mean(DowJones[64:78])   #Parece  ser não-estacionário
acf(DowJones, type="correlation",lwd=2,col="blue",main="FAC da Série DowJones")
pacf(DowJones,lwd=2,col="blue",main="FACP da Série DowJones")    
#Diferenciando:
DowJones.dif=diff(DowJones)
mean(DowJones.dif[1:15])
mean(DowJones.dif[16:63])
mean(DowJones.dif[64:77])   #Parece  ser estacionário
plot(DowJones.dif,type='l',lwd=2,main="Série DowJones Diferenciada") 
acf(DowJones.dif,lwd=2,col="blue",main="FAC da Série DowJones Diferenciada")
pacf(DowJones.dif,lwd=2,col="blue",main="FACP da Série DowJones Diferenciada")
rm(DowJones.dif)      
##Escolha do modelo  
serie=DowJones[1:63]
ARIMA2.function(serie,16) #Feita uma pequena mudança nesta função pois é NE.
##o melhor modelo é: 
prev_DowJones=arimax(DowJones[1:63],order=c(1,1,2),xreg=1:63)       
tsdiag(prev_DowJones,63)                           
#Previsão:
int=prev_DowJones$coef[4]*(1-prev_DowJones$coef[1])
phi1=prev_DowJones$coef[1]
theta1=prev_DowJones$coef[2]
theta2=prev_DowJones$coef[3]
DowJones_arima=c()                
DowJones_arima[1]=fitted(prev_DowJones)[1] #=110.8292       
DowJones_arima[2]=fitted(prev_DowJones)[2] #=111.0676
for (i in 3:length(DowJones)){
  DowJones_arima[i]=int+(1+phi1)*DowJones[i-1]-phi1*DowJones[i-2]+theta1*
  (DowJones[i-1]-DowJones_arima[i-1])+theta2*(DowJones[i-2]-DowJones_arima[i-2])} 
graf(DowJones,DowJones_arima,"Previsão da Série DowJones por ARIMA")   
                                                                                   
######## globtp ########
#Estacionariedade:           
mean(globtp[1:21])
mean(globtp[22:85])
mean(globtp[86:106])   #Parece  ser não estacionário
acf(globtp, type="correlation",lwd=2,col="blue",main="FAC da Série Globtp")
pacf(globtp,lwd=2,col="blue",main="FACP da Série Globtp")         
#Diferenciando:
globtp.dif=diff(globtp)
mean(globtp.dif[1:21])
mean(globtp.dif[22:85])
mean(globtp.dif[86:105])
plot(globtp.dif,type='l',lwd=2,main="Série Globtp Diferenciada") 
acf(globtp.dif,col="blue",lwd=2,main="FAC da Série Globtp Diferenciada")
pacf(globtp.dif,col="blue",lwd=2,main="FACP da Série Globtp Diferenciada") 
rm(globtp.dif)
#Escolha do modelo
serie=globtp[1:85]
ARIMA.function(serie,22)       
#o melhor modelo é:
prev_globtp=arimax(globtp[1:85],order=c(1,1,1),xreg=1:85)
tsdiag(prev_globtp,85)
#Previsão:
int=prev_globtp$coef[3]*(1-prev_globtp$coef[1])
phi1=prev_globtp$coef[1]
theta1=prev_globtp$coef[2]
globtp_arima=c()                
globtp_arima[1]=fitted(prev_globtp)[1]  #= -0.39959438
globtp_arima[2]= fitted(prev_globtp)[2] #=-0.39153180    
for (i in 3:length(globtp)){
 globtp_arima[i]=int+phi1*globtp[i-1]+globtp[i-1]-phi1*globtp[i-2]+theta1*
  (globtp[i-1]-globtp_arima[i-1])}
graf(globtp,globtp_arima,"Previsão da Série Globtp por ARIMA")  

######## HURON ########
#Estacionariedade:           
mean(HURON[1:19])
mean(HURON[20:79])
mean(HURON[80:98])   ##Parece  ser estacionário
acf(HURON, type="correlation",lwd=2,col="blue",main="FAC da Série HURON")
pacf(HURON,lwd=2,col="blue",main="FACP da Série HURON")         
#Escolha do modelo
serie=HURON[1:79]
ARIMA.function(serie,20) 
#o melhor modelo é: 
prev_HURON=arimax(HURON[1:79],order=c(2,0,0))                    
tsdiag(prev_HURON,79) 
#Previsão:
int=prev_HURON$coef[3]*(1-prev_HURON$coef[1]-prev_HURON$coef[2])
phi1=prev_HURON$coef[1]
phi2=prev_HURON$coef[2]
HURON_arima=c()                
HURON_arima[1]=fitted(prev_HURON)[1] #=9.762774
HURON_arima[2]=fitted(prev_HURON)[2] #=10.247705      
for (i in 3:length(HURON)){                                                                                  
  HURON_arima[i]=int+phi1*HURON[i-1]+phi2*HURON[i-2]}
graf(HURON,HURON_arima,"Previsão da Série HURON por ARIMA")  

######## SHEEP ########
#Estacionariedade:           
mean(SHEEP[1:16])
mean(SHEEP[15:59])
mean(SHEEP[60:73])   #Parece  ser estacionário
acf(SHEEP, type="correlation",lwd=2,col="blue",main="FAC da Série SHEEP")
pacf(SHEEP,lwd=2,col="blue",main="FACP da Série SHEEP")    
#Diferenciando:
SHEEP.dif=diff(SHEEP)  
plot(SHEEP.dif,type='l',main="Série SHEEP Difereniada") 
acf(SHEEP.dif,lwd=2,col="blue",main="FAC da Série SHEEP Diferenciada")
pacf(SHEEP.dif,lwd=2,col="blue",main="FACP da Série SHEEP Diferenciada")   
rm(SHEEP.dif)   
#Escolha do modelo
serie=SHEEP[1:59]
ARIMA.function(serie,15)           
#o melhor modelo é: 
prev_SHEEP=arimax(SHEEP[1:59],order=c(2,1,1),xreg=1:59)  
tsdiag(prev_SHEEP,59)
#Previsão:
int=prev_SHEEP$coef[4]*(1-prev_SHEEP$coef[1]-prev_SHEEP$coef[2])
phi1=prev_SHEEP$coef[1]
phi2=prev_SHEEP$coef[2]
theta1=prev_SHEEP$coef[3]
SHEEP_arima=c()               
SHEEP_arima[1]=fitted(prev_SHEEP)[1]#=2200.786
SHEEP_arima[2]=fitted(prev_SHEEP)[2]#=2228.305
SHEEP_arima[3]=fitted(prev_SHEEP)[3]#=2395.930    
for (i in 4:length(SHEEP)){
  SHEEP_arima[i]=int+(phi1+1)*SHEEP[i-1]+(phi2-phi1)*(SHEEP[i-2])-phi2*
    SHEEP[i-3]+theta1*(SHEEP[i-1]-SHEEP_arima[i-1])} 
graf(SHEEP,SHEEP_arima,"Previsão da Série SHEEP por ARIMA")  

######## summer ########
#Estacionariedade:           
mean(summer[1:20])
mean(summer[21:84])
mean(summer[85:104])   #Parece  ser estacionário
acf(summer, type="correlation",lwd=2,col="blue",main="FAC da Série Summer")
pacf(summer,lwd=2,col="blue",main="FACP da Série Summer")         
#Escolha do modelo
serie=summer[1:84]
ARIMA.function(serie,21) 
#o melhor modelo é: 
prev_summer=arimax(summer[1:84],order=c(1,0,0)) 
tsdiag(prev_summer,84)
#Previsão:
int=prev_summer$coef[2]*(1-prev_summer$coef[1])
phi1=prev_summer$coef[1]
summer_arima=c()               
summer_arima[1]=fitted(prev_summer)[1]#=15.32855 
for (i in 2:length(summer)){
  summer_arima[i]=int+phi1*summer[i-1]}
graf(summer,summer_arima,"Previsão da Série Summer por ARIMA")   

######## ausgundeaths ########
#Estacionariedade:           
mean(ausgundeaths[1:18])
mean(ausgundeaths[19:72])
mean(ausgundeaths[73:90])   #Parece  ser estacionário
acf(ausgundeaths, type="correlation",lwd=2,col="blue",main="FAC da Série Ausgundeaths")
pacf(ausgundeaths,lwd=2,col="blue",main="FACP da Série Ausgundeaths")  
#Diferenciando:       
ausgundeaths.dif=diff(ausgundeaths)  
plot(ausgundeaths.dif,type='l',main="Série Ausgundeaths Diferenciada") 
acf(ausgundeaths.dif,lwd=2,col="blue",main="FAC da Série Ausgundeaths Diferenciada")
pacf(ausgundeaths.dif,lwd=2,col="blue",main="FACP da Série Ausgundeaths Diferenciada") 
rm(ausgundeaths.dif)  
#Escolha do modelo
serie=ausgundeaths[1:72]
ARIMA.function(serie,19) 
#o melhor modelo é: 
prev_ausgundeaths=arimax(ausgundeaths[1:72],order=c(1,1,1),xreg=1:72)
tsdiag(prev_ausgundeaths,72)  
#Previsão:
int=prev_ausgundeaths$coef[3]*(1-prev_ausgundeaths$coef[1])
phi1=prev_ausgundeaths$coef[1]
theta1=prev_ausgundeaths$coef[2]
ausgundeaths_arima=c() 
ausgundeaths_arima[1]=fitted(prev_ausgundeaths)[1]#=9.157297  
ausgundeaths_arima[2]=fitted(prev_ausgundeaths)[2]#=9.128523   
for (i in 3:length(ausgundeaths)){
 ausgundeaths_arima[i]=int+phi1*ausgundeaths[i-1]+ausgundeaths[i-1]-phi1*
  ausgundeaths[i-2]+theta1*(ausgundeaths[i-1]-ausgundeaths_arima[i-1])}
graf(ausgundeaths,ausgundeaths_arima,"Previsão da Série Ausgundeaths por ARIMA")  

######## Carga ########
#Estacionariedade:           
mean(Carga[1:18])
mean(Carga[19:73])
mean(Carga[73:91])   #Parece  ser estacionário
acf(Carga, type="correlation",lwd=2,col="blue",main="FAC da Série Carga")
pacf(Carga,lwd=2,col="blue",main="FACP da Série Carga") 
#Diferenciando:      
Carga.dif=diff(Carga)  
plot(Carga.dif,type='l',main="Série Carga Diferenciada") 
acf(Carga.dif,lwd=2,col="blue",main="FAC da Série Carga Diferenciada")
pacf(Carga.dif,lwd=2,col="blue",main="FACP da Série Carga Diferenciada") 
rm(Carga.dif)          
#Escolha do modelo
serie=Carga[1:73]
ARIMA.function(serie,19) 
#o melhor modelo é: 
prev_Carga=arimax(Carga[1:73],order=c(0,1,1),xreg=1:73) 
tsdiag(prev_Carga,73) 
#Previsão:
int=prev_Carga$coef[2]
theta1=prev_Carga$coef[1]
Carga_arima=c() 
Carga_arima[1]=fitted(prev_Carga)[1]#=56.68548       
for (i in 2:length(Carga)){
 Carga_arima[i]=int+theta1*(Carga[i-1]-Carga_arima[i-1])+Carga[i-1]} 
graf(Carga,Carga_arima,"Previsão da Série Carga por ARIMA")  

rm(theta1,theta2,phi1,phi2,int)

#############################  Modelos Estruturais #############################
      
########### Cow
z=read.table("cow.txt")
op=Var_Mod_Est_Opt(z,16,60,c(100,1,1))
param=op$par   
cow_ME=Mod_Est_opt(z,param,"Cow")    
erros(cow,c(16,60),cow_ME)     #61,75

########### buffsnow
z=read.table("buffsnow.txt")
op=Var_Mod_Est_Opt(z,13,51,c(100,1,1))
param=op$par   
buffsnow_ME=Mod_Est_opt(z,param,"Buffsnow")    
erros(buffsnow,c(13,51),buffsnow_ME)    #52,63

########### calfem
z=read.table("calfem.txt")
z=cbind(z[1:120,1])
op=Var_Mod_Est_Opt(z,25,96,c(100,1,1))
param=op$par   
calfem_ME=Mod_Est_opt(z,param,"Calfem")    
erros(calfem,c(25,96),calfem_ME)#97,120

########### DowJones
z=read.table("DowJones.txt")
op=Var_Mod_Est_Opt(z,16,63,c(10,10,100))
param=op$par   
DowJones_ME=Mod_Est_opt(z,param,"DowJones")    
erros(DowJones,c(16,63),DowJones_ME)     #64,78
           
########### globtp
z=read.table("globtp.txt")
op=Var_Mod_Est_Opt(z,22,85,c(200,10,10))
param=op$par   
globtp_ME=Mod_Est_opt(z,param,"Globtp")    
erros(globtp,c(22,85),globtp_ME) #86,106    

###########HURON
z=read.table("HURON.txt")
op=Var_Mod_Est_Opt(z,20,79,c(100,10,10))
param=op$par   
HURON_ME=Mod_Est_opt(z,param,"HURON")    
erros(HURON,c(20,79),HURON_ME)      #80,98

########### SHEEP
z=read.table("SHEEP.txt")
op=Var_Mod_Est_Opt(z,15,59,c(10,1,100))
param=op$par   
SHEEP_ME=Mod_Est_opt(z,param,"SHEEP")    
erros(SHEEP,c(15,59),SHEEP_ME)    #60,73

########### summer
z=read.table("summer.txt")
z=cbind(z[1:104,1])
op=Var_Mod_Est_Opt(z,21,84,c(100,1,1))
param=op$par   
summer_ME=Mod_Est_opt(z,param,"Summer")    
erros(summer,c(21,84),summer_ME) #85,104
  
########### ausgundeaths
z=read.table("ausgundeaths.txt")
z=cbind(z[,4])
op=Var_Mod_Est_Opt(z,19,72,c(100,1,10))
param=op$par   
ausgundeaths_ME=Mod_Est_opt(z,param,"Ausgundeaths")    
erros(ausgundeaths,c(19,72),ausgundeaths_ME)   #73,90

########### Carga
z=read.table("Carga.txt")
op=Var_Mod_Est_Opt(z,19,73,c(100,15,10))
param=op$par   
Carga_ME=Mod_Est_opt(z,param,"Carga")    
erros(Carga,c(19,73),Carga_ME)        

rm(op,param)

################################################################################
############################ Combinação de Previsão ############################

################################ Média Simples #################################
cow_MS=(cow_holt+cow_arima+cow_ME)/3
buffsnow_MS=(buffsnow_holt+buffsnow_arima+buffsnow_ME)/3
calfem_MS=(calfem_holt+calfem_arima+calfem_ME)/3
DowJones_MS=(DowJones_holt+DowJones_arima+DowJones_ME)/3
globtp_MS=(globtp_holt+globtp_arima+globtp_ME)/3
HURON_MS=(HURON_holt+HURON_arima+HURON_ME)/3
SHEEP_MS=(SHEEP_holt+SHEEP_arima+SHEEP_ME)/3
summer_MS=(summer_holt+summer_arima+summer_ME)/3
ausgundeaths_MS=(ausgundeaths_holt+ausgundeaths_arima+ausgundeaths_ME)/3
Carga_MS=(Carga_holt+Carga_arima+Carga_ME)/3

#Gráficos
#par(ask=TRUE)
graf(cow,cow_MS,"Previsão da Série Cow por Combinação: MÉDIA Simples")
graf(buffsnow,buffsnow_MS,"Previsão da Série Buffsnow por Combinação: MÉDIA Simples")
graf(calfem,calfem_MS,"Previsão da Série Calfem por Combinação: MÉDIA Simples")
graf(DowJones,DowJones_MS,"Previsão da Série DowJones por Combinação: MÉDIA Simples")
graf(globtp,globtp_MS,"Previsão da Série Globtp por Combinação: MÉDIA Simples")
graf(HURON,HURON_MS,"Previsão da Série HURON por Combinação: MÉDIA Simples")
graf(SHEEP,SHEEP_MS,"Previsão da Série SHEEP por Combinação: MÉDIA Simples")
graf(summer,summer_MS,"Previsão da Série Summer por Combinação: MÉDIA Simples")
graf(ausgundeaths,ausgundeaths_MS,"Previsão da Série Ausgundeaths por Combinação: MÉDIA Simples")
graf(Carga,Carga_MS,"Previsão da Série Carga por Combinação: MÉDIA Simples")      

#################################### Mediana ###################################
cow_Med=c()
  for(i in 1:75){
  cow_Med[i]=median(cbind(cow_holt,cow_arima,cow_ME)[i,])}
buffsnow_Med=c()
  for(i in 1:63){
  buffsnow_Med[i]=median(cbind(buffsnow_holt,buffsnow_arima,buffsnow_ME)[i,])}
calfem_Med=c()
  for(i in 1:120){
  calfem_Med[i]=median(cbind(calfem_holt,calfem_arima,calfem_ME)[i,])}
DowJones_Med=c()
  for(i in 1:78){
  DowJones_Med[i]=median(cbind(DowJones_holt,DowJones_arima,DowJones_ME)[i,])}
globtp_Med=c()
  for(i in 1:106){
  globtp_Med[i]=median(cbind(globtp_holt,globtp_arima,globtp_ME)[i,])}
HURON_Med=c()
  for(i in 1:98){
  HURON_Med[i]=median(cbind(HURON_holt,HURON_arima,HURON_ME)[i,])}
SHEEP_Med=c()
  for(i in 1:73){
  SHEEP_Med[i]=median(cbind(SHEEP_holt,SHEEP_arima,SHEEP_ME)[i,])}
summer_Med=c()
  for(i in 1:104){
  summer_Med[i]=median(cbind(summer_holt,summer_arima,summer_ME)[i,])}
ausgundeaths_Med=c()
  for(i in 1:90){
  ausgundeaths_Med[i]=median(cbind(ausgundeaths_holt,ausgundeaths_arima,ausgundeaths_ME)[i,])}
Carga_Med=c()
  for(i in 1:91){
  Carga_Med[i]=median(cbind(Carga_holt,Carga_arima,Carga_ME)[i,])}

#Gráficos
#par(ask=TRUE)
graf(cow,cow_Med,"Previsão da Série Cow por Combinação: Mediana")
graf(buffsnow,buffsnow_Med,"Previsão da Série Buffsnow por Combinação: Mediana")
graf(calfem,calfem_Med,"Previsão da Série Calfem por Combinação: Mediana")
graf(DowJones,DowJones_Med,"Previsão da Série DowJones por Combinação: Mediana")
graf(globtp,globtp_Med,"Previsão da Série Globtp por Combinação: Mediana")
graf(HURON,HURON_Med,"Previsão da Série HURON por Combinação: Mediana")
graf(SHEEP,SHEEP_Med,"Previsão da Série SHEEP por Combinação: Mediana")
graf(summer,summer_Med,"Previsão da Série Summer por Combinação: Mediana")
graf(ausgundeaths,ausgundeaths_Med,"Previsão da Série Ausgundeaths por Combinação: Mediana")
graf(Carga,Carga_Med,"Previsão da Série Carga por Combinação: Mediana")

############################## Média dos Extremos ##############################
cow_MEx=c()
  for(i in 1:75){
  cow_MEx[i]=mean(c(max(cbind(cow_holt,cow_arima,cow_ME)[i,]),min(cbind(cow_holt,
  cow_arima,cow_ME)[i,]))) }                                        
buffsnow_MEx=c()
  for(i in 1:63){
  buffsnow_MEx[i]=mean(c(max(cbind(buffsnow_holt,buffsnow_arima,buffsnow_ME)[i,]),
  min(cbind(buffsnow_holt,buffsnow_arima,buffsnow_ME)[i,]))) }
calfem_MEx=c()
  for(i in 1:120){
  calfem_MEx[i]=mean(c(max(cbind(calfem_holt,calfem_arima,calfem_ME)[i,]),min
  (cbind(calfem_holt,calfem_arima,calfem_ME)[i,]))) }
DowJones_MEx=c()
  for(i in 1:78){
  DowJones_MEx[i]=mean(c(max(cbind(DowJones_holt,DowJones_arima,DowJones_ME)[i,]),
  min(cbind(DowJones_holt,DowJones_arima,DowJones_ME)[i,]))) }
globtp_MEx=c()
  for(i in 1:106){
  globtp_MEx[i]=mean(c(max(cbind(globtp_holt,globtp_arima,globtp_ME)[i,]),min
  (cbind(globtp_holt,globtp_arima,globtp_ME)[i,]))) }
HURON_MEx=c()
  for(i in 1:98){
  HURON_MEx[i]=mean(c(max(cbind(HURON_holt,HURON_arima,HURON_ME)[i,]),min(cbind
  (HURON_holt,HURON_arima,HURON_ME)[i,]))) }
SHEEP_MEx=c()
  for(i in 1:73){
  SHEEP_MEx[i]=mean(c(max(cbind(SHEEP_holt,SHEEP_arima,SHEEP_ME)[i,]),min(cbind
  (SHEEP_holt,SHEEP_arima,SHEEP_ME)[i,]))) }
summer_MEx=c()
  for(i in 1:104){
  summer_MEx[i]=mean(c(max(cbind(summer_holt,summer_arima,summer_ME)[i,]),min
  (cbind(summer_holt,summer_arima,summer_ME)[i,]))) }
ausgundeaths_MEx=c()
  for(i in 1:90){
  ausgundeaths_MEx[i]=mean(c(max(cbind(ausgundeaths_holt,ausgundeaths_arima,
  ausgundeaths_ME)[i,]),min(cbind(ausgundeaths,ausgundeaths_arima,ausgundeaths_ME)[i,]))) }
Carga_MEx=c()
  for(i in 1:91){
  Carga_MEx[i]=mean(c(max(cbind(Carga_holt,Carga_arima,Carga_ME)[i,]),min(cbind
  (Carga_holt,Carga_arima,Carga_ME)[i,]))) }  

#Gráficos
graf(cow,cow_MEx,"Previsão da Série Cow por Combinação: MÉDIA dos Extremos")
graf(buffsnow,buffsnow_MEx,"Previsão da Série Buffsnow por Combinação: MÉDIA dos Extremos")
graf(calfem,calfem_MEx,"Previsão da Série Calfem por Combinação: MÉDIA dos Extremos")
graf(DowJones,DowJones_MEx,"Previsão da Série DowJones por Combinação: MÉDIA dos Extremos")
graf(globtp,globtp_MEx,"Previsão da Série Globtp por Combinação: MÉDIA dos Extremos")
graf(HURON,HURON_MEx,"Previsão da Série HURON por Combinação: MÉDIA dos Extremos")
graf(SHEEP,SHEEP_MEx,"Previsão da Série SHEEP por Combinação: MÉDIA dos Extremos")
graf(summer,summer_MEx,"Previsão da Série Summer por Combinação: MÉDIA dos Extremos")
graf(ausgundeaths,ausgundeaths_MEx,"Previsão da Série Ausgundeaths por Combinação: MÉDIA dos Extremos")
graf(Carga,Carga_MEx,"Previsão da Série Carga por Combinação: MÉDIA dos Extremos")

################################ Regressão ################################  
########### Cow
Reg_cow=lm(cow[1:60]~cow_holt[1:60]+cow_arima[1:60]+cow_ME[1:60])
Reg_cow
cow_R=c()
for (i in 1:75) {
cow_R[i]= Reg_cow$coefficients[1]+Reg_cow$coefficients[2]*cow_holt[i]+
  Reg_cow$coefficients[3]*cow_arima[i]+Reg_cow$coefficients[4]*cow_ME[i]}

########### Buffsnow
Reg_buffsnow=lm(buffsnow[1:51]~buffsnow_holt[1:51]+buffsnow_arima[1:51]+buffsnow_ME[1:51])
Reg_buffsnow
buffsnow_R=c()
for(i in 1:63){
buffsnow_R[i]=Reg_buffsnow$coefficients[1]+Reg_buffsnow$coefficients[2]*buffsnow_holt[i]+
Reg_buffsnow$coefficients[3]*buffsnow_arima[i]+Reg_buffsnow$coefficients[4]*buffsnow_ME[i]}

########### Calfem
Reg_calfem=lm(calfem[1:96]~calfem_holt[1:96]+calfem_arima[1:96]+calfem_ME[1:96])
Reg_calfem
calfem_R=c()
for(i in 1:120){       #Não tem a componente arima pois as previsões são uma constante.
calfem_R[i]=Reg_calfem$coefficients[1]+Reg_calfem$coefficients[2]*calfem_holt[i]
+Reg_calfem$coefficients[4]*calfem_ME[i]}

########### Dowjones
Reg_DowJones=lm(DowJones[1:63]~DowJones_holt[1:63]+DowJones_arima[1:63]+DowJones_ME[1:63])
Reg_DowJones
DowJones_R=c()
for(i in 1:78){
DowJones_R[i]=Reg_DowJones$coefficients[1]+Reg_DowJones$coefficients[2]*DowJones_holt[i]+
Reg_DowJones$coefficients[3]*DowJones_arima[i]+Reg_DowJones$coefficients[4]*DowJones_ME[i]}

########### Globtp
Reg_globtp=lm(globtp[1:85]~globtp_holt[1:85]+globtp_arima[1:85]+globtp_ME[1:85])
Reg_globtp
globtp_R=c()
for(i in 1:106){
globtp_R[i]=Reg_globtp$coefficients[1]+Reg_globtp$coefficients[2]*globtp_holt[i]+
Reg_globtp$coefficients[3]*globtp_arima[i]+Reg_globtp$coefficients[4]*globtp_ME[i]}

########### HURON
Reg_HURON=lm(HURON[1:79]~HURON_holt[1:79]+HURON_arima[1:79]+HURON_ME[1:79])
Reg_HURON
HURON_R=c()
for(i in 1:98){
HURON_R[i]=Reg_HURON$coefficients[1]+Reg_HURON$coefficients[2]*HURON_holt[i]+
Reg_HURON$coefficients[3]*HURON_arima[i]+Reg_HURON$coefficients[4]*HURON_ME[i]}

########### SHEEP
Reg_SHEEP=lm(SHEEP[1:59]~SHEEP_holt[1:59]+SHEEP_arima[1:59]+SHEEP_ME[1:59])
Reg_SHEEP
SHEEP_R=c()
for(i in 1:73){
SHEEP_R[i]=Reg_SHEEP$coefficients[1]+Reg_SHEEP$coefficients[2]*SHEEP_holt[i]+
Reg_SHEEP$coefficients[3]*SHEEP_arima[i]+Reg_SHEEP$coefficients[4]*SHEEP_ME[i]}

########### Summer
Reg_summer=lm(summer[1:84]~summer_holt[1:84]+summer_arima[1:84]+summer_ME[1:84])
Reg_summer
summer_R=c()
for(i in 1:104){
summer_R[i]=Reg_summer$coefficients[1]+Reg_summer$coefficients[2]*summer_holt[i]+
Reg_summer$coefficients[3]*summer_arima[i]+Reg_summer$coefficients[4]*summer_ME[i]}

########### Ausgundeaths
Reg_ausgundeaths=lm(ausgundeaths[1:72]~ausgundeaths_holt[1:72]+ausgundeaths_arima[1:72]+ausgundeaths_ME[1:72])
Reg_ausgundeaths
ausgundeaths_R=c()
for(i in 1:90){
ausgundeaths_R[i]=Reg_ausgundeaths$coefficients[1]+Reg_ausgundeaths$coefficients[2]*ausgundeaths_holt[i]+
Reg_ausgundeaths$coefficients[3]*ausgundeaths_arima[i]+Reg_ausgundeaths$coefficients[4]*ausgundeaths_ME[i]}

########### Carga
Reg_Carga=lm(Carga[1:73]~Carga_holt[1:73]+Carga_arima[1:73]+Carga_ME[1:73])
Reg_Carga
Carga_R=c()
for(i in 1:91){
Carga_R[i]=Reg_Carga$coefficients[1]+Reg_Carga$coefficients[2]*Carga_holt[i]+
Reg_Carga$coefficients[3]*Carga_arima[i]+Reg_Carga$coefficients[4]*Carga_ME[i]}

#Gráficos
graf(cow,cow_R,"Previsão da Série Cow por Combinação: Regressão")
graf(buffsnow,buffsnow_R,"Previsão da Série Buffsnow por Combinação: Regressão")
graf(calfem,calfem_R,"Previsão da Série Calfem por Combinação: Regressão")
graf(DowJones,DowJones_R,"Previsão da Série DowJones por Combinação: Regressão")
graf(globtp,globtp_R,"Previsão da Série Globtp por Combinação: Regressão")
graf(HURON,HURON_R,"Previsão da Série HURON por Combinação: Regressão")
graf(SHEEP,SHEEP_R,"Previsão da Série SHEEP por Combinação: Regressão")
graf(summer,summer_R,"Previsão da Série Summer por Combinação: Regressão")
graf(ausgundeaths,ausgundeaths_R,"Previsão da Série Ausgundeaths por Combinação: Regressão")
graf(Carga,Carga_R,"Previsão da Série Carga por Combinação: Regressão")

################################### Optimal ###################################
w_cow=Peso.function(cow[1:60],cow_holt[1:60],cow_arima[1:60],cow_ME[1:60])
cow_Op=cow_holt*w_cow[1,1]+cow_arima*w_cow[2,1]+cow_ME*w_cow[3,1]

w_buffsnow=Peso.function(buffsnow[1:51],buffsnow_holt[1:51],buffsnow_arima[1:51],buffsnow_ME[1:51])
buffsnow_Op=buffsnow_holt*w_buffsnow[1,1]+buffsnow_arima*w_buffsnow[2,1]+buffsnow_ME*w_buffsnow[3,1]

w_calfem=Peso.function(calfem[1:96],calfem_holt[1:96],calfem_arima[1:96],calfem_ME[1:96])
calfem_Op=calfem_holt*w_calfem[1,1]+calfem_arima*w_calfem[2,1]+calfem_ME*w_calfem[3,1]

w_DowJones=Peso.function(DowJones[1:63],DowJones_holt[1:63],DowJones_arima[1:63],DowJones_ME[1:63])
DowJones_Op=DowJones_holt*w_DowJones[1,1]+DowJones_arima*w_DowJones[2,1]+DowJones_ME*w_DowJones[3,1]

w_globtp=Peso.function(globtp[1:85],globtp_holt[1:85],globtp_arima[1:85],globtp_ME[1:85])
globtp_Op=globtp_holt*w_globtp[1,1]+globtp_arima*w_globtp[2,1]+globtp_ME*w_globtp[3,1]

w_HURON=Peso.function(HURON[1:79],HURON_holt[1:79],HURON_arima[1:79],HURON_ME[1:79])
HURON_Op=HURON_holt*w_HURON[1,1]+HURON_arima*w_HURON[2,1]+HURON_ME*w_HURON[3,1]

w_SHEEP=Peso.function(SHEEP[1:59],SHEEP_holt[1:59],SHEEP_arima[1:59],SHEEP_ME[1:59])
SHEEP_Op=SHEEP_holt*w_SHEEP[1,1]+SHEEP_arima*w_SHEEP[2,1]+SHEEP_ME*w_SHEEP[3,1]

w_summer=Peso.function(summer[1:84],summer_holt[1:84],summer_arima[1:84],summer_ME[1:84])
summer_Op=summer_holt*w_summer[1,1]+summer_arima*w_summer[2,1]+summer_ME*w_summer[3,1]

w_ausgundeaths=Peso.function(ausgundeaths[1:72],ausgundeaths_holt[1:72],ausgundeaths_arima[1:72],ausgundeaths_ME[1:72])
ausgundeaths_Op=ausgundeaths_holt*w_ausgundeaths[1,1]+ausgundeaths_arima*w_ausgundeaths[2,1]+ausgundeaths_ME*w_ausgundeaths[3,1]

w_Carga=Peso.function(Carga[1:73],Carga_holt[1:73],Carga_arima[1:73],Carga_ME[1:73])
Carga_Op=Carga_holt*w_Carga[1,1]+Carga_arima*w_Carga[2,1]+Carga_ME*w_Carga[3,1]

#Gráficos
graf(cow,cow_Op,"Previsão da Série Cow por Combinação: Optimal")
graf(buffsnow,buffsnow_Op,"Previsão da Série Buffsnow por Combinação: Optimal")
graf(calfem,calfem_Op,"Previsão da Série Calfem por Combinação: Optimal")
graf(DowJones,DowJones_Op,"Previsão da Série DowJones por Combinação: Optimal")
graf(globtp,globtp_Op,"Previsão da Série Globtp por Combinação: Optimal")
graf(HURON,HURON_Op,"Previsão da Série HURON por Combinação: Optimal")
graf(SHEEP,SHEEP_Op,"Previsão da Série SHEEP por Combinação: Optimal")
graf(summer,summer_Op,"Previsão da Série Summer por Combinação: Optimal")
graf(ausgundeaths,ausgundeaths_Op,"Previsão da Série Ausgundeaths por Combinação: Optimal")
graf(Carga,Carga_Op,"Previsão da Série Carga por Combinação: Optimal")

########################## Optimal com independência ########################### 
w_cow=Peso.function(cow[1:60],cow_holt[1:60],cow_arima[1:60],cow_ME[1:60],Ind=TRUE)
cow_OpI=cow_holt*w_cow[1,1]+cow_arima*w_cow[2,1]+cow_ME*w_cow[3,1]

w_buffsnow=Peso.function(buffsnow[1:51],buffsnow_holt[1:51],buffsnow_arima[1:51],buffsnow_ME[1:51],Ind=TRUE)
buffsnow_OpI=buffsnow_holt*w_buffsnow[1,1]+buffsnow_arima*w_buffsnow[2,1]+buffsnow_ME*w_buffsnow[3,1]

w_calfem=Peso.function(calfem[1:96],calfem_holt[1:96],calfem_arima[1:96],calfem_ME[1:96],Ind=TRUE)
calfem_OpI=calfem_holt*w_calfem[1,1]+calfem_arima*w_calfem[2,1]+calfem_ME*w_calfem[3,1]

w_DowJones=Peso.function(DowJones[1:63],DowJones_holt[1:63],DowJones_arima[1:63],DowJones_ME[1:63],Ind=TRUE)
DowJones_OpI=DowJones_holt*w_DowJones[1,1]+DowJones_arima*w_DowJones[2,1]+DowJones_ME*w_DowJones[3,1]

w_globtp=Peso.function(globtp[1:85],globtp_holt[1:85],globtp_arima[1:85],globtp_ME[1:85],Ind=TRUE)
globtp_OpI=globtp_holt*w_globtp[1,1]+globtp_arima*w_globtp[2,1]+globtp_ME*w_globtp[3,1]

w_HURON=Peso.function(HURON[1:79],HURON_holt[1:79],HURON_arima[1:79],HURON_ME[1:79],Ind=TRUE)
HURON_OpI=HURON_holt*w_HURON[1,1]+HURON_arima*w_HURON[2,1]+HURON_ME*w_HURON[3,1]

w_SHEEP=Peso.function(SHEEP[1:59],SHEEP_holt[1:59],SHEEP_arima[1:59],SHEEP_ME[1:59],Ind=TRUE)
SHEEP_OpI=SHEEP_holt*w_SHEEP[1,1]+SHEEP_arima*w_SHEEP[2,1]+SHEEP_ME*w_SHEEP[3,1]

w_summer=Peso.function(summer[1:84],summer_holt[1:84],summer_arima[1:84],summer_ME[1:84],Ind=TRUE)
summer_OpI=summer_holt*w_summer[1,1]+summer_arima*w_summer[2,1]+summer_ME*w_summer[3,1]

w_ausgundeaths=Peso.function(ausgundeaths[1:72],ausgundeaths_holt[1:72],ausgundeaths_arima[1:72],ausgundeaths_ME[1:72],Ind=TRUE)
ausgundeaths_OpI=ausgundeaths_holt*w_ausgundeaths[1,1]+ausgundeaths_arima*w_ausgundeaths[2,1]+ausgundeaths_ME*w_ausgundeaths[3,1]

w_Carga=Peso.function(Carga[1:73],Carga_holt[1:73],Carga_arima[1:73],Carga_ME[1:73],Ind=TRUE)
Carga_OpI=Carga_holt*w_Carga[1,1]+Carga_arima*w_Carga[2,1]+Carga_ME*w_Carga[3,1]

#Gráficos
#par(ask=TRUE)
graf(cow,cow_OpI,"Previsão da Série Cow por Combinação: Optimal com Independência")
graf(buffsnow,buffsnow_OpI,"Previsão da Série Buffsnow por Combinação: Optimal com Independência")
graf(calfem,calfem_OpI,"Previsão da Série Calfem por Combinação: Optimal com Independência")
graf(DowJones,DowJones_OpI,"Previsão da Série DowJones por Combinação: Optimal com Independência")
graf(globtp,globtp_OpI,"Previsão da Série Globtp por Combinação: Optimal com Independência")
graf(HURON,HURON_OpI,"Previsão da Série HURON por Combinação: Optimal com Independência")
graf(SHEEP,SHEEP_OpI,"Previsão da Série SHEEP por Combinação: Optimal com Independência")
graf(summer,summer_OpI,"Previsão da Série Summer por Combinação: Optimal com Independência")
graf(ausgundeaths,ausgundeaths_OpI,"Previsão da Série Ausgundeaths por Combinação: Optimal com Independência")
graf(Carga,Carga_OpI,"Previsão da Série Carga por Combinação: Optimal com Independência")

################################################################################   
############################ Combinações Reestimadas ###########################

################################## Regressão ###################################  
Reg_cow_r=lm(cow[1:60]~cow_holt[1:60]+cow_arima[1:60]+cow_ME[1:60])
cow_R_r=c()
cow_R_r[3:60]=Reg_cow_r$fitted.values
model_cow=matrix(Reg_cow_r$coefficients,1)
for (i in 61:75) {
  Reg_cow_r=lm(cow[1:i]~cow_holt[1:i]+cow_arima[1:i]+cow_ME[1:i])
  model_cow=rbind(model_cow,Reg_cow_r$coefficients)
  cow_R_r[i]= Reg_cow_r$coefficients[1]+Reg_cow_r$coefficients[2]*cow_holt[i]+
    Reg_cow_r$coefficients[3]*cow_arima[i]+Reg_cow_r$coefficients[4]*cow_ME[i]}
Reg_cow_r

Reg_buffsnow_r=lm(buffsnow[1:51]~buffsnow_holt[1:51]+buffsnow_arima[1:51]+buffsnow_ME[1:51])
buffsnow_R_r=c()
buffsnow_R_r[3:51]=Reg_buffsnow_r$fitted.values
model_buffsnow=matrix(Reg_buffsnow_r$coefficients,1)
for(i in 52:63){
  Reg_buffsnow_r=lm(buffsnow[1:i]~buffsnow_holt[1:i]+buffsnow_arima[1:i]+buffsnow_ME[1:i])
  model_buffsnow=rbind(model_buffsnow,Reg_buffsnow_r$coefficients)
  buffsnow_R_r[i]=Reg_buffsnow_r$coefficients[1]+Reg_buffsnow_r$coefficients[2]*buffsnow_holt[i]+
    Reg_buffsnow_r$coefficients[3]*buffsnow_arima[i]+Reg_buffsnow_r$coefficients[4]*buffsnow_ME[i]}
Reg_buffsnow_r

Reg_calfem_r=lm(calfem[1:96]~calfem_holt[1:96]+calfem_arima[1:96]+calfem_ME[1:96])
calfem_R_r=c()
calfem_R_r[3:96]=Reg_calfem_r$fitted.values
model_calfem=matrix(Reg_calfem_r$coefficients,1)
for(i in 97:120){       #Não tem a componente arima pois as previsões são uma constante.
  Reg_calfem_r=lm(calfem[1:i]~calfem_holt[1:i]+calfem_arima[1:i]+calfem_ME[1:i])
  model_calfem=rbind(model_calfem,Reg_calfem_r$coefficients)
  calfem_R_r[i]=Reg_calfem_r$coefficients[1]+Reg_calfem_r$coefficients[2]*calfem_holt[i]
  +Reg_calfem_r$coefficients[4]*calfem_ME[i]}
Reg_calfem_r

Reg_DowJones_r=lm(DowJones[1:63]~DowJones_holt[1:63]+DowJones_arima[1:63]+DowJones_ME[1:63])
DowJones_R_r=c()
DowJones_R_r[3:63]=Reg_DowJones_r$fitted.values
model_DowJones=matrix(Reg_DowJones_r$coefficients,1)
for(i in 64:78){
  Reg_DowJones_r=lm(DowJones[1:i]~DowJones_holt[1:i]+DowJones_arima[1:i]+DowJones_ME[1:i])
  model_DowJones=rbind(model_DowJones,Reg_DowJones_r$coefficients)
  DowJones_R_r[i]=Reg_DowJones_r$coefficients[1]+Reg_DowJones_r$coefficients[2]*DowJones_holt[i]+
    Reg_DowJones_r$coefficients[3]*DowJones_arima[i]+Reg_DowJones_r$coefficients[4]*DowJones_ME[i]}
Reg_DowJones_r

Reg_globtp_r=lm(globtp[1:85]~globtp_holt[1:85]+globtp_arima[1:85]+globtp_ME[1:85])
globtp_R_r=c()
globtp_R_r[3:85]=Reg_globtp_r$fitted.values
model_globtp=matrix(Reg_globtp_r$coefficients,1)
for(i in 86:106){
  Reg_globtp_r=lm(globtp[1:i]~globtp_holt[1:i]+globtp_arima[1:i]+globtp_ME[1:i])
  model_globtp=rbind(model_globtp,Reg_globtp_r$coefficients)
  globtp_R_r[i]=Reg_globtp_r$coefficients[1]+Reg_globtp_r$coefficients[2]*globtp_holt[i]+
    Reg_globtp_r$coefficients[3]*globtp_arima[i]+Reg_globtp_r$coefficients[4]*globtp_ME[i]}
Reg_globtp_r

Reg_HURON_r=lm(HURON[1:79]~HURON_holt[1:79]+HURON_arima[1:79]+HURON_ME[1:79])
HURON_R_r=c()
HURON_R_r[3:79]=Reg_HURON_r$fitted.values
model_HURON=matrix(Reg_HURON_r$coefficients,1)
for(i in 80:98){
  Reg_HURON_r=lm(HURON[1:i]~HURON_holt[1:i]+HURON_arima[1:i]+HURON_ME[1:i])
  model_HURON=rbind(model_HURON,Reg_HURON_r$coefficients)
  HURON_R_r[i]=Reg_HURON_r$coefficients[1]+Reg_HURON_r$coefficients[2]*HURON_holt[i]+
    Reg_HURON_r$coefficients[3]*HURON_arima[i]+Reg_HURON_r$coefficients[4]*HURON_ME[i]}
Reg_HURON_r

Reg_SHEEP_r=lm(SHEEP[1:59]~SHEEP_holt[1:59]+SHEEP_arima[1:59]+SHEEP_ME[1:59])
SHEEP_R_r=c()
SHEEP_R_r[3:59]=Reg_SHEEP_r$fitted.values
model_SHEEP=matrix(Reg_SHEEP_r$coefficients,1)
for(i in 60:73){
  Reg_SHEEP_r=lm(SHEEP[1:i]~SHEEP_holt[1:i]+SHEEP_arima[1:i]+SHEEP_ME[1:i])
  model_SHEEP=rbind(model_SHEEP,Reg_SHEEP_r$coefficients)
  SHEEP_R_r[i]=Reg_SHEEP_r$coefficients[1]+Reg_SHEEP_r$coefficients[2]*SHEEP_holt[i]+
    Reg_SHEEP_r$coefficients[3]*SHEEP_arima[i]+Reg_SHEEP_r$coefficients[4]*SHEEP_ME[i]}
Reg_SHEEP_r

Reg_summer_r=lm(summer[1:84]~summer_holt[1:84]+summer_arima[1:84]+summer_ME[1:84])
summer_R_r=c()
summer_R_r[3:84]=Reg_summer_r$fitted.values
model_summer=matrix(Reg_summer_r$coefficients,1)
for(i in 85:104){
  Reg_summer_r=lm(summer[1:i]~summer_holt[1:i]+summer_arima[1:i]+summer_ME[1:i])
  model_summer=rbind(model_summer,Reg_summer_r$coefficients)
  summer_R_r[i]=Reg_summer_r$coefficients[1]+Reg_summer_r$coefficients[2]*summer_holt[i]+
    Reg_summer_r$coefficients[3]*summer_arima[i]+Reg_summer_r$coefficients[4]*summer_ME[i]}
Reg_summer_r

Reg_ausgundeaths_r=lm(ausgundeaths[1:72]~ausgundeaths_holt[1:72]+ausgundeaths_arima[1:72]+ausgundeaths_ME[1:72])
ausgundeaths_R_r=c()
ausgundeaths_R_r[3:72]=Reg_ausgundeaths_r$fitted.values
model_ausgundeaths=matrix(Reg_ausgundeaths_r$coefficients,1)
for(i in 73:90){
  Reg_ausgundeaths_r=lm(ausgundeaths[1:i]~ausgundeaths_holt[1:i]+ausgundeaths_arima[1:i]+ausgundeaths_ME[1:i])
  model_ausgundeaths=rbind(model_ausgundeaths,Reg_ausgundeaths_r$coefficients)
  ausgundeaths_R_r[i]=Reg_ausgundeaths_r$coefficients[1]+Reg_ausgundeaths_r$coefficients[2]*ausgundeaths_holt[i]+
    Reg_ausgundeaths_r$coefficients[3]*ausgundeaths_arima[i]+Reg_ausgundeaths_r$coefficients[4]*ausgundeaths_ME[i]}
Reg_ausgundeaths_r

Reg_Carga_r=lm(Carga[1:73]~Carga_holt[1:73]+Carga_arima[1:73]+Carga_ME[1:73])
Carga_R_r=c()
Carga_R_r[3:73]=Reg_Carga_r$fitted.values
model_Carga=matrix(Reg_Carga_r$coefficients,1)
for(i in 74:91){
  Reg_Carga_r=lm(Carga[1:i]~Carga_holt[1:i]+Carga_arima[1:i]+Carga_ME[1:i])
  model_Carga=rbind(model_Carga,Reg_Carga_r$coefficients)
  Carga_R_r[i]=Reg_Carga_r$coefficients[1]+Reg_Carga_r$coefficients[2]*Carga_holt[i]+
    Reg_Carga_r$coefficients[3]*Carga_arima[i]+Reg_Carga_r$coefficients[4]*Carga_ME[i]}
Reg_Carga_r

#Gráficos
graf(cow,cow_R_r,"Previsão da série Cow por Combinação: Regressão_Reestimando")
graf(buffsnow,buffsnow_R_r,"Previsão da série Buffsnow por Combinação: Regressão_Reestimando")
graf(calfem,calfem_R_r,"Previsão da série Calfem por Combinação: Regressão_Reestimando")
graf(DowJones,DowJones_R_r,"Previsão da série DowJones por Combinação: Regressão_Reestimando")
graf(globtp,globtp_R_r,"Previsão da série Globtp por Combinação: Regressão_Reestimando")
graf(HURON,HURON_R_r,"Previsão da série HURON por Combinação: Regressão_Reestimando")
graf(SHEEP,SHEEP_R_r,"Previsão da série SHEEP por Combinação: Regressão_Reestimando")
graf(summer,summer_R_r,"Previsão da série Summer por Combinação: Regressão_Reestimando")
graf(ausgundeaths,ausgundeaths_R_r,"Previsão da série Ausgundeaths por Combinação: Regressão_Reestimando")
graf(Carga,Carga_R_r,"Previsão da série Carga por Combinação: Regressão_Reestimando")

graf2(model_cow)
graf2(model_buffsnow)
#graf2(model_calfem)
graf2(model_DowJones)
graf2(model_globtp)
graf2(model_HURON)
graf2(model_SHEEP)
graf2(model_summer)
graf2(model_ausgundeaths)
graf2(model_Carga)

#################################### Optimal ###################################     
w_cow_r=Peso.function(cow[1:60],cow_holt[1:60],cow_arima[1:60],cow_ME[1:60])
cow_Op_r=c()
cow_Op_r=cow_holt[1:60]*w_cow_r[1,1]+cow_arima[1:60]*w_cow_r[2,1]+cow_ME[1:60]*w_cow_r[3,1]
peso_cow_r=w_cow_r
for(i in 61:75){
  w_cow_r=Peso.function(cow[1:i],cow_holt[1:i],cow_arima[1:i],cow_ME[1:i])
  peso_cow_r=cbind(peso_cow_r,w_cow_r)
  cow_Op_r[i]=cow_holt[i]*w_cow_r[1,1]+cow_arima[i]*w_cow_r[2,1]+cow_ME[i]*w_cow_r[3,1]
}

w_buffsnow_r=Peso.function(buffsnow[1:51],buffsnow_holt[1:51],buffsnow_arima[1:51],buffsnow_ME[1:51])
buffsnow_Op_r=c()
buffsnow_Op_r=buffsnow_holt[1:51]*w_buffsnow_r[1,1]+buffsnow_arima[1:51]*w_buffsnow_r[2,1]+buffsnow_ME[1:51]*w_buffsnow_r[3,1]
peso_buffsnow_r=w_buffsnow_r
for(i in 52:63){
  w_buffsnow_r=Peso.function(buffsnow[1:i],buffsnow_holt[1:i],buffsnow_arima[1:i],buffsnow_ME[1:i])
  peso_buffsnow_r=cbind(peso_buffsnow_r,w_buffsnow_r)
  buffsnow_Op_r[i]=buffsnow_holt[i]*w_buffsnow_r[1,1]+buffsnow_arima[i]*w_buffsnow_r[2,1]+buffsnow_ME[i]*w_buffsnow_r[3,1]
}

w_calfem_r=Peso.function(calfem[1:96],calfem_holt[1:96],calfem_arima[1:96],calfem_ME[1:96])
calfem_Op_r=c()
calfem_Op_r=calfem_holt[1:96]*w_calfem_r[1,1]+calfem_arima[1:96]*w_calfem_r[2,1]+calfem_ME[1:96]*w_calfem_r[3,1]
peso_calfem_r=w_calfem_r
for(i in 97:120){  
  w_calfem_r=Peso.function(calfem[1:i],calfem_holt[1:i],calfem_arima[1:i],calfem_ME[1:i])
  peso_calfem_r=cbind(peso_calfem_r,w_calfem_r)
  calfem_Op_r[i]=calfem_holt[i]*w_calfem_r[1,1]+calfem_arima[i]*w_calfem_r[2,1]+calfem_ME[i]*w_calfem_r[3,1]
}

w_DowJones_r=Peso.function(DowJones[1:63],DowJones_holt[1:63],DowJones_arima[1:63],DowJones_ME[1:63])
DowJones_Op_r=c()
DowJones_Op_r=DowJones_holt[1:63]*w_DowJones_r[1,1]+DowJones_arima[1:63]*w_DowJones_r[2,1]+DowJones_ME[1:63]*w_DowJones_r[3,1]
peso_DowJones_r=w_DowJones_r
for(i in 64:78){
  w_DowJones_r=Peso.function(DowJones[1:i],DowJones_holt[1:i],DowJones_arima[1:i],DowJones_ME[1:i])
  peso_DowJones_r=cbind(peso_DowJones_r,w_DowJones_r)
  DowJones_Op_r[i]=DowJones_holt[i]*w_DowJones_r[1,1]+DowJones_arima[i]*w_DowJones_r[2,1]+DowJones_ME[i]*w_DowJones_r[3,1]
}

w_globtp_r=Peso.function(globtp[1:85],globtp_holt[1:85],globtp_arima[1:85],globtp_ME[1:85])
globtp_Op_r=c()
globtp_Op_r=globtp_holt[1:85]*w_globtp_r[1,1]+globtp_arima[1:85]*w_globtp_r[2,1]+globtp_ME[1:85]*w_globtp_r[3,1]
peso_globtp_r=w_globtp_r
for(i in 86:106){
  w_globtp_r=Peso.function(globtp[1:i],globtp_holt[1:i],globtp_arima[1:i],globtp_ME[1:i])
  peso_globtp_r=cbind(peso_globtp_r,w_globtp_r)
  globtp_Op_r[i]=globtp_holt[i]*w_globtp_r[1,1]+globtp_arima[i]*w_globtp_r[2,1]+globtp_ME[i]*w_globtp_r[3,1]
}

w_HURON_r=Peso.function(HURON[1:79],HURON_holt[1:79],HURON_arima[1:79],HURON_ME[1:79])
HURON_Op_r=c()
HURON_Op_r=HURON_holt[1:79]*w_HURON_r[1,1]+HURON_arima[1:79]*w_HURON_r[2,1]+HURON_ME[1:79]*w_HURON_r[3,1]
peso_HURON_r=w_HURON_r
for (i in 80:98){
  w_HURON_r=Peso.function(HURON[1:i],HURON_holt[1:i],HURON_arima[1:i],HURON_ME[1:i])
  peso_HURON_r=cbind(peso_HURON_r,w_HURON_r)
  HURON_Op_r[i]=HURON_holt[i]*w_HURON_r[1,1]+HURON_arima[i]*w_HURON_r[2,1]+HURON_ME[i]*w_HURON_r[3,1]
}

w_SHEEP_r=Peso.function(SHEEP[1:59],SHEEP_holt[1:59],SHEEP_arima[1:59],SHEEP_ME[1:59])
SHEEP_Op_r=c()
SHEEP_Op_r=SHEEP_holt[1:59]*w_SHEEP_r[1,1]+SHEEP_arima[1:59]*w_SHEEP_r[2,1]+SHEEP_ME[1:59]*w_SHEEP_r[3,1]
peso_SHEEP_r=w_SHEEP_r
for (i in 60:73){
  w_SHEEP_r=Peso.function(SHEEP[1:i],SHEEP_holt[1:i],SHEEP_arima[1:i],SHEEP_ME[1:i])
  peso_SHEEP_r=cbind(peso_SHEEP_r,w_SHEEP_r)
  SHEEP_Op_r[i]=SHEEP_holt[i]*w_SHEEP_r[1,1]+SHEEP_arima[i]*w_SHEEP_r[2,1]+SHEEP_ME[i]*w_SHEEP_r[3,1]
}

w_summer_r=Peso.function(summer[1:84],summer_holt[1:84],summer_arima[1:84],summer_ME[1:84])
summer_Op_r=c()
summer_Op_r=summer_holt[1:84]*w_summer_r[1,1]+summer_arima[1:84]*w_summer_r[2,1]+summer_ME[1:84]*w_summer_r[3,1]
peso_summer_r=w_summer_r
for(i in 85:104){
  w_summer_r=Peso.function(summer[1:i],summer_holt[1:i],summer_arima[1:i],summer_ME[1:i])
  peso_summer_r=cbind(peso_summer_r,w_summer_r)
  summer_Op_r[i]=summer_holt[i]*w_summer_r[1,1]+summer_arima[i]*w_summer_r[2,1]+summer_ME[i]*w_summer_r[3,1]
}

w_ausgundeaths_r=Peso.function(ausgundeaths[1:72],ausgundeaths_holt[1:72],ausgundeaths_arima[1:72],ausgundeaths_ME[1:72])
ausgundeaths_Op_r=c()
ausgundeaths_Op_r=ausgundeaths_holt[1:72]*w_ausgundeaths_r[1,1]+ausgundeaths_arima[1:72]*w_ausgundeaths_r[2,1]+ausgundeaths_ME[1:72]*w_ausgundeaths_r[3,1]
peso_ausgundeaths_r=w_ausgundeaths_r
for (i in 73:90){
  w_ausgundeaths_r=Peso.function(ausgundeaths[1:i],ausgundeaths_holt[1:i],ausgundeaths_arima[1:i],ausgundeaths_ME[1:i])
  peso_ausgundeaths_r=cbind(peso_ausgundeaths_r,w_ausgundeaths_r)
  ausgundeaths_Op_r[i]=ausgundeaths_holt[i]*w_ausgundeaths_r[1,1]+ausgundeaths_arima[i]*w_ausgundeaths_r[2,1]+ausgundeaths_ME[i]*w_ausgundeaths_r[3,1]
}

w_Carga_r=Peso.function(Carga[1:73],Carga_holt[1:73],Carga_arima[1:73],Carga_ME[1:73])
Carga_Op_r=c()
Carga_Op_r=Carga_holt[1:73]*w_Carga_r[1,1]+Carga_arima[1:73]*w_Carga_r[2,1]+Carga_ME[1:73]*w_Carga_r[3,1]
peso_Carga_r=w_Carga_r
for(i in 74:91){
  w_Carga_r=Peso.function(Carga[1:i],Carga_holt[1:i],Carga_arima[1:i],Carga_ME[1:i])
  peso_Carga_r=cbind(peso_Carga_r,w_Carga_r)
  Carga_Op_r[i]=Carga_holt[i]*w_Carga_r[1,1]+Carga_arima[i]*w_Carga_r[2,1]+Carga_ME[i]*w_Carga_r[3,1]
}

#Gráficos
graf(cow,cow_Op_r,"Previsão da série Cow por Combinação: Optimal_Reestimando")
graf(buffsnow,buffsnow_Op_r,"Previsão da série Buffsnow por Combinação: Optimal_Reestimando")
graf(calfem,calfem_Op_r,"Previsão da série Calfem por Combinação: Optimal_Reestimando")
graf(DowJones,DowJones_Op_r,"Previsão da série DowJones por Combinação: Optimal_Reestimando")
graf(globtp,globtp_Op_r,"Previsão da série Globtp por Combinação: Optimal_Reestimando")
graf(HURON,HURON_Op_r,"Previsão da série HURON por Combinação: Optimal_Reestimando")
graf(SHEEP,SHEEP_Op_r,"Previsão da série SHEEP por Combinação: Optimal_Reestimando")
graf(summer,summer_Op_r,"Previsão da série Summer por Combinação: Optimal_Reestimando")
graf(ausgundeaths,ausgundeaths_Op_r,"Previsão da série Ausgundeaths por Combinação: Optimal_Reestimando")
graf(Carga,Carga_Op_r,"Previsão da série Carga por Combinação: Optimal_Reestimando")

graf3(peso_cow_r)
graf3(peso_buffsnow_r)
graf3(peso_calfem_r)
graf3(peso_DowJones_r)
graf3(peso_globtp_r)
graf3(peso_HURON_r)
graf3(peso_SHEEP_r)
graf3(peso_summer_r)
graf3(peso_ausgundeaths_r)
graf3(peso_Carga_r)

########################### Optimal com independência ##########################
w_cow_r=Peso.function(cow[1:60],cow_holt[1:60],cow_arima[1:60],cow_ME[1:60],Ind=TRUE)
cow_OpI_r=c()
cow_OpI_r=cow_holt[1:60]*w_cow_r[1,1]+cow_arima[1:60]*w_cow_r[2,1]+cow_ME[1:60]*w_cow_r[3,1]
peso_cow_r=w_cow_r
for(i in 61:75){
  w_cow_r=Peso.function(cow[1:i],cow_holt[1:i],cow_arima[1:i],cow_ME[1:i],Ind=TRUE)
  peso_cow_r=cbind(peso_cow_r,w_cow_r)
  cow_OpI_r[i]=cow_holt[i]*w_cow_r[1,1]+cow_arima[i]*w_cow_r[2,1]+cow_ME[i]*w_cow_r[3,1]
}

w_buffsnow_r=Peso.function(buffsnow[1:51],buffsnow_holt[1:51],buffsnow_arima[1:51],buffsnow_ME[1:51],Ind=TRUE)
buffsnow_OpI_r=c()
buffsnow_OpI_r=buffsnow_holt[1:51]*w_buffsnow_r[1,1]+buffsnow_arima[1:51]*w_buffsnow_r[2,1]+buffsnow_ME[1:51]*w_buffsnow_r[3,1]
peso_buffsnow_r=w_buffsnow_r
for(i in 52:63){
  w_buffsnow_r=Peso.function(buffsnow[1:i],buffsnow_holt[1:i],buffsnow_arima[1:i],buffsnow_ME[1:i],Ind=TRUE)
  peso_buffsnow_r=cbind(peso_buffsnow_r,w_buffsnow_r)
  buffsnow_OpI_r[i]=buffsnow_holt[i]*w_buffsnow_r[1,1]+buffsnow_arima[i]*w_buffsnow_r[2,1]+buffsnow_ME[i]*w_buffsnow_r[3,1]
}

w_calfem_r=Peso.function(calfem[1:96],calfem_holt[1:96],calfem_arima[1:96],calfem_ME[1:96],Ind=TRUE)
calfem_OpI_r=c()
calfem_OpI_r=calfem_holt[1:96]*w_calfem_r[1,1]+calfem_arima[1:96]*w_calfem_r[2,1]+calfem_ME[1:96]*w_calfem_r[3,1]
peso_calfem_r=w_calfem_r
for(i in 97:120){  
  w_calfem_r=Peso.function(calfem[1:i],calfem_holt[1:i],calfem_arima[1:i],calfem_ME[1:i],Ind=TRUE)
  peso_calfem_r=cbind(peso_calfem_r,w_calfem_r)
  calfem_OpI_r[i]=calfem_holt[i]*w_calfem_r[1,1]+calfem_arima[i]*w_calfem_r[2,1]+calfem_ME[i]*w_calfem_r[3,1]
}

w_DowJones_r=Peso.function(DowJones[1:63],DowJones_holt[1:63],DowJones_arima[1:63],DowJones_ME[1:63],Ind=TRUE)
DowJones_OpI_r=c()
DowJones_OpI_r=DowJones_holt[1:63]*w_DowJones_r[1,1]+DowJones_arima[1:63]*w_DowJones_r[2,1]+DowJones_ME[1:63]*w_DowJones_r[3,1]
peso_DowJones_r=w_DowJones_r
for(i in 64:78){
  w_DowJones_r=Peso.function(DowJones[1:i],DowJones_holt[1:i],DowJones_arima[1:i],DowJones_ME[1:i],Ind=TRUE)
  peso_DowJones_r=cbind(peso_DowJones_r,w_DowJones_r)
  DowJones_OpI_r[i]=DowJones_holt[i]*w_DowJones_r[1,1]+DowJones_arima[i]*w_DowJones_r[2,1]+DowJones_ME[i]*w_DowJones_r[3,1]
}

w_globtp_r=Peso.function(globtp[1:85],globtp_holt[1:85],globtp_arima[1:85],globtp_ME[1:85],Ind=TRUE)
globtp_OpI_r=c()
globtp_OpI_r=globtp_holt[1:85]*w_globtp_r[1,1]+globtp_arima[1:85]*w_globtp_r[2,1]+globtp_ME[1:85]*w_globtp_r[3,1]
peso_globtp_r=w_globtp_r
for(i in 86:106){
  w_globtp_r=Peso.function(globtp[1:i],globtp_holt[1:i],globtp_arima[1:i],globtp_ME[1:i],Ind=TRUE)
  peso_globtp_r=cbind(peso_globtp_r,w_globtp_r)
  globtp_OpI_r[i]=globtp_holt[i]*w_globtp_r[1,1]+globtp_arima[i]*w_globtp_r[2,1]+globtp_ME[i]*w_globtp_r[3,1]
}

w_HURON_r=Peso.function(HURON[1:79],HURON_holt[1:79],HURON_arima[1:79],HURON_ME[1:79],Ind=TRUE)
HURON_OpI_r=c()
HURON_OpI_r=HURON_holt[1:79]*w_HURON_r[1,1]+HURON_arima[1:79]*w_HURON_r[2,1]+HURON_ME[1:79]*w_HURON_r[3,1]
peso_HURON_r=w_HURON_r
for (i in 80:98){
  w_HURON_r=Peso.function(HURON[1:i],HURON_holt[1:i],HURON_arima[1:i],HURON_ME[1:i],Ind=TRUE)
  peso_HURON_r=cbind(peso_HURON_r,w_HURON_r)
  HURON_OpI_r[i]=HURON_holt[i]*w_HURON_r[1,1]+HURON_arima[i]*w_HURON_r[2,1]+HURON_ME[i]*w_HURON_r[3,1]
}

w_SHEEP_r=Peso.function(SHEEP[1:59],SHEEP_holt[1:59],SHEEP_arima[1:59],SHEEP_ME[1:59],Ind=TRUE)
SHEEP_OpI_r=c()
SHEEP_OpI_r=SHEEP_holt[1:59]*w_SHEEP_r[1,1]+SHEEP_arima[1:59]*w_SHEEP_r[2,1]+SHEEP_ME[1:59]*w_SHEEP_r[3,1]
peso_SHEEP_r=w_SHEEP_r
for (i in 60:73){
  w_SHEEP_r=Peso.function(SHEEP[1:i],SHEEP_holt[1:i],SHEEP_arima[1:i],SHEEP_ME[1:i],Ind=TRUE)
  peso_SHEEP_r=cbind(peso_SHEEP_r,w_SHEEP_r)
  SHEEP_OpI_r[i]=SHEEP_holt[i]*w_SHEEP_r[1,1]+SHEEP_arima[i]*w_SHEEP_r[2,1]+SHEEP_ME[i]*w_SHEEP_r[3,1]
}

w_summer_r=Peso.function(summer[1:84],summer_holt[1:84],summer_arima[1:84],summer_ME[1:84],Ind=TRUE)
summer_OpI_r=c()
summer_OpI_r=summer_holt[1:84]*w_summer_r[1,1]+summer_arima[1:84]*w_summer_r[2,1]+summer_ME[1:84]*w_summer_r[3,1]
peso_summer_r=w_summer_r
for(i in 85:104){
  w_summer_r=Peso.function(summer[1:i],summer_holt[1:i],summer_arima[1:i],summer_ME[1:i],Ind=TRUE)
  peso_summer_r=cbind(peso_summer_r,w_summer_r)
  summer_OpI_r[i]=summer_holt[i]*w_summer_r[1,1]+summer_arima[i]*w_summer_r[2,1]+summer_ME[i]*w_summer_r[3,1]
}

w_ausgundeaths_r=Peso.function(ausgundeaths[1:72],ausgundeaths_holt[1:72],ausgundeaths_arima[1:72],ausgundeaths_ME[1:72],Ind=TRUE)
ausgundeaths_OpI_r=c()
ausgundeaths_OpI_r=ausgundeaths_holt[1:72]*w_ausgundeaths_r[1,1]+ausgundeaths_arima[1:72]*w_ausgundeaths_r[2,1]+ausgundeaths_ME[1:72]*w_ausgundeaths_r[3,1]
peso_ausgundeaths_r=w_ausgundeaths_r
for (i in 73:90){
  w_ausgundeaths_r=Peso.function(ausgundeaths[1:i],ausgundeaths_holt[1:i],ausgundeaths_arima[1:i],ausgundeaths_ME[1:i],Ind=TRUE)
  peso_ausgundeaths_r=cbind(peso_ausgundeaths_r,w_ausgundeaths_r)
  ausgundeaths_OpI_r[i]=ausgundeaths_holt[i]*w_ausgundeaths_r[1,1]+ausgundeaths_arima[i]*w_ausgundeaths_r[2,1]+ausgundeaths_ME[i]*w_ausgundeaths_r[3,1]
}

w_Carga_r=Peso.function(Carga[1:73],Carga_holt[1:73],Carga_arima[1:73],Carga_ME[1:73],Ind=TRUE)
Carga_OpI_r=c()
Carga_OpI_r=Carga_holt[1:73]*w_Carga_r[1,1]+Carga_arima[1:73]*w_Carga_r[2,1]+Carga_ME[1:73]*w_Carga_r[3,1]
peso_Carga_r=w_Carga_r
for(i in 74:91){
  w_Carga_r=Peso.function(Carga[1:i],Carga_holt[1:i],Carga_arima[1:i],Carga_ME[1:i],Ind=TRUE)
  peso_Carga_r=cbind(peso_Carga_r,w_Carga_r)
  Carga_OpI_r[i]=Carga_holt[i]*w_Carga_r[1,1]+Carga_arima[i]*w_Carga_r[2,1]+Carga_ME[i]*w_Carga_r[3,1]
}

#Gráficos
graf(cow,cow_OpI_r,"Previsão da série Cow por Combinação: Optimal_Ind_Reestimando")
graf(buffsnow,buffsnow_OpI_r,"Previsão da série Buffsnow por Combinação: Optimal_Ind_Reestimando")
graf(calfem,calfem_OpI_r,"Previsão da série Calfem por Combinação: Optimal_Ind_Reestimando")
graf(DowJones,DowJones_OpI_r,"Previsão da série DowJones por Combinação: Optimal_Ind_Reestimando")
graf(globtp,globtp_OpI_r,"Previsão da série Globtp por Combinação: Optimal_Ind_Reestimando")
graf(HURON,HURON_OpI_r,"Previsão da série HURON por Combinação: Optimal_Ind_Reestimando")
graf(SHEEP,SHEEP_OpI_r,"Previsão da série SHEEP por Combinação: Optimal_Ind_Reestimando")
graf(summer,summer_OpI_r,"Previsão da série Summer por Combinação: Optimal_Ind_Reestimando")
graf(ausgundeaths,ausgundeaths_OpI_r,"Previsão da série Ausgundeaths por Combinação: Optimal_Ind_Reestimando")
graf(Carga,Carga_OpI_r,"Previsão da série Carga por Combinação: Optimal_Ind_Reestimando")

graf3(peso_cow_r)
graf3(peso_buffsnow_r)
graf3(peso_calfem_r)
graf3(peso_DowJones_r)
graf3(peso_globtp_r)
graf3(peso_HURON_r)
graf3(peso_SHEEP_r)
graf3(peso_summer_r)
graf3(peso_ausgundeaths_r)
graf3(peso_Carga_r)

################################################################################   
############################# Comparação De Modelos ############################
#Tabela de MSE
Tab_Erros = matrix(
  c(
    erros(cow,c(61,75),cow_holt)[2],
    erros(cow,c(61,75),cow_arima)[2],
    erros(cow,c(61,75),cow_ME)[2],
    erros(cow,c(61,75),cow_MS)[2],
    erros(cow,c(61,75),cow_Med)[2],
    erros(cow,c(61,75),cow_MEx)[2],
    erros(cow,c(61,75),cow_R)[2],
    erros(cow,c(61,75),cow_R_r)[2],
    erros(cow,c(61,75),cow_Op)[2],
    erros(cow,c(61,75),cow_Op_r)[2],
    erros(cow,c(61,75),cow_OpI)[2],
   erros(cow,c(61,75),cow_OpI_r)[2],
   
   erros(buffsnow,c(52,63),buffsnow_holt)[2],
   erros(buffsnow,c(52,63),buffsnow_arima)[2],
   erros(buffsnow,c(52,63),buffsnow_ME)[2],
   erros(buffsnow,c(52,63),buffsnow_MS)[2],
   erros(buffsnow,c(52,63),buffsnow_Med)[2],
   erros(buffsnow,c(52,63),buffsnow_MEx)[2],
   erros(buffsnow,c(52,63),buffsnow_R)[2],
   erros(buffsnow,c(52,63),buffsnow_R_r)[2],
   erros(buffsnow,c(52,63),buffsnow_Op)[2],
   erros(buffsnow,c(52,63),buffsnow_Op_r)[2],
   erros(buffsnow,c(52,63),buffsnow_OpI)[2],
   erros(buffsnow,c(52,63),buffsnow_OpI_r)[2],

   erros(calfem,c(97,120),calfem_holt)[2],
   erros(calfem,c(97,120),calfem_arima)[2],
   erros(calfem,c(97,120),calfem_ME)[2],
   erros(calfem,c(97,120),calfem_MS)[2],
   erros(calfem,c(97,120),calfem_Med)[2],
   erros(calfem,c(97,120),calfem_MEx)[2],
   erros(calfem,c(97,120),calfem_R)[2],
   erros(calfem,c(97,120),calfem_R_r)[2],
   erros(calfem,c(97,120),calfem_Op)[2],
   erros(calfem,c(97,120),calfem_Op_r)[2],
   erros(calfem,c(97,120),calfem_OpI)[2],
   erros(calfem,c(97,120),calfem_OpI_r)[2],
   
   
  erros(DowJones,c(64,78),DowJones_holt)[2],
  erros(DowJones,c(64,78),DowJones_arima)[2],
  erros(DowJones,c(64,78),DowJones_ME)[2],
  erros(DowJones,c(64,78),DowJones_MS)[2],
  erros(DowJones,c(64,78),DowJones_Med)[2],
  erros(DowJones,c(64,78),DowJones_MEx)[2],
  erros(DowJones,c(64,78),DowJones_R)[2],
  erros(DowJones,c(64,78),DowJones_R_r)[2],
  erros(DowJones,c(64,78),DowJones_Op)[2],
  erros(DowJones,c(64,78),DowJones_Op_r)[2],
  erros(DowJones,c(64,78),DowJones_OpI)[2],
  erros(DowJones,c(64,78),DowJones_OpI_r)[2],
  
  erros(globtp,c(86,106),globtp_holt)[2],
  erros(globtp,c(86,106),globtp_arima)[2],
  erros(globtp,c(86,106),globtp_ME)[2],
  erros(globtp,c(86,106),globtp_MS)[2],
  erros(globtp,c(86,106),globtp_Med)[2],
  erros(globtp,c(86,106),globtp_MEx)[2],
  erros(globtp,c(86,106),globtp_R)[2],
  erros(globtp,c(86,106),globtp_R_r)[2],
  erros(globtp,c(86,106),globtp_Op)[2],
  erros(globtp,c(86,106),globtp_Op_r)[2],
  erros(globtp,c(86,106),globtp_OpI)[2],
  erros(globtp,c(86,106),globtp_OpI_r)[2],
  
  erros(HURON,c(80,98),HURON_holt)[2],
  erros(HURON,c(80,98),HURON_arima)[2],
  erros(HURON,c(80,98),HURON_ME)[2],
  erros(HURON,c(80,98),HURON_MS)[2],
  erros(HURON,c(80,98),HURON_Med)[2],
  erros(HURON,c(80,98),HURON_MEx)[2],
  erros(HURON,c(80,98),HURON_R)[2],
  erros(HURON,c(80,98),HURON_R_r)[2],
  erros(HURON,c(80,98),HURON_Op)[2],
  erros(HURON,c(80,98),HURON_Op_r)[2],
  erros(HURON,c(80,98),HURON_OpI)[2],
  erros(HURON,c(80,98),HURON_OpI_r)[2],
  
  erros(SHEEP,c(60,73),SHEEP_holt)[2],
  erros(SHEEP,c(60,73),SHEEP_arima)[2],
  erros(SHEEP,c(60,73),SHEEP_ME)[2],
  erros(SHEEP,c(60,73),SHEEP_MS)[2],
  erros(SHEEP,c(60,73),SHEEP_Med)[2],
  erros(SHEEP,c(60,73),SHEEP_MEx)[2],
  erros(SHEEP,c(60,73),SHEEP_R)[2],
  erros(SHEEP,c(60,73),SHEEP_R_r)[2],
  erros(SHEEP,c(60,73),SHEEP_Op)[2],
  erros(SHEEP,c(60,73),SHEEP_Op_r)[2],
  erros(SHEEP,c(60,73),SHEEP_OpI)[2],
  erros(SHEEP,c(60,73),SHEEP_OpI_r)[2],
  
  erros(summer,c(85,104),summer_holt)[2],
  erros(summer,c(85,104),summer_arima)[2],
  erros(summer,c(85,104),summer_ME)[2],
  erros(summer,c(85,104),summer_MS)[2],
  erros(summer,c(85,104),summer_Med)[2],
  erros(summer,c(85,104),summer_MEx)[2],
  erros(summer,c(85,104),summer_R)[2],
  erros(summer,c(85,104),summer_R_r)[2],
  erros(summer,c(85,104),summer_Op)[2],
  erros(summer,c(85,104),summer_Op_r)[2],
  erros(summer,c(85,104),summer_OpI)[2],
  erros(summer,c(85,104),summer_OpI_r)[2],
  
  erros(ausgundeaths,c(73,90),ausgundeaths_holt)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_arima)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_ME)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_MS)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_Med)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_MEx)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_R)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_R_r)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_Op)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_Op_r)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_OpI)[2],
  erros(ausgundeaths,c(73,90),ausgundeaths_OpI_r)[2],
  
  erros(Carga,c(74,91),Carga_holt)[2],
  erros(Carga,c(74,91),Carga_arima)[2],
  erros(Carga,c(74,91),Carga_ME)[2],
  erros(Carga,c(74,91),Carga_MS)[2],
  erros(Carga,c(74,91),Carga_Med)[2],
  erros(Carga,c(74,91),Carga_MEx)[2],
  erros(Carga,c(74,91),Carga_R)[2],
  erros(Carga,c(74,91),Carga_R_r)[2],
  erros(Carga,c(74,91),Carga_Op)[2],
  erros(Carga,c(74,91),Carga_Op_r)[2],
  erros(Carga,c(74,91),Carga_OpI)[2],
  erros(Carga,c(74,91),Carga_OpI_r)[2]),
  nrow = 10, ncol = 12,byrow = TRUE,dimnames = list(c("Cow","Buffsnow","CalFem","DowJones",
                                     "Globtp","HURON","SHEEP","Summer",
                                     "Ausgundeaths","Carga"),
                                   c(" HOLT","ARIMA","ESTRUTURAL",
                                     "MEDIA SIMPLES","MEDIANA",
                                     "MEDIA_DOS_EXTREMOS","REGRESSAO",
                                     "REGRESSAO_res","OPTIMAL","OPTIMAL_res",
                                     "OPTIMAL I","OPTIMAL I_res")
                                   ))


t(Tab_Erros)

### Salvando a tabela de erros ###
write.table(Tab_Erros,"Tab_Erros.txt")

# Gráficos MSE
cw=as.numeric(Tab_Erros[1,])
b=as.numeric(Tab_Erros[2,])
c=as.numeric(Tab_Erros[3,])
d=as.numeric(Tab_Erros[4,])
g=as.numeric(Tab_Erros[5,])
h=as.numeric(Tab_Erros[6,])
s=as.numeric(Tab_Erros[7,])
su=as.numeric(Tab_Erros[8,])
a=as.numeric(Tab_Erros[9,])
cr=as.numeric(Tab_Erros[10,])
names(cw)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(b)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(c)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(d)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(g)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(h)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(s)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(su)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(a)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")
names(cr)=c("H","A","ME","MS","Med","Mex","Reg","Reg*","Op","Op*","OpI","OpI*")

barplot(cw,main="MSE de Cow")
barplot(b,main="MSE de Buffsnow")
barplot(c,main="MSE de Calfem")
barplot(d,main="MSE de DowJones")
barplot(g,main="MSE de Globtp")
barplot(h,main="MSE de Huron")
barplot(s,main="MSE de Sheep")
barplot(su,main="MSE de Summer")
barplot(a,main="MSE de Ausgundeaths")
barplot(cr,main="MSE de Carga")

################################################################################
############################ Salvando as Previsões #############################

#### Salvando as previsões de Holt  ###
write.table(cow_holt,"cow_holt.txt")
write.table(buffsnow_holt,"buffsnow_holt.txt")
write.table(calfem_holt,"calfem_holt.txt")
write.table(DowJones_holt,"DowJones_holt.txt")
write.table(globtp_holt,"globtp_holt.txt")
write.table(HURON_holt,"HURON_holt.txt")
write.table(SHEEP_holt,"SHEEP_holt.txt")
write.table(summer_holt,"summer_holt.txt")
write.table(ausgundeaths_holt,"ausgundeaths_holt.txt")
write.table(Carga_holt,"Carga_holt.txt")

#### Salvando as previsões de ARIMA  ###
write.table(cow_arima,"cow_arima.txt")
write.table(buffsnow_arima,"buffsnow_arima.txt") 
write.table(calfem_arima,"calfem_arima.txt") 
write.table(DowJones_arima,"DowJones_arima.txt")
write.table(globtp_arima,"globtp_arima.txt")
write.table(HURON_arima,"HURON_arima.txt")
write.table(SHEEP_arima,"SHEEP_arima.txt")
write.table(summer_arima,"summer_arima.txt")
write.table(ausgundeaths_arima,"ausgundeaths_arima.txt")
write.table(Carga_arima,"Carga_arima.txt")

#### Salvando as previsões do Modelo Estrutural ###
write.table(cow_ME,"cow_ME.txt")
write.table(buffsnow_ME,"buffsnow_ME.txt") 
write.table(calfem_ME,"calfem_ME.txt") 
write.table(DowJones_ME,"DowJones_ME.txt")
write.table(globtp_ME,"globtp_ME.txt")
write.table(HURON_ME,"HURON_ME.txt")
write.table(SHEEP_ME,"SHEEP_ME.txt")
write.table(summer_ME,"summer_ME.txt")
write.table(ausgundeaths_ME,"ausgundeaths_ME.txt")
write.table(Carga_ME,"Carga_ME.txt")

### Salvando as previsões de Média Simples ###
write.table(cow_MS,"cow_MS.txt")
write.table(buffsnow_MS,"buffsnow_MS.txt")
write.table(calfem_MS,"calfem_MS.txt")
write.table(DowJones_MS,"DowJones_MS.txt")
write.table(globtp_MS,"globtp_MS.txt")
write.table(HURON_MS,"HURON_MS.txt")
write.table(SHEEP_MS,"SHEEP_MS.txt")
write.table(summer_MS,"summer_MS.txt")
write.table(ausgundeaths_MS,"ausgundeaths_MS.txt")
write.table(Carga_MS,"Carga_MS.txt")

### Salvando as previsões Mediana ###
write.table(cow_Med,"cow_Med.txt")
write.table(buffsnow_Med,"buffsnow_Med.txt")
write.table(calfem_Med,"calfem_Med.txt")
write.table(DowJones_Med,"DowJones_Med.txt")
write.table(globtp_Med,"globtp_Med.txt")
write.table(HURON_Med,"HURON_Med.txt")
write.table(SHEEP_Med,"SHEEP_Med.txt")
write.table(summer_Med,"summer_Med.txt")
write.table(ausgundeaths_Med,"ausgundeaths_Med.txt")
write.table(Carga_Med,"Carga_Med.txt")

### Salvando as previsões Média dos extremos ###
write.table(cow_MEx,"cow_MEx.txt")
write.table(buffsnow_MEx,"buffsnow_MEx.txt")
write.table(calfem_MEx,"calfem_MEx.txt")
write.table(DowJones_MEx,"DowJones_MEx.txt")
write.table(globtp_MEx,"globtp_MEx.txt")
write.table(HURON_MEx,"HURON_MEx.txt")
write.table(SHEEP_MEx,"SHEEP_MEx.txt")
write.table(summer_MEx,"summer_MEx.txt")
write.table(ausgundeaths_MEx,"ausgundeaths_MEx.txt")
write.table(Carga_MEx,"Carga_MEx.txt")

### Salvando as previsẽes de regressão ###
write.table(cow_R,"cow_R.txt")
write.table(buffsnow_R,"buffsnow_R.txt")
write.table(calfem_R,"calfem_R.txt")
write.table(DowJones_R,"DowJones_R.txt")
write.table(globtp_R,"globtp_R.txt")
write.table(HURON_R,"HURON_R.txt")
write.table(SHEEP_R,"SHEEP_R.txt")
write.table(summer_R,"summer_R.txt")
write.table(ausgundeaths_R,"ausgundeaths_R.txt")
write.table(Carga_R,"Carga_R.txt")

### Salvando as previsões  Optimal ###
write.table(cow_Op,"cow_Op.txt")
write.table(buffsnow_Op,"buffsnow_Op.txt")
write.table(calfem_Op,"calfem_Op.txt")
write.table(DowJones_Op,"DowJones_Op.txt")
write.table(globtp_Op,"globtp_Op.txt")
write.table(HURON_Op,"HURON_Op.txt")
write.table(SHEEP_Op,"SHEEP_Op.txt")
write.table(summer_Op,"summer_Op.txt")
write.table(ausgundeaths_Op,"ausgundeaths_Op.txt")
write.table(Carga_Op,"Carga_Op.txt")

### Salvando as previsões  Optimal com indenpendência ###
write.table(cow_OpI,"cow_OpI.txt")
write.table(buffsnow_OpI,"buffsnow_OpI.txt")
write.table(calfem_OpI,"calfem_OpI.txt")
write.table(DowJones_OpI,"DowJones_OpI.txt")
write.table(globtp_OpI,"globtp_OpI.txt")
write.table(HURON_OpI,"HURON_OpI.txt")
write.table(SHEEP_OpI,"SHEEP_OpI.txt")
write.table(summer_OpI,"summer_OpI.txt")
write.table(ausgundeaths_OpI,"ausgundeaths_OpI.txt")
write.table(Carga_OpI,"Carga_OpI.txt")

### Salvando as previsẽes de regressão reestimada ###
write.table(cow_R_r,"cow_R_res.txt")
write.table(buffsnow_R_r,"buffsnow_R_res.txt")
write.table(calfem_R_r,"calfem_R_res.txt")
write.table(DowJones_R_r,"DowJones_R_res.txt")
write.table(globtp_R_r,"globtp_R_res.txt")
write.table(HURON_R_r,"HURON_R_res.txt")
write.table(SHEEP_R_r,"SHEEP_R_res.txt")
write.table(summer_R_r,"summer_R_res.txt")
write.table(ausgundeaths_R_r,"ausgundeaths_R_res.txt")
write.table(Carga_R_r,"Carga_R_res.txt")

### Salvando as previsões  Optimal reestimada###
write.table(cow_Op_r,"cow_Op_res.txt")
write.table(buffsnow_Op_r,"buffsnow_Op_res.txt")
write.table(calfem_Op_r,"calfem_Op_res.txt")
write.table(DowJones_Op_r,"DowJones_Op_res.txt")
write.table(globtp_Op_r,"globtp_Op_res.txt")
write.table(HURON_Op_r,"HURON_Op_res.txt")
write.table(SHEEP_Op_r,"SHEEP_Op_res.txt")
write.table(summer_Op_r,"summer_Op_res.txt")
write.table(ausgundeaths_Op_r,"ausgundeaths_Op_res.txt")
write.table(Carga_Op_r,"Carga_Op_res.txt")

### Salvando as previsões  Optimal com indenpendência reestimada###
write.table(cow_OpI_r,"cow_OpI_res.txt")
write.table(buffsnow_OpI_r,"buffsnow_OpI_res.txt")
write.table(calfem_OpI_r,"calfem_OpI_res.txt")
write.table(DowJones_OpI_r,"DowJones_OpI_res.txt")
write.table(globtp_OpI_r,"globtp_OpI_res.txt")
write.table(HURON_OpI_r,"HURON_OpI_res.txt")
write.table(SHEEP_OpI_r,"SHEEP_OpI_res.txt")
write.table(summer_OpI_r,"summer_OpI_res.txt")
write.table(ausgundeaths_OpI_r,"ausgundeaths_OpI_res.txt")
write.table(Carga_OpI_r,"Carga_OpI_res.txt")