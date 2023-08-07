#Modelando a taxa de desocupaçãodos jovens 15-29 de Minas Gerais

#LENDO OS DADOS
dados<-read.csv("Dados/Taxa_de_desocupação_Jovens_15_29_MG.csv", sep=";") 

str(dados)

dados$Sexo<-as.factor(dados$Sexo)
dados$Raca<-as.factor(dados$Raca)
dados$N_Instrucao<-as.factor(dados$N_Instrucao)
dados$Idade_Cat<-as.factor(dados$Idade_Cat)
dados$Taxa=dados$Desocupados/dados$Total

str(dados)

attach(dados)

#Análise descritiva
par(mfrow=c(1,4))
plot(Taxa~Sexo, main="Taxa de desocupação por Sexo", names=c("Masculino","Feminino"))
plot(Raca,Taxa, main="Taxa de desocupação por Raça",names=c("Brancos","Pretos e Pardos"))
plot(N_Instrucao,Taxa, main="Taxa de desocupação por Nível de Instrução")
plot(Idade_Cat,Taxa, main="Taxa de desocupação por Idade",names=c("15-17","18-24","25-29"))

#Modelo de Regressão Poisson

#1) modelo só  intercepto

modelo.constante<-glm(Desocupados ~ 1, data=dados, family=poisson) 
summary(modelo.constante) #Par?metro significativo
#Ajuste
D=modelo.constante$deviance
df=modelo.constante$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.constante, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.constante$aic
M1=cbind(D,df,p,X2,pX2,AIC)
M1 #Não validade, AIC enorme

rm(D,df,p,X2,pX2,AIC)

#Modelo offset
#M2
modelo.offset<-glm(Desocupados ~offset(log(Total)), family=poisson, data=dados)
summary(modelo.offset) #Parâmetro Significativo
#Ajuste
D=modelo.offset$deviance
df=modelo.offset$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.offset, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.offset$aic
M2=cbind(D,df,p,X2,pX2,AIC)
M2 #Não validado

rm(D,df,p,X2,pX2,AIC)

#3) modelo sexo
modelo.Sexo<-glm( Desocupados ~ offset(log(Total)) + Sexo, data=dados, family=poisson)
summary(modelo.Sexo) #Parâmetros Significativo
#Ajuste
D=modelo.Sexo$deviance
df=modelo.Sexo$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.Sexo, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.Sexo$aic
M3=cbind(D,df,p,X2,pX2,AIC)
M3 #Não validado

rm(D,df,p,X2,pX2,AIC)

#4) Modelo Raça
modelo.Raca<-glm( Desocupados ~ offset(log(Total)) + Raca, data=dados, family=poisson)
summary(modelo.Raca) # Parâmetro Significativo
#Ajuste
D=modelo.Raca$deviance
df=modelo.Raca$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.Raca, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.Ra?a$aic
M4=cbind(D,df,p,X2,pX2,AIC)
M4 #Não validado

rm(D,df,p,X2,pX2,AIC)


#5) modelo Idade
modelo.Idade<-glm( Desocupados ~ offset(log(Total)) + Idade_Cat, data=dados, family=poisson)
summary(modelo.Idade)#Parâmtros significativos
#Ajuste
D=modelo.Idade$deviance
df=modelo.Idade$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.Idade, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.Idade$aic
M5=cbind(D,df,p,X2,pX2,AIC)
M5 #Não validado

rm(D,df,p,X2,pX2,AIC)

#6) modelo Nível de Instrução
modelo.Inst<-glm( Desocupados ~ offset(log(Total)) + N_Instrucao, data=dados, family=poisson)
summary(modelo.Inst)#Significância apenas para o nível sete e intercepto
#Ajuste
D=modelo.Inst$deviance
df=modelo.Inst$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.Inst, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.Inst$aic
M6=cbind(D,df,p,X2,pX2,AIC)
M6 #Não validado

rm(D,df,p,X2,pX2,AIC)

#7) modelo Sexo + Raça
modelo.sexo_raca<-glm( Desocupados ~ offset(log(Total)) + Sexo+Raca, data=dados, family=poisson)
summary(modelo.sexo_raca)#Parâmetros significativos
#Ajuste
D=modelo.sexo_raca$deviance
df=modelo.sexo_raca$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.sexo_raca, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.sexo_raca$aic
M7=cbind(D,df,p,X2,pX2,AIC)
M7 #Não validado

rm(D,df,p,X2,pX2,AIC)


#8) modelo Sexo+Idade
modelo.sexo_id<-glm( Desocupados ~ offset(log(Total)) + Sexo+Idade_Cat, data=dados, family=poisson)
summary(modelo.sexo_id) #Parâmetros significativos
D=modelo.sexo_id$deviance
df=modelo.sexo_id$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.sexo_id, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.sexo_id$aic
M8=cbind(D,df,p,X2,pX2,AIC)
M8 #Não validado

rm(D,df,p,X2,pX2,AIC)

#9) modelo Sexo+Instrução
modelo.sexo_inst<-glm( Desocupados ~ offset(log(Total)) + Sexo+N_Instrucao, data=dados, family=poisson)
summary(modelo.sexo_inst)# Significância apenas de intercepto, sexo e inst 5 e 7
D=modelo.sexo_inst$deviance
df=modelo.sexo_inst$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.sexo_inst, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.sexo_inst$aic
M9=cbind(D,df,p,X2,pX2,AIC)
M9 #Não validado

rm(D,df,p,X2,pX2,AIC)

#10) modelo Raça + Idade
modelo.raca_id<-glm( Desocupados ~ offset(log(Total)) + Raca+Idade_Cat, data=dados, family=poisson)
summary(modelo.raca_id) #Todos significativos
D=modelo.raca_id$deviance
df=modelo.raca_id$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.raca_id, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.raca_id$aic
M10=cbind(D,df,p,X2,pX2,AIC)
M10 #Não validado

rm(D,df,p,X2,pX2,AIC)

#11) modelo Raça + N_Instrução
modelo.raca_inst<-glm( Desocupados ~ offset(log(Total)) + Raca+N_Instrucao, data=dados, family=poisson)
summary(modelo.raca_inst) #Significância apenas do intercepto, sexo e inst 7
D=modelo.raca_inst$deviance
df=modelo.raca_inst$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.raca_inst, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.raca_inst$aic
M11=cbind(D,df,p,X2,pX2,AIC)
M11 #N?o validado

rm(D,df,p,X2,pX2,AIC)

#12) modelo Sexo + Raça + Idade_Cat 
modelo.sexo_raca_id<-glm( Desocupados ~ offset(log(Total)) + Sexo+Raca+Idade_Cat, data=dados, family=poisson)
summary(modelo.sexo_raca_id) #Todos os parâmetros significativos
D=modelo.sexo_raca_id$deviance
df=modelo.sexo_raca_id$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.sexo_raca_id, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.sexo_raca_id$aic
M12=cbind(D,df,p,X2,pX2,AIC)
M12 #MODELO VALIDADO!!!

rm(D,df,p,X2,pX2,AIC)

#13) modelo Sexo + Raça + N_Instrução
modelo.sexo_raca_inst<-glm( Desocupados ~ offset(log(Total)) + Sexo+Raca+N_Instrucao, data=dados, family=poisson)
summary(modelo.sexo_raca_inst) #Todos os parâmetros significativos (Inst apenas nível sete)
D=modelo.sexo_raca_inst$deviance
df=modelo.sexo_raca_inst$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.sexo_raca_inst, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.sexo_raca_inst$aic
M13=cbind(D,df,p,X2,pX2,AIC)
M13 #MODELO não VALIDADO!!!

rm(D,df,p,X2,pX2,AIC)

#14) modelo Sexo + Raça + Idade_Cat +N_Intrução
modelo.sexo_raca_id_inst<-glm( Desocupados ~ offset(log(Total)) + Sexo+Raca+Idade_Cat+N_Instrucao, data=dados, family=poisson)
summary(modelo.sexo_raca_id_inst)# Não significância para N_Instrução, exceto nível sete.
D=modelo.sexo_raca_id_inst$deviance
df=modelo.sexo_raca_id_inst$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.sexo_raca_id_inst, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.sexo_raca_id_inst$aic
M14=cbind(D,df,p,X2,pX2,AIC)
M14 #Modelo validado

rm(D,df,p,X2,pX2,AIC)

#Comparando os modelos validados, ou seja, os modelos M12 e M14
Dif= modelo.sexo_raca_id$deviance-modelo.sexo_raca_id_inst$deviance
df= modelo.sexo_raca_id$df.residual-modelo.sexo_raca_id_inst$df.residual
p=1-pchisq(Dif,df)
Dif_D=cbind("M12 vs M14",Dif,df,p)
Dif_D #Rejeita H0, portanto a inclusão da variável N_Instrucao é sifnigicativa

rm(Dif,df,p)

anova(modelo.sexo_raca_id,modelo.sexo_raca_id_inst,test="Chisq") #Apenas confirmando o cálculo anterior
anova(modelo.sexo_raca_id_inst,test="Chisq")

#Ficamos com o modelo 14, agora é colocar interações.

#M15 Interação Sexo * Raça

modelo.comp.SR<-glm( Desocupados ~ offset(log(Total)) + Sexo*Raca+Idade_Cat+N_Instrucao, data=dados, family=poisson)
summary(modelo.comp.SR)# Não significância para N_Instrucao, exceto nível sete e Interação
D=modelo.comp.SR$deviance
df=modelo.comp.SR$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.comp.SR, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.comp.SR$aic
M15=cbind(D,df,p,X2,pX2,AIC)
M15#Modelo validado

rm(D,df,p,X2,pX2,AIC)

Dif= modelo.sexo_raca_id_inst$deviance-modelo.comp.SR$deviance
df= modelo.sexo_raca_id_inst$df.residual-modelo.comp.SR$df.residual
p=1-pchisq(Dif,df)
Dif=cbind("M14 vs M15",Dif,df,p)
Dif_D=rbind(Dif_D,Dif) #Não Rejeita H0, portanto a inclusão da INTERAÇÃO não é sifnigicativa
Dif_D

rm(Dif,df,p)

anova(modelo.sexo_raca_id_inst,modelo.comp.SR,test="Chisq") #Apenas confirmando o cálculo anterior
anova(modelo.comp.SR,test="Chisq")


#M16 Interação Sexo * Idade

modelo.comp.SI<-glm( Desocupados ~ offset(log(Total)) + Raca+Sexo*Idade_Cat+N_Instrucao, data=dados, family=poisson)
summary(modelo.comp.SI)# Não significância para N_Instrucao, exceto nível sete e Interação
D=modelo.comp.SI$deviance
df=modelo.comp.SI$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.comp.SI, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.comp.SI$aic
M16=cbind(D,df,p,X2,pX2,AIC)
M16#Modelo validado

rm(D,df,p,X2,pX2,AIC)

Dif= modelo.sexo_raca_id_inst$deviance-modelo.comp.SI$deviance
df= modelo.sexo_raca_id_inst$df.residual-modelo.comp.SI$df.residual
p=1-pchisq(Dif,df)
Dif=cbind("M14 vs M16",Dif,df,p)
Dif_D=rbind(Dif_D,Dif) #Não Rejeita H0, portanto a inclusão da INTERAÇÃO não é sifnigicativa
Dif_D

rm(Dif,df,p)

anova(modelo.sexo_raca_id_inst,modelo.comp.SI,test="Chisq") #Apenas confirmando o cálculo anterior
anova(modelo.comp.SI,test="Chisq")

#M17 Interação Sexo * Nível de Instrução

modelo.comp.SN<-glm( Desocupados ~ offset(log(Total)) + Raca+Idade_Cat+Sexo*N_Instrucao, data=dados, family=poisson)
summary(modelo.comp.SN)# Apenas idade e  raça significativos
D=modelo.comp.SN$deviance
df=modelo.comp.SN$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.comp.SN, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.comp.SN$aic
M17=cbind(D,df,p,X2,pX2,AIC)
M17#Modelo validado

rm(D,df,p,X2,pX2,AIC)

Dif= modelo.sexo_raca_id_inst$deviance-modelo.comp.SN$deviance
df= modelo.sexo_raca_id_inst$df.residual-modelo.comp.SN$df.residual
p=1-pchisq(Dif,df)
Dif=cbind("M14 vs M17",Dif,df,p)
Dif_D=rbind(Dif_D,Dif) #Não Rejeita H0, portanto a inclusão da INTERAÇÃO não é sifnigicativa
Dif_D

rm(Dif,df,p)

anova(modelo.sexo_raca_id_inst,modelo.comp.SN,test="Chisq") #Apenas confirmando o cálculo anterior
anova(modelo.comp.SN,test="Chisq")

#M18 Interação Raça * Idade

modelo.comp.RI<-glm( Desocupados ~ offset(log(Total)) + Sexo+Raca*Idade_Cat+N_Instrucao, data=dados, family=poisson)
summary(modelo.comp.RI)# #INTERAÇÃO SEM SIGNFICÂNCIA
D=modelo.comp.RI$deviance
df=modelo.comp.RI$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.comp.RI, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.comp.RI$aic
M18=cbind(D,df,p,X2,pX2,AIC)
M18#Modelo validado

rm(D,df,p,X2,pX2,AIC)

Dif= modelo.sexo_raca_id_inst$deviance-modelo.comp.RI$deviance
df= modelo.sexo_raca_id_inst$df.residual-modelo.comp.RI$df.residual
p=1-pchisq(Dif,df)
Dif=cbind("M14 vs M18",Dif,df,p)
Dif_D=rbind(Dif_D,Dif) #Não Rejeita H0, portanto a inclusão da INTERAÇÃO não é sifnigicativa
Dif_D

rm(Dif,df,p)

anova(modelo.sexo_raca_id_inst,modelo.comp.RI,test="Chisq") #Apenas confirmando o cálculo anterior
anova(modelo.comp.RI,test="Chisq")

#M19 Interação Raça * n_intrução

modelo.comp.RINST<-glm( Desocupados ~ offset(log(Total)) + Sexo+Idade_Cat+Raca*N_Instrucao, data=dados, family=poisson)
summary(modelo.comp.RINST)# #INTERAÇÃO SEM SIGNFICÂNCIA E RAÇA TBM
D=modelo.comp.RINST$deviance
df=modelo.comp.RINST$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.comp.RINST, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.comp.RINST$aic
M19=cbind(D,df,p,X2,pX2,AIC)
M19#Modelo validado

rm(D,df,p,X2,pX2,AIC)

Dif= modelo.sexo_raca_id_inst$deviance-modelo.comp.RINST$deviance
df= modelo.sexo_raca_id_inst$df.residual-modelo.comp.RINST$df.residual
p=1-pchisq(Dif,df)
Dif=cbind("M14 vs M19",Dif,df,p)
Dif_D=rbind(Dif_D,Dif) #Não Rejeita H0, portanto a inclusão da INTERAÇÃO não é sifnigicativa
Dif_D

rm(Dif,df,p)

anova(modelo.sexo_raca_id_inst,modelo.comp.RINST,test="Chisq") #Apenas confirmando o cálculo anterior
anova(modelo.comp.RINST,test="Chisq")

#M120 Interação IDADE * n_INSTRUÇÃO

modelo.comp.IDINST<-glm( Desocupados ~ offset(log(Total)) + Sexo+Raca+Idade_Cat*N_Instrucao, data=dados, family=poisson)
summary(modelo.comp.IDINST)# #INTERAÇÃO SEM SIGNFICÂNCIA E RAÇA TBM
D=modelo.comp.IDINST$deviance
df=modelo.comp.IDINST$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.comp.IDINST, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.comp.IDINST$aic
M20=cbind(D,df,p,X2,pX2,AIC)
M20#Modelo validado

rm(D,df,p,X2,pX2,AIC)

Dif= modelo.sexo_raca_id_inst$deviance-modelo.comp.IDINST$deviance
df= modelo.sexo_raca_id_inst$df.residual-modelo.comp.IDINST$df.residual
p=1-pchisq(Dif,df)
Dif=cbind("M14 vs M20",Dif,df,p)
Dif_D=rbind(Dif_D,Dif) #Não Rejeita H0, portanto a inclusão da INTERAÇÃO não é sifnigicativa
Dif_D

rm(Dif,df,p)

anova(modelo.sexo_raca_id_inst,modelo.comp.IDINST,test="Chisq") #Apenas confirmando o cálculo anterior
anova(modelo.comp.IDINST,test="Chisq")

#M21 Interação SEXO*IDADE*RAÇA

modelo.comp.SRI<-glm( Desocupados ~ offset(log(Total)) + Sexo*Raca*Idade_Cat+N_Instrucao, data=dados, family=poisson)
summary(modelo.comp.SRI)# #INTERAÇÃO SEM SIGNFICÂNCIA 
D=modelo.comp.SRI$deviance
df=modelo.comp.SRI$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.comp.SRI, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.comp.SRI$aic
M21=cbind(D,df,p,X2,pX2,AIC)
M21#Modelo validado

rm(D,df,p,X2,pX2,AIC)

Dif= modelo.sexo_raca_id_inst$deviance-modelo.comp.SRI$deviance
df= modelo.sexo_raca_id_inst$df.residual-modelo.comp.SRI$df.residual
p=1-pchisq(Dif,df)
Dif=cbind("M14 vs M21",Dif,df,p)
Dif_D=rbind(Dif_D,Dif) #Não Rejeita H0, portanto a inclusão da INTERAÇÃO não é sifnigicativa
Dif_D

rm(Dif,df,p)

anova(modelo.sexo_raca_id_inst,modelo.comp.SRI,test="Chisq") #Apenas confirmando o cálculo anterior
anova(modelo.comp.SRI,test="Chisq")

#Salvando os resultados

RESULTADOS=rbind(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16,M17,M18,M19,M20,M21)
row.names(RESULTADOS)=c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12","M13",
                        "M14","M15","M16","M17","M18","M19","M20","M21")

RESULTADOS
write.csv2(RESULTADOS,"Resultados/RESULTADOS AJUSTE.csv")
Dif_D
write.csv2(Dif_D,"Resultados/Comparações Deviance.csv")

#Modelo Saturado
#XX) modelo Sexo*Raca*Idade_Cat*N_Intrucao
modelo.sat<-glm( Desocupados ~ offset(log(Total)) + Sexo*Raca*Idade_Cat*N_Instrucao, data=dados, family=poisson)
summary(modelo.sat)
D=modelo.sat$deviance
df=modelo.sat$df.residual
p=1-pchisq(D,df)
X2=sum(residuals(modelo.sat, type="pearson")^2)
pX2=1-pchisq(X2,df)
AIC=modelo.sat$aic
MXX=cbind(D,df,p,X2,pX2,AIC)
MXX

#AJUSTE DO MODELO COMPLETO ADITIVO - OK

#modelo escolhido até aqui: modelo completo aditivo CAR + AGE + DIST
summary(modelo.sexo_raca_id_inst)
anova(modelo.sexo_raca_id_inst, test="Chisq")
# Não rejeitamos a hipótese de que o modelo possui um ajuste semelhante ao ajuste do modelo saturado

#valores esperados
dados$ajust=fitted(modelo.sexo_raca_id_inst)

#Y/N esperadas
dados$Tx_ajus=dados$ajust/dados$Total
dados_aj=dados
write.csv2(dados_aj,"Resultados/dados_aj.csv")

#FAZER NO EXCEL GRÁFICO DA Y/N ESPERADA X OBSERVADA

#Residuos de Pearson
round(matrix(residuals(modelo.sexo_raca_id_inst, type="pearson"), ncol=4, byrow=F),8)

model.modelo.sexo_raca_id_inst<-lm.influence(modelo.sexo_raca_id_inst)$hat
residuos<-residuals(modelo.sexo_raca_id_inst, type="pearson")
residuos.adj<-residuos/sqrt(1-model.modelo.sexo_raca_id_inst)
round(matrix(residuos.adj, ncol=4, byrow=F),8)

#Gráfico dos resíduos ajustados

par(mfrow=c(1,1))

plot(residuos.adj, main="Resíduos ajustados")

plot(dados$Sexo,dados$Taxa)

Coef_Ad=data.frame(Coeficientes=modelo.sexo_raca_id_inst$coeff, OR=c(round(exp(modelo.sexo_raca_id_inst$coef),3))
      ,OR_Efeito=c(round(100*(exp(modelo.sexo_raca_id_inst$coef)-1),2)))
write.csv2(Coef_Ad,"Resultados/Coef_Ad.csv")

attach(dados)
par(mfrow=c(1,4))
plot(Tx_ajus~Sexo, main="Taxa de desocupação por Sexo", names=c("Masculino","Feminino"))
plot(Raca,Tx_ajus, main="Taxa de desocupação por Ra?a",names=c("Brancos","Pretos e Pardos"))
plot(N_Instrucao,Tx_ajus, main="Taxa de desocupação por Nível de Instrução")
plot(Idade_Cat,Tx_ajus, main="Taxa de desocupação por Idade",names=c("15-17","18-24","25-29"))