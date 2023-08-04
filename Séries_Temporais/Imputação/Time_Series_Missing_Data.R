############################################################################################################################################
########################################## Metodologias de Imputação de Dados em Séries Temporais ##########################################
###########################################################################################################################################

# Instalando pacotes necessários
install.packages("zoo")
install.packages("data.table")

# Carregando os pacotes necessários
library(zoo)
library(data.table)

# Verificando diretório de trabalho
getwd()
# Se for necessário alterar o diretório, use:
#setwd('caminho')

# Carregando os dados e tratando a coluna de data
desocup <- fread('taxa_desocupacao_brasil.csv')
desocup [, date:=as.Date(date)] # Convertendo para o formato de data do R
setkey(desocup,date) # A coluna date é definida como a chave do data.table

# Gerando um conjunto de dados onde os dados estão ausentes de forma aleatória
aa.desocup.idx <- sample(1:nrow(desocup), 0.2*nrow(desocup))
aa.desocup <- desocup[-aa.desocup.idx]

# Gerando um conjunto de dados com padrão nos dados ausentes
# Padrão é dado por: dados possuem maior probabilidade de ausência quando o desemprego é baixo
baixo.desocup.idx <- which(desocup$desocupacao<7.5)
#num.to.select <- 2*length(baixo.desocup.idx)
baixo.desocup.idx <- sample (baixo.desocup.idx,)
baixo.desocup <- desocup[-baixo.desocup.idx]

# Gerando conjunto de dados com todas as datas disponíveis, recebendo NA nas dasta ausentes
datas.completas <- seq(from = desocup$date[1], to = tail(desocup$date,1), by="months")
aa.desocup = aa.desocup[J(datas.completas), roll=0]
aa.desocup[,rpt := is.na(desocupacao)]
baixo.desocup = baixo.desocup[J(datas.completas), roll=0]
baixo.desocup[,rpt := is.na(desocupacao)]

################################################ Métodos de preenchimento de dados ausentes ################################################

## 1 - Método de preenchimento Forward fill: Transferir o último valor conhecido para o valor ausente anterior.
aa.desocup[,imputed.ff := na.locf(desocupacao, na.rm = FALSE)]
baixo.desocup[,imputed.ff := na.locf(desocupacao, na.rm = FALSE)]

## 2 - Método de preenchimento Backward fill: Transferir o próximo valor conhecido para o valor ausente anterior. !!!CUIDADO LOOKAHEAD!!!
aa.desocup[, imputed.bf := na.locf(desocupacao, na.rm = FALSE, fromLast = TRUE)] 
baixo.desocup[, imputed.bf := na.locf(desocupacao, na.rm = FALSE, fromLast = TRUE)] 

# Plotar o gráfico
desocup[50:100, plot(date, desocupacao, col = 1, lwd = 2, type = 'b')]
aa.desocup[50:100, lines(date, imputed.ff, col = 2, lwd = 2, lty = 2)]
aa.desocup[50:100][rpt == TRUE, points(date,imputed.ff, col = 2, pch = 6, cex = 2)]
aa.desocup[50:100, lines(date, imputed.bf, col = 3, lwd = 3, lty = 3)]
aa.desocup[50:100][rpt == TRUE, points(date,imputed.bf, col = 3, pch = 6, cex = 2)]
title("Gráfico de Desocupacao e Imputação usando Forward and Backward", line = 1)
legend("topleft", legend = c("desocupacao", "Forward","Backward"),col = c(1, 2,3), lty = c(1, 2,3), pch = c(1, 6), lwd = 2, cex = 0.6)
dev.copy(png, file = "grafico_desocupacao_brasil_imputado_FB.png", width = 800, height = 600)
dev.off()

## 3 - Método de preenchimento Média Móvel: médias ou mediana dos últimos n valores conhecidos
# Média móvel de janela com tamanho 3
aa.desocup[,imputed.mm.nolookah := rollapply(c(NA,NA, desocupacao),3,
                                               function(x){
                                                 if (!is.na(x[3])) x[3] else mean(x,na.rm = TRUE)
                                               })]
baixo.desocup[,imputed.mm.nolookah := rollapply(c(NA,NA, desocupacao),3,
                                               function(x){
                                                 if (!is.na(x[3])) x[3] else mean(x,na.rm = TRUE)
                                               })]

## 4 - Método de preenchimento Média Móvel: médias ou mediana de n valores (passados e futuros) -   !!!!!CUIDADO LOOKAHEAD!!!!!
# Média móvel de janela com tamanho 3
aa.desocup[, imputed.mm.lookah := rollapply(c(NA, desocupacao, NA), 3,
                                                function(x) {
                                                  if (!is.na(x[1])) x[1] else mean(x, na.rm = TRUE)
                                                })]         
baixo.desocup[, imputed.mm.lookah := rollapply(c(NA, desocupacao, NA), 3,
                                                function(x) {
                                                  if (!is.na(x[1])) x[1] else mean(x, na.rm = TRUE)
                                                })]  

# Plotar o gráfico
desocup[50:100, plot(date, desocupacao, col = 1, lwd = 2, type = 'b')]
aa.desocup[50:100, lines(date, imputed.mm.nolookah, col = 2, lwd = 2, lty = 2)]
aa.desocup[50:100][rpt == TRUE, points(date,imputed.mm.nolookah, col = 2, pch = 6, cex = 2)]
aa.desocup[50:100, lines(date, imputed.mm.lookah, col = 3, lwd = 2, lty = 2)]
aa.desocup[50:100][rpt == TRUE, points(date,imputed.mm.lookah, col = 3, pch = 6, cex = 2)]
title("Gráfico de desocupacao e Imputação Média Móvel", line = 1)
legend("topleft", legend = c("desocupacao", "Sem Lookahead","Com Lookahead"),col = c(1, 2,3), lty = c(1, 2,3), pch = c(1, 6), lwd = 1, cex = 0.6)
dev.copy(png, file = "grafico_desocupacao_brasil_imputado_MM.png", width = 800, height = 600)
dev.off()

#OBS: 
# Uma imputação de dados por média móvel reduz a variância no conjunto de dados e consequentemente pode-se superestimar o desempenho do
# modelo. Usar a média geral do conjunto de dados não é recomendada para imputação em séries temporais, pois isso implica em conhecer o 
# futuro, um LOOKAHEAD.

## 5 - Método de preenchimeto Interpolação
# Interpolação é um método para encontrar os valores dos pontos de dados ausentes com base em restrições geométricas sobre como
# queremos que os dados gerais se comportem. Como exemplo, podemos citar a interpolação linear e polinomial

# Interpolação linear usando dados passados e dados futuros - !!!CUIDADO LOOKAHEAD!!!
aa.desocup[,imputed.li := na.approx(desocupacao)]
baixo.desocup[,imputed.li := na.approx(desocupacao)]

# Interpolação Polinomial usando dados passados e dados futuros - !!!CUIDADO LOOKAHEAD!!!
aa.desocup[,imputed.sp := na.spline(desocupacao)]
baixo.desocup[,imputed.sp := na.spline(desocupacao)]

# Plotar o gráfico
desocup[1:30, plot(date, desocupacao, col = 1, lwd = 2, type = 'b')]
aa.desocup[1:30, lines(date, imputed.li, col = 2, lwd = 2, lty = 2)]
aa.desocup[1:30][rpt == TRUE, points(date,imputed.li, col = 2, pch = 6, cex = 2)]
aa.desocup[1:30, lines(date, imputed.sp, col = 3, lwd = 3, lty = 3)]
aa.desocup[1:30][rpt == TRUE, points(date,imputed.sp, col = 3, pch = 6, cex = 2)]
title("Gráfico de desocupacao e Interpolação Linerar e Polinomial", line = 1)
legend("bottomleft", legend = c("desocupacao", "Linear","Polinomial"),col = c(1, 2,3), lty = c(1, 2,3), pch = c(1, 6), lwd = 2, cex = 0.8)
dev.copy(png, file = "grafico_desocupacao_brasil_imputado_interpolacao.png", width = 800, height = 600)
dev.off()

########################################## Avaliando os diferentes tipos de imputação que fizemos ##########################################
aa.desocup[ , lapply(.SD, function(x) mean((x - desocup$desocupacao)^2,
                                           na.rm = TRUE)),
            .SDcols = c("imputed.ff","imputed.bf",
                        "imputed.mm.nolookah",
                        "imputed.li","imputed.sp")]
#Saída de quando rodei - pode dar diferente ao executar novamente pois os dados ausentes são gerados aleatoriamente.
#   imputed.ff imputed.bf imputed.mm.nolookah  imputed.li  imputed.sp
#1: 0.03264706 0.05514706          0.03475926 0.004760008 0.004797156

baixo.desocup[ , lapply(.SD, function(x) mean((x - desocup$desocupacao)^2,
                                           na.rm = TRUE)),
            .SDcols = c("imputed.ff","imputed.bf",
                        "imputed.mm.nolookah",
                        "imputed.li","imputed.sp")]
#Saída
#imputed.ff imputed.bf imputed.mm.nolookah imputed.li imputed.sp
#1: 0.07404412 0.08904412         0.002717391 0.08119748  0.1469166