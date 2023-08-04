# Análise de Dados com Linguagem R

<div align="center">
  <img src="header_tcc.png" alt="header" >
</div>
<br/>

-   [1. Descrição](#1-descrição)
-   [2. Organização do Diretório](#2-organização-do-diretório)
-   [3. Referências](#3-referências)

# 1. Descrição

Neste projeto, revisitei meu código R das análises da minha monografia do curso de [Bacharel em Estatística da Universidade Federal de Juiz de Fora (UFJF)](https://www.ufjf.br/cursoestatistica/), intitulada <i>**Um Estudo Empı́rico do Desempenho de Combinações de Previsões**</i>. Concluí o curso em 2011, assim, depois de mais de 11 anos foram necessários alguns pequenos ajustes do código original (dado as atualizações das bibliotecas do R que utilizei) para que o mesmo funcionasse. Fiquei surpresa, pois realmente foram poucos ajustes, mas também me surprendeu o quanto o código pode ser otimizado. Não omimizei neste primeiro momento, mas assim que possível pretendo fazê-lo.

A combinação de previsões é um procedimento que tenta melhorar a precisão das previsões, que aproveita a disponibilidade de vários métodos de previsão individual. **Este trabalho teve como objetivo aplicar os conhecimentos aprendidos durante o curso e fazer um estudo sobre o desempenho de diferentes combinações de previsões**. Realizou-se uma comparação entre métodos de previsão individuais e métodos de combinação, escolhendo para isso apenas uma medida de desempenho, o erro quadrático médio (MSE). As previsões individuais foram feitas pelo método de Amortecimento Exponencial de Holt, por modelos ARIMA e por modelos estruturais de tendência local. Realizou-se seis tipos de combinação: média simples, mediana, média dos extremos, regressão, combinação ótima e combinação ótima com independência. Para verificar o melhor desempenho das combinações de previsões, ajustou-se diversos métodos em dez séries reais, estacionárias e não-sazonais.

Neste trabalho utilizou-se a linguagem R e os pacotes [TSA](https://cran.r-project.org/web/packages/TSA/index.html) que é útil para análise de séries temporais[^1] , [nortest](https://cran.r-project.org/web/packages/nortest/index.html) que é útil para testes de normalidade[^2] e [car](https://cran.r-project.org/web/packages/car/index.html) que é útil para análise de regressão[^3].

Como conclusão verificou-se que as previsões pelo método ARIMA apresentaram um melhor desempenho em 60% das séries estudadas quando comparado aos modelos de previsão individual e em 70% das séries pelo menos um modelo de previsão de combinação se mostrou mais eficiente que as previsões individuais. Portando, combinar as diversidades de métodos de previsão individuais garante efetivamente uma melhor precisão das previsões e a combinação por regressão mostrou melhor desempenho que as outras combinações.

<blockquote style="background-color: salmon; padding: 10px; color: black;">
Após a aprovação do trabalho pela banca de professores da UFJF, publiquei um <a href="https://simpep.feb.unesp.br/abrir_arquivo_pdf.php?tipo=artigo&evento=10&art=266&cad=22556&opcao=com_id">resumo</a> no ANAIS do XXII Simpósio de Engenharia de Produção (<a href="https://simpep.feb.unesp.br/index.php">SIMPEP</a>), e posteriormente, em 2020, um artigo no <a href="https://www.brazilianjournals.com/index.php/BJB/article/view/13472">Brazilian Journal of Business</a>.
</blockquote><br/>

[^1]: O pacote "TSA" é um pacote fundamental para análise de séries temporais no R. Ele fornece várias funções para modelar, analisar e prever dados temporais. Com o "TSA", os usuários podem ajustar modelos ARIMA (AutoRegressive Integrated Moving Average), fazer decomposição de séries temporais, realizar testes estatísticos para estacionariedade, entre outras análises relacionadas a séries temporais.<br/>
[^2]: O pacote "nortest" é voltado para testes de normalidade de dados. Ele oferece uma variedade de testes estatísticos que permitem verificar se uma amostra de dados segue uma distribuição normal ou não. Esses testes são úteis para garantir a validade de pressupostos em muitas técnicas estatísticas, como análise de variância (ANOVA) e regressão, que pressupõem normalidade dos resíduos.<br/>
[^3]: O pacote "car" (Companion to Applied Regression) é uma ferramenta útil para realizar análises de regressão no R. Ele fornece funções que auxiliam na criação de modelos de regressão, além de oferecer ferramentas para diagnóstico de pressupostos e tratamento de outliers. O "car" é uma extensão muito utilizada junto com o pacote "lm" (linear models) para aprimorar a análise de regressão.<br/>

# 2. Organização do Diretório

Neste diretório os arquivos estão organizados da seguinte forma:

:file_folder: [Dados](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/tree/main/Series_Temporais/TCC_Graduacao/Dados) : Pasta com as 10 séries originais usadas neste trabalho

:file_folder: [Graficos](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/tree/main/Series_Temporais/TCC_Graduacao/Graficos) : Pasta com todos os gráficos gerados na análise

:file_folder: [Previsoes](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/tree/main/Series_Temporais/TCC_Graduacao/Previsoes) : Pasta com todas as previsões geradas na análise

:page_facing_up: [Artigo_Brazilian_Journal_Business.pdf](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/blob/main/Series_Temporais/TCC_Graduacao/Artigo_Brazilian_Journal_Business.pdf): Artigo publicado no Brazilian Journal of Business

:page_facing_up: [Monografia_Leiliane.pdf](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/blob/main/Series_Temporais/TCC_Graduacao/Monografia_Leiliane.pdf): Trabalho de Conclusão de Curso Completo

:page_facing_up: [Monografia_Leilane_Apt.pdf](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/blob/main/Series_Temporais/TCC_Graduacao/Monografia_Leilane_Apt.pdf): Apresentação do TCC na Defesa de Monografia

:page_facing_up: [XXII_SIMPEP_Art_266.pdf](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/blob/main/Series_Temporais/TCC_Graduacao/XXII_SIMPEP_Art_266.pdf): Resumo publicado no SIMPEP

:page_facing_up: [Monografia.r](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/blob/main/Series_Temporais/TCC_Graduacao/Monografia.r): Script em R

:page_facing_up: [OutputR_Monografia.txt](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/blob/main/Series_Temporais/TCC_Graduacao/OutputR_Monografia.txt): Output do R

:chart_with_downwards_trend: [Tab_Erros.txt](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/blob/main/Series_Temporais/TCC_Graduacao/Tab_Erros.txt): Tabela com os MSE dos modelos de previsão analisados

# 3. Referências

**BOX**, G.E.P; **JENKINS**, G.M.; **REINSEL**, G.C. <i>Time series analysis: forecasting and control<i/>, 4.ed. New Jersey: Prentice Hall, 2008.

**CLEMEN**, R.T. <i>Combining forecasts: a review and annotated bibliography<i/>. Int J Forecasting, n.5, 559-583, 1989.

**MONTGOMERY**, D. C.; **JOHNSON**, L. A.; **GARDNER**, J. S. <i>Forecasting and time series analysis</i>. 2.ed. NewYork: McGraw-Hill, 1990.

**MORETTIN**, P.A.; **TOLOI**, C.M. <i>Análise de Séries Temporais</i>. 2.ed. São Paulo: Edgard Blucher, 2006.

**OLIVEIRA**, LS; **HIPPERT**, HS.<i> Um estudo empírico do desempenho de combinações de previsões<i/>. 2011. Monografia - Universidade Federal de Juiz de Fora, Juiz de Fora, 2011.
