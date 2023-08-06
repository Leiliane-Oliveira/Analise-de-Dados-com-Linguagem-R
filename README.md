# Análise de Dados com Linguagem R

Repositório com projetos que vão desde análise de dados à machine learning com R.

<div align="center">
  <img src="header.png" alt="header">
</div>

-   [1. Introdução](#1-introdução)
-   [2. Estrutura de pastas](#2-estrutura-de-pastas)
-   [3. Projetos](#3-projetos)
    -   [3.1. Séries Temporais](#31-séries-temporais)
        -   [3.1.1. TCC Graduação](#311-tcc-graduação)
        -   [3.1.2. Metodologias de Imputação de Dados](#312-metodologias-de-imputação-de-dados)
    -   [3.2. Inferência Estatística](#32-inferência-estatística)
        -   [3.2.1. Teorema do Limite Central](#321-teorema-do-limite-central)
-   [4. Referências](#4-referências)

# 1. Introdução

É com imenso prazer que lhe dou as boas-vindas a este espaço em constante evolução, onde busco compartilhar minha jornada no mundo da análise de dados e da programação em R.

Este repositório foi criado com o objetivo de ser um local que reflete meu crescimento e aprendizado ao longo do tempo. Inicialmente, você encontrará aqui o resultado de meu esforço e dedicação durante a graduação em Estatística: meu Trabalho de Conclusão de Curso (TCC), que espero que seja uma amostra das habilidades desenvolvidas durante essa fase acadêmica.

Mas o verdadeiro diferencial deste espaço está na sua constante mutação. Pretendo preenchê-lo com uma variedade de projetos de análise de dados em R, abordando desde tarefas simples até desafios mais complexos. Cada projeto será uma oportunidade para aplicar minhas habilidades em estatística, visualização de dados, modelagem e até mesmo machine learning, além de explorar diversas bibliotecas e técnicas disponíveis em R.

Espero que ao acompanhar este repositório, você também possa aprender e se inspirar com meus estudos e progresso. Como uma jornada de aprendizado nunca termina, estou sempre em busca de novos conhecimentos e ferramentas para aprimorar minhas capacidades. Sendo assim, a cada nova biblioteca aprendida, a cada nova técnica dominada, estarei prontamente atualizando este espaço para compartilhar com você meu crescimento contínuo.

Fique à vontade para explorar os projetos disponíveis, verificar meu código-fonte e até mesmo sugerir melhorias ou projetos futuros. Estou interessada em feedbacks construtivos, pois acredito que é por meio da colaboração e do compartilhamento de conhecimento que podemos crescer ainda mais como profissionais e como comunidade.

Obrigada por estar aqui e por me acompanhar nessa jornada de aprendizado e aprimoramento. Juntos, poderemos construir um repositório rico em informações e contribuir para um mundo mais analítico e informado.

Nas seções abaixo você encontrará como as pastas deste repositório estão organizadas e em seguida um breve resumo de cada projeto disponibilizado aqui.

Vamos começar essa jornada!

# 2. Estrutura de pastas

```markdown{
Projetos de Análise de Dados com R
├── Series_Temporais
│   ├── TCC_Graduacao
│   ├── Imputacao
├── Inferencia_Estatistica
|   ├── TLC
```

# 3. Projetos

## 3.1. Séries Temporais

Uma série temporal é uma sequência de dados observados ao longo do tempo, onde cada observação é registrada em um ponto específico no tempo. Essas observações podem ser coletadas em intervalos regulares (por exemplo, a cada hora, dia, mês) ou irregulares, dependendo do contexto e do problema em questão.

As séries temporais são amplamente utilizadas em diversas áreas, como economia, meteorologia, finanças, ciências sociais e engenharia, entre outras. Elas podem conter informações sobre o comportamento e padrões que evoluem ao longo do tempo, como tendências, sazonalidades, ciclos e ruído aleatório.

A análise de séries temporais tem o objetivo de entender e modelar esses padrões para prever valores futuros, identificar tendências ou detectar anomalias. Ela é uma área de estudo importante no campo da estatística, aprendizado de máquina e ciência de dados, e tem uma ampla variedade de aplicações, desde previsão de vendas e demanda de produtos até monitoramento de sistemas complexos e previsão do tempo.

Nos projetos de Séries Temporais tenho como objetivo abordar desde o tratamento dos dados para análises de séries temporais à modelos de previsão para séries temporais, incluindo meu Trabaho de Conclusão de Curso da graduação realizado em 2011.

### 3.1.1. TCC Graduação

[Neste projeto](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/tree/main/Series_Temporais/TCC_Graduacao), revisitei meu código R das análises da minha monografia do curso de [Bacharel em Estatística da Universidade Federal de Juiz de Fora (UFJF)](https://www.ufjf.br/cursoestatistica/), intitulada <i>**Um Estudo Empı́rico do Desempenho de Combinações de Previsões**</i>. Concluí o curso em 2011, assim, depois de mais de 11 anos foram necessários alguns pequenos ajustes do código original (dado as atualizações das bibliotecas do R que utilizei) para que o mesmo funcionasse. Fiquei surpresa, pois realmente foram poucos ajustes, mas também me surprendeu o quanto o código pode ser otimizado. Não omimizei neste primeiro momento, mas assim que possível pretendo fazê-lo.

### 3.1.2. Metodologias de Imputação de Dados

[Neste projeto](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/tree/main/Series_Temporais/Imputacao), tenho como objetivo praticar alguns dos métodos de imputação de dados como: Forward fill, Backward fill, Média Móvel, Imputação linear e Imputação Polinomial. Abordo, também, o problema de Lookahead. Utilizo a série histórica do Instituto Brasileiro de Geografia e Estatística (IBGE) da taxa de desocupação no Brasil.

## 3.2. Inferência Estatística

A inferência estatística é um ramo da estatística que lida com o processo de tirar conclusões ou fazer estimativas sobre uma população maior com base em informações obtidas de uma amostra representativa dessa população. Envolve a utilização de técnicas estatísticas para extrair insights, realizar testes de hipóteses, construir intervalos de confiança e tomar decisões informadas sobre características desconhecidas da população, utilizando os dados amostrais disponíveis. Em resumo, a inferência estatística permite generalizar informações da amostra para a população, com um nível definido de confiança estatística.

Nos projetos de Inferência Estatística tenho como objetivo simular o Teorema do Limite Central (TLC) e abordar diferentes Testes de Hipóteses.

### 3.2.1 Teorema do Limite Central

O Teorema Central do Limite é crucial no contexto da Inferência Estatística, pois ele fornece a base para a distribuição das médias amostrais. Quando a amostra é grande, a distribuição das médias amostrais se aproxima de uma distribuição normal. Isso é fundamental para a construção de intervalos de confiança e para realizar testes de hipóteses sobre parâmetros populacionais. [Neste projeto](https://github.com/Leiliane-Oliveira/Analise-de-Dados-com-Linguagem-R/tree/main/Inferencia_Estatistica/TLC) simulou-se esse teorema no R.

# 4. Referências

As referências são apresentadas no README de cada projeto.
