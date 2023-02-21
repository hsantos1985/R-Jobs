







"
Trabalho de Análise Descritiva de um Conjunto de Dados
Utilizando os conhecimentos adquiridos em nosso treinamento realize uma análise descritiva básica de um conjunto de dados retirados da Pesquisa Nacional por Amostra de Domicílios - 2015 do IBGE.

Vamos construir histogramas, calcular e avaliar medidas de tendência central, medidas separatrizes e de dispersão dos dados.

Siga o roteiro proposto e vá completando as células vazias. Procure pensar em mais informações interessantes que podem ser exploradas em nosso dataset.

DATASET DO PROJETO
Pesquisa Nacional por Amostra de Domicílios - 2015
A Pesquisa Nacional por Amostra de Domicílios - PNAD investiga anualmente, de forma permanente, características gerais da população, de educação, trabalho, rendimento e habitação e outras, com periodicidade variável, de acordo com as necessidades de informação para o país, como as características sobre migração, fecundidade, nupcialidade, saúde, segurança alimentar, entre outros temas. O levantamento dessas estatísticas constitui, ao longo dos 49 anos de realização da pesquisa, um importante instrumento para formulação, validação e avaliação de políticas orientadas para o desenvolvimento socioeconômico e a melhoria das condições de vida no Brasil.

Fonte dos Dados
https://ww2.ibge.gov.br/home/estatistica/populacao/trabalhoerendimento/pnad2015/microdados.shtm

Variáveis utilizadas
Renda
Rendimento mensal do trabalho principal para pessoas de 10 anos ou mais de idade.

Idade
Idade do morador na data de referência em anos.

Altura (elaboração própria)
Altura do morador em metros.

UF
Código	Descrição
11	Rondônia
12	Acre
13	Amazonas
14	Roraima
15	Pará
16	Amapá
17	Tocantins
21	Maranhão
22	Piauí
23	Ceará
24	Rio Grande do Norte
25	Paraíba
26	Pernambuco
27	Alagoas
28	Sergipe
29	Bahia
31	Minas Gerais
32	Espírito Santo
33	Rio de Janeiro
35	São Paulo
41	Paraná
42	Santa Catarina
43	Rio Grande do Sul
50	Mato Grosso do Sul
51	Mato Grosso
52	Goiás
53	Distrito Federal
Sexo
Código	Descrição
0	Masculino
1	Feminino
Anos de Estudo
Código	Descrição
1	Sem instrução e menos de 1 ano
2	1 ano
3	2 anos
4	3 anos
5	4 anos
6	5 anos
7	6 anos
8	7 anos
9	8 anos
10	9 anos
11	10 anos
12	11 anos
13	12 anos
14	13 anos
15	14 anos
16	15 anos ou mais
17	Não determinados
Não aplicável
Cor
Código	Descrição
0	Indígena
2	Branca
4	Preta
6	Amarela
8	Parda
9	Sem declaração
Observação
Os seguintes tratamentos foram realizados nos dados originais:

Foram eliminados os registros onde a Renda era inválida (999 999 999 999);
Foram eliminados os registros onde a Renda era missing;
Foram considerados somente os registros das Pessoas de Referência de cada domicílio (responsável pelo domicílio).

"

#importação das bibliotecas

install.packages('tapply')
install.packages('ggplot')


library(ggplot2)
library(ggplot)



library(tapply)



## 1 IMPORTAÇÃO DOS DADOS
dados<-read.csv('C:/Users/Dell/Documents/13_Alura_cursos/03_formação_estatística_R/01_Estatísticas_com_frequencias/dados.csv', sep=',')
dados


## 2 DISTRIBUIÇÃO DE FREQUENCIAS DA VARIÁVEL RENDA
"
Para avaliar o comportamento da variável RENDA vamos construir uma tabela de frequências considerando as seguintes classes em salários mínimos (SM)
Descreva os pontos mais relevantes que você observa na tabela e no gráfico.
Classes de renda:

A ► Acima de 25 SM

B ► De 15 a 25 SM

C ► De 5 a 15 SM

D ► De 2 a 5 SM

E ► Até 2 SM

Para construir as classes de renda considere que o salário mínimo na época da pesquisa era de R$ 788,00.

"

#2.1 DEFINIR O INTERVALO DAS CLASSES EM REAIS


classes<-c(min(dados$Renda),
           2 * 788,
           5 * 788,
           15 * 788,
           25 * 788,
           max(dados$Renda)
)

classes

# 2.2 Definir os labels das classes# 2º Definir os labels das classes

labels<-c('E','D','C','B','A')

#2.3 DEFININDO A COLUNA DE FREQUENCIAS

frequencia<-table(
  cut(
  x=dados$Renda,
  breaks = classes,
  labels=labels,
  include.lowest=TRUE
  )
)

  frequencia
  

#2.3 DEFININDO A COLUNA DE FREQUENCIAS E PORCENTAGEM
  percentual<-round(prop.table(frequencia)*100,2)
percentual

# 2.4 Construir a coluna de percentuais

dist_freq_tab<-cbind('Frequência'=frequencia, 'Porcentagem %'=percentual)
dist_freq_tab


# 2.5 Juntar as colunas de frequência e percentuais e ordenar as linhas de acordo com os labels das classes

dist_freq_tab[order(row.names(dist_freq_tab)),
  ]



#2.6 Construa um gráfico de barras para visualizar as informações da tabela de frequências acima



bar_chat<-data.frame(dist_fre_tab)
bar_chat



ggplot(
  
  bar_chat,
  aes(x=row.names(bar_chat),
      y=bar_chat$Frequência)) +
    geom_bar(stat='identity')+
    ylab('Freqência')+
    xlab('Classes de Renda')+
    ggtitle('Gráficos Classes de Renda')+
    formatos
    


# CONCLUSÕES 
"
Conclui-se que 
A clasee A possui a maior renda da população, contudo, apenas 0.55% da população
se enquandra nesta classe, a qual posssui renda igual ou superior a 25 sálarios mínimos
"


#3 Crie um histograma para as variáveis QUANTITATIVAS de nosso dataset



#3.1 HISTOGRAMAS
#3.11 IDADE

dados


ggplot(dados, aes(x = Idade)) + 
  geom_histogram(bins = 50) + 
  ylab("Frequência") + 
  xlab("Idades") + 
  ggtitle('Histograma das Idades') +
  formatos


#3.12 ALTURA

ggplot(dados, aes(x = Altura)) + 
  geom_histogram() + 
  ylab("Frequência") + 
  xlab("Altura") + 
  ggtitle('Histograma das Alturas') +
  formatos

#3.13 RENDA

ggplot(dados, aes(x = Renda)) + 
  geom_histogram(bins = 100) + 
  ylab("Frequência") + 
  xlab("R$") + 
  ggtitle('Histograma das Rendas') +
  formatos


#3.14 ANOS DE ESTUDO


dados

ggplot(dados, aes(x = Anos.de.Estudo)) + 
  geom_histogram(bins = 16) + 
  ylab("Frequência") + 
  xlab("Anos") + 
  ggtitle('Histograma dos anos de estudo') +
  formatos


# ou ainda 


hist(
  x=dados$Anos.de.Estudo,
  breakes='Sturges',
  col = 'yellow',
  main = 'Histograma Anos de estudo',
  xlab = 'Anos',
  ylab = 'Frequências',
  prob = TRUE,
  las = 1
)


#3.2 RENDIMENTO ATÉ R$20.000, HISTOGRAMA DE RENDA

ggplot(dados[dados$Renda < 20000, ], aes(x = Renda)) + 
  geom_histogram() + 
  ylab("Frequência") + 
  xlab("R$") + 
  ggtitle('Histograma das Rendas - Pessoas com renda até R$ 20.000,00') +
  formatos



# 4 TABELA DE FREQUENCIA SEXO E COR

#Construir uma tabela de frequências e uma com os percentuais cruzando das variáveis SEXO e COR



#Substituindo os valores da coluna sexo

sexo = c(
  'Masculino', 
  'Feminino'
)
cor = c(
  'Indígena', 
  'Branca', 
  'Preta', 
  'Amarela', 
  'Parda'
)
anos_de_estudo = c(
  'Sem instrução e menos de 1 ano', 
  '1 ano', 
  '2 anos', 
  '3 anos', 
  '4 anos', 
  '5 anos', 
  '6 anos', 
  '7 anos', 
  '8 anos', 
  '9 anos', 
  '10 anos', 
  '11 anos', 
  '12 anos', 
  '13 anos', 
  '14 anos', 
  '15 anos ou mais', 
  'Não determinados')


# Sexo
dados$Cat.Sexo<-factor(dados$Sexo)
levels(dados$Cat.Sexo)<-sexo

#Cor
dados$Cat.Cor<-factor(dados$Cor)
levels(dados$Cat.Cor)<-cor

#Anos de Estudo
dados$Cat.Anos.de.Estudo<-factor(dados$Anos.de.Estudo,order=TRUE)
levels(dados$Cat.Anos.de.Estudo)<-anos_de_estudo

head(dados)


#Sexo e Anos de estudo
frequencia<-table(dados$Cat.Sexo, dados$Cat.Anos.de.Estudo)
frequencia<-cbind(frequencia)
frequencia


#Sexo e Cor
frequencia<-table(dados$Cat.Sexo, dados$Cat.Cor)
frequencia<-cbind(frequencia)
frequencia

percentual<-round(cbind(prop.table(frequencia)*100),2)
percentual



freq<-table(dados$Renda,dados$Sexo )
freq<-cbind(freq)
freq

#CONCLUSÕES
'
Percebe-se que 32,6% da população asculina é de cor parda, para o feminino 14,1%

'

#ANÁLISE DESCRITIVA DA RENDA

mean(dados$Renda)


median(dados$Renda)


Moda <- function(x) {
  frequencias <- table(x) 
  return(names(frequencias)[frequencias == max(frequencias)])
}

as.numeric(Moda(dados$Renda))

var(dados$Renda)


sd(dados$Renda)


#Média, mediana,  valor máximo, desvio padrão e variância da variável RENDA segundo cor

medias<-tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), mean)
medias


mediana<-tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), median)
mediana

maximo<-tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), max)
maximo

variancia<-tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), var)
variancia

desvio_padrao<-tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), sd)
desvio_padrao



#Conclusões
'
Conclui-se que a maior média de renda se encontra para a cor Amarela, tanto para masculino e feminino

'


ggplot(data = dados[dados$Renda < 10000, ], aes(x = Cat.Cor, y = Renda, fill = Cat.Sexo)) + 
  geom_boxplot(size = 0.2) + 
  coord_flip() +
  ylab("R$") + 
  xlab("Cor") + 
  guides(fill = guide_legend(title = 'Sexo')) +
  ggtitle('Box-plot da RENDA por SEXO e COR') +
  formatos


#Percentual de pessoas do dataset ganham salário mímimo (R$788,00) ou enos?

round(
  
  length(dados$Renda[dados$Renda <= 788]) / length(dados$Renda) ,3
)


#o valor máximo ganho por 99% das pessoas de nosso dataset

quantile(dados$Renda, .99)


#Média, mediana, valor máximo e desvio-padrão da variável RENDA segundo ANOS DE ESTUDO e SEXO

dados

medias<-
  tapply(
    dados$Renda, list(
      dados$Cat.Anos.de.Estudo,
      dados$Cat.Sexo),mean    )
    
 medias   

 
 
 medianas<-
   tapply(
     dados$Renda, list(
       dados$Cat.Anos.de.Estudo,
       dados$Cat.Sexo),median )
 
 medianas
 
 
 
 maximos<-
   tapply(
     dados$Renda, list(
       dados$Cat.Anos.de.Estudo,
       dados$Cat.Sexo),max )
 
 maximos
 
 
 desvio_padrao <- tapply(dados$Renda, list(dados$Cat.Anos.de.Estudo, dados$Cat.Sexo), sd)
 desvio_padrao
 
 
 ggplot(data = dados[dados$Renda < 10000, ], aes(x = Cat.Anos.de.Estudo, y = Renda, fill = Cat.Sexo)) + 
   geom_boxplot(size = 0.2) + 
   coord_flip() +
   ylab("R$") + 
   xlab("Anos de Estudo") + 
   guides(fill = guide_legend(title = 'Sexo')) +
   ggtitle('Box-plot da RENDA por SEXO e ANOS DE ESTUDO') +
   formatos
