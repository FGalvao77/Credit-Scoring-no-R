#### Curso Credit scoring no R ####
##### Aula 1 - Nivelamento - conceitos básicos #####
###### Estudo da base de dados do Titanic ######


#######  Mudando a área de trabalho do R #######
caminho <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(caminho)
getwd()

### Carregar os pacotes no ambiente 
install.packages(c('readr')) # instalando o pacote
library(readr)               # ativando o pacote usado para fazer a leitura de dados csv e txt 

##########
# Realizando a leitura da base de dados via web e instanciando em um objeto
bdados <- read_csv("https://www.udacity.com/api/nodes/5420148578/supplemental_media/titanic-datacsv/download")
  ##########

### Quais são as variáveis da base de dados
names(bdados)       # lista o nome das colunas
str(bdados)         # detalha a estrutura do objeto

### Quantas linhas e colunas (variáveis)?
cat("A base de dados estudada possui ", length(bdados), 
    "variáveis e ", nrow(bdados), " linhas", "\n")

### Para tirar dúvida sobre alguma funcionalidade usamos o '?' e/ou 'help' e nome da função e/ou pacote
?cat
help("cat")   # nome do pacote entre parenteses e aspas

### Descoberta 1: Criando tabelas de frequência simple ####
### Variáveis categóricas: Sex, Embarked
table(bdados$Sex)                    # Genero
table(bdados$Embarked)               # Cidade de embarque
table(bdados$Survived)               # Sobrevivente
table(bdados$Age)                    # Idade

### Também podemos usar o 'help' para sanar alguma dúvida
help('prop.table')

### Descoberta 2: Estudo descritivo da variáveis quantitativas ####
summary(bdados$Age)         # Idade
summary(bdados$SibSp)       # Quantidade de irmãos
summary(bdados$Parch)       # Quantidade de pais
summary(bdados$Fare)        # Valor da passagem 
summary(bdados)             # Análise estatística geral na base de dados como todo

### Descoberta 3: Mortos por sexo
table(bdados$Sex, bdados$Survived)              # Quantidade de sobreviventes por sexo, onde "0" - não sobreviveu e "1" - sobreviveu
prop.table(table(bdados$Survived))
prop.table(table(bdados$Sex, bdados$Survived))
prop.table(table(bdados$Sex, bdados$Survived),
           margin = 1)
prop.table(table(bdados$Sex, bdados$Survived),
           margin = 2)

### Decoberta 4: Conhecer a distribuição do preço do ticket pago.
hist(bdados$Fare)

### Que tal um box-plot?
boxplot(bdados$Fare)    # tarifa
boxplot(bdados$Age)     # idade

### Descoberta 5: Que tal fazer um gráfico de dispers?o
plot(bdados$Fare, bdados$Age, main = "Passagem x Idade",
     xlab = "Preço", ylab = "Idade", col = "blue", pch = 16,
     cex = 1.1)

### Descoberta 6: Gráfico de sobreviventes
library(ggplot2)    # habilitando o pacote

class(bdados$Survived)
bdados$sobrevive <- factor(bdados$Survived, 
                           labels=c("Mortos", "Sobreviventes"))
class(bdados$sobrevive)

### Gerando o gráfico de barras
ggplot(data=bdados, aes(x=sobrevive)) +
  geom_bar(stat="count", fill="orange") +
  xlab("")+
  theme_minimal() +
  labs(title = "Quantidade de mortos vs sobreviventes")

### Descoberta 7: Crianças (de 0 a 12 anos)
class_id <- function(idade){idade<13}   # criando uma função

#teste1 = bdados$Age < 13   # linha de código para teste

bdados$crianca <- factor(class_id(bdados$Age), 
                         label=c("Adulto", "Criança"))
table(bdados$crianca)
prop.table(table(bdados$crianca))

### Descoberta 8: Criança que morreram
aux_tab = table(bdados$crianca, bdados$sobrevive)
aux_tab
prop.table(aux_tab)
prop.table(aux_tab, margin = 1)
prop.table(aux_tab, margin = 2)

### Descoberta 9: Preço da passagem por sexo
install.packages('dplyr')
library("dplyr")    # habilitando o pacote

bdados %>%
  group_by(Sex) %>%
  summarise_at(vars(Fare), list(media = mean, 
                                mediana = median))

### Descoberta 10: Quem não pagou sobreviveu?
custo_0 <- bdados %>%
  dplyr::filter(Fare == 0)    # realizando uma comparação de igualdade

table(custo_0$sobrevive)      # realizando uma comparação de passageiros que não pagaram a passagem - 'Mortos x Sobreviventes'
  
