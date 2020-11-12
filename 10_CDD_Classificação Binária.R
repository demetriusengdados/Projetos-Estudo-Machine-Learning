###########################################################################################################
#                                                                                                         #
#                                          Ciência dos Dados                                              #
#                                                                                                         #
#                                   Machine Learning - KNN - Previsão                                     #
#                                                                                                         #
#                                   Prevendo o resultado do índice S&P                                    #        
#                                                                                                         #
###########################################################################################################


# Classificação KNN em R

# Classificação KNN em R - Definindo Um Problema Para Classificação Binária

# Prevendo o resultado do índice S&P (The Standard & Poor's 500) do
# American stock market index (NYSE or NASDAQ)

# https://rdrr.io/cran/ISLR/man/Smarket.html

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Definindo o diretório de trabalho
getwd()
setwd("C:/Users/user/Videos/Ciência dos Dados/Cursos/YouTube/10 - Previsão com KNN")

# Instalando os pacotes
install.packages("ISLR")
install.packages("caret")
install.packages("e1071")

# Carregando os pacotes
library(ISLR)
library(caret)
library(e1071)

# Definindo o seed
set.seed(300)

###########################################################################################################
# 1° - Classificação KNN em R - Carregando e Explorando o Dataset
###########################################################################################################


?Smarket
summary(Smarket)
str(Smarket)
head(Smarket)
View(Smarket)

# Split do dataset em treino e teste
?createDataPartition
indxTrain <- createDataPartition(y = Smarket$Direction, p = 0.75, list = FALSE)
View(indxTrain)
dados_treino <- Smarket[indxTrain,]
dados_teste <- Smarket[-indxTrain,]
class(dados_treino)
class(dados_teste)

# Verificando a distribuição dos dados originais e das partições
prop.table(table(Smarket$Direction)) * 100
prop.table(table(dados_treino$Direction)) * 100

# Correlação entre as variáveis preditoras
descrCor <- cor(dados_treino[,names(dados_treino) != "Direction"])
descrCor

###########################################################################################################
# 2° - Preprocessamento - Classificação KNN em R - Normalização dos Dados (Center e Scale)
###########################################################################################################

# A transformação de "scale" calcula o desvio padrão para um atributo e divide 
# cada valor por esse desvio padrão.

# A transformação "center" calcula a média de um atributo e a subtrai de cada valor.

# Função de Normalização
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center = T, scale = T)
  }
  return(df)
}

# Removendo a variável target dos dados de treino e teste
numeric.vars_treino <- colnames(treinoX <- dados_treino[,names(dados_treino) != "Direction"])
numeric.vars_teste <- colnames(testeX <- dados_teste[,names(dados_teste) != "Direction"])

# Aplicando normalização às variáveis preditoras de treino e teste
dados_treino_scaled <- scale.features(dados_treino, numeric.vars_treino)
dados_teste_scaled <- scale.features(dados_teste, numeric.vars_teste)
View(dados_treino_scaled)
View(dados_teste_scaled)

###########################################################################################################
# 3° - Classificação KNN em R - Construção e Treinamento do Modelo
###########################################################################################################

set.seed(400)
?trainControl
?train

# Arquivo de controle
ctrl <- trainControl(method = "repeatedcv", repeats = 3) 

# Criação do modelo
knn_v1 <- train(Direction ~ ., 
                data = dados_treino_scaled, 
                method = "knn", 
                trControl = ctrl, 
                # preProcess = c("center","scale"), # Isso é a normalização
                tuneLength = 20)

###########################################################################################################
# 4° - Classificação KNN em R - Avaliação do Modelo
###########################################################################################################

# Modelo KNN
knn_v1

# Número de Vizinhos x Acurácia
plot(knn_v1)

# Fazendo previsões
knnPredict <- predict(knn_v1, newdata = dados_teste_scaled)
knnPredict

# Criando a Confusion Matrix
confusionMatrix(knnPredict, dados_teste$Direction)


# Classificação KNN em R - Aplicando Outras Métricas

# Arquivo de controle
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, 
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# Treinamento do modelo
knn_v2 <- train(Direction ~ ., 
                data = dados_treino_scaled, 
                method = "knn", 
                trControl = ctrl, 
                metric = "ROC",
                # preProcess = c("center","scale"),  # Isso é a normalização
                tuneLength = 20)

# Modelo KNN
knn_v2


# Número de Vizinhos x Acurácia
plot(knn_v2, print.thres = 0.5, type="S")

# Fazendo previsões
knnPredict <- predict(knn_v2, newdata = dados_teste_scaled)

# Criando a Confusion Matrix
confusionMatrix(knnPredict, dados_teste$Direction)

###########################################################################################################
# 5° - Previsões com novos dados
###########################################################################################################

# Preparando dados de entrada
Year = c(2006, 2007, 2008)
Lag1 = c(1.30, 0.09, -0.654)
Lag2 = c(1.483, -0.198, 0.589)
Lag3 = c(-0.345, 0.029, 0.690)
Lag4 = c(1.398, 0.104, 1.483)
Lag5 = c(0.214, 0.105, 0.589)
Volume = c(1.36890, 1.09876, 1.231233)
Today = c(0.289, -0.497, 1.649)

novos_dados = data.frame(Year, Lag1, Lag2, Lag3, Lag4, Lag5, Volume, Today)
novos_dados
str(novos_dados)
class(novos_dados)
View(novos_dados)
# Normalizando os dados

# Extraindo os nomes das variáveis
nomes_variaveis <- colnames(novos_dados)
nomes_variaveis

# Aplicando a função
novos_dados_scaled <- scale.features(novos_dados, nomes_variaveis)
novos_dados_scaled
str(novos_dados_scaled)
class(novos_dados_scaled)
View(novos_dados_scaled)

# Fazendo previsões
knnPredict <- predict(knn_v2, newdata = novos_dados_scaled)
cat(sprintf("\n Previsão de \"%s\" é \"%s\"\n", novos_dados$Year, knnPredict))


###########################################################################################################
#                                             Fim                                                         #
###########################################################################################################



