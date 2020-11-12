###########################################################################################################
#                                                                                                         #
#                                          Ciência dos Dados                                              #
#                                                                                                         #
#                                             #NaPrática                                                  #
#                                                                                                         #
#                  Machine Learning - Modelo de Árvores de Decisão Para Risco de Crédito                  #
#                                                                                                         #
#                                 Aprovação de Empréstimos Online                                         #        
#                                                                                                         #
#                              (Problemas Clássicos de Risco de Crédito)                                  #
###########################################################################################################

###########################################################################################################
# Algoritmo C5.0 - Decision Tree 
###########################################################################################################

# Problema de Negócio
# Construindo Um Modelo de Decisão Para Risco de Crédito - Aprovação de Empréstimos Online

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Definindo o diretório de trabalho
getwd()
setwd("")

# Calculando a entropia de duas classes
-0.60 * log2(0.60) - 0.40 * log2(0.40)

# Gerando a curva de Entropia
curve(-x * log2(x) - (1 - x) * log2(1 - x), col = "red", xlab = "x", ylab = "Entropy", lwd = 4)

# Identificando o risco de crédito
credit <- read.csv("dados/credito.csv")
str(credit)
View(credit)

# Verificando 2 atributos do cliente
table(credit$checking_balance)
table(credit$savings_balance)

# Verificando as características do crédito
summary(credit$months_loan_duration)
summary(credit$amount)

# Variável target
table(credit$default)

# Usando sample para construir os dados de treino e de teste
set.seed(123)
train_sample <- sample(1000, 900)

# Split dos dataframes
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# Verificando a proporção da variável target
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Construindo um modelo
install.packages("C50")
library(C50)
?C5.0

# Criando e visualizando o modelo
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

# Informações detalhadas sobre a árvore
summary(credit_model)

# Avaliando a performance do modelo
credit_pred <- predict(credit_model, credit_test)

# Confusion Matrix para comparar valores observados e valores previstos
install.packages("gmodels")
library(gmodels)

# Criando a Confusion Matrix
?CrossTable
CrossTable(credit_test$default, 
           credit_pred,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Observado', 'Previsto'))

# Melhorando a performance do modelo

# Aumentando a precisão com 10 tentativas
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)

# Score do modelo
credit_boost_pred10 <- predict(credit_boost10, credit_test)

# Confusion Matrix
CrossTable(credit_test$default, 
           credit_boost_pred10,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Observado', 'Previsto'))

# Dando pesos aos erros

# Criando uma matriz de dimensões de custo
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("Previsto", "Observado")
matrix_dimensions

# Construindo a matriz
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# Aplicando a matriz a árvore
?C5.0
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)

# Score do modelo
credit_cost_pred <- predict(credit_cost, credit_test)

# Confusion Matrix
CrossTable(credit_test$default, 
           credit_cost_pred,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Observado', 'Previsto'))


################################## FIM  ##########################################################################

