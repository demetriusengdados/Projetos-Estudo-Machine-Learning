###########################################################################################################
#                                                                                                         #
#                                          Ciência dos Dados                                              #
#                                                                                                         #
#                                             #NaPrática                                                  #
#                                                                                                         #
#                         Machine Learning - Redes Neurais Artificiais                                    #
#                                                                                                         #
#                             Modelando a força (strength) do concreto                                    #
#                               para Construção da Hidrelétrica                                           #        
#                                                                                                         #
#                                                                                                         #
###########################################################################################################

###########################################################################################################
# Redes Neurais em R
###########################################################################################################


# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Definindo o diretório de trabalho
getwd()

# Carregando os dados
concrete <- read.csv("concrete.csv")
View(concrete)
str(concrete)

# Função de Normalização
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Aplicando a função de normalização a todo o dataset
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# Confirmando que o range está entre 0 e 1
summary(concrete_norm$strength)

# Comparando com o original
summary(concrete$strength)

# Criando dataset de treino e de teste
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

# Treinando o modelo
install.packages("neuralnet")
library(neuralnet)

# Rede Neural com apenas UMA camada oculta de neurônios
set.seed(12345) 
?neuralnet
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                            ash + water + superplastic + 
                            coarseagg + fineagg + age,
                            data = concrete_train)

print(concrete_model)

# Visualizando a rede criada
plot(concrete_model)

# Avaliando a performance
model_results <- compute(concrete_model, concrete_test[1:8])

# Obter os valores previstos
predicted_strength <- model_results$net.result

# Examinando a correlação dos valores previstos
?cor
cor(predicted_strength, concrete_test$strength)

# Otimizando o modelo
# Aumentando o número de camadas ocultas (2 camadas) com 5 e 4 neurônios respectivamente.
set.seed(12345) 
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden =  c(5,4) )

# Plot
plot(concrete_model2)

# Avaliando o resultado
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

model_results2

###########################################################################################################
# FIM
###########################################################################################################




