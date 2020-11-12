###############################################################################################################
#         Ciência dos Dados -  #naPrática - Churn Analytics em R - Prevendo a Rotatividade de Clientes
###############################################################################################################

# Customer Churn Analytics 

# A rotatividade (churn) de clientes ocorre quando clientes ou assinantes param de fazer negócios 
# com uma empresa ou serviço. Também é conhecido como perda de clientes ou taxa de cancelamento.

# Um setor no qual saber e prever as taxas de cancelamento é particularmente útil é o setor de telecomunicações, 
# porque a maioria dos clientes tem várias opções de escolha dentro de uma localização geográfica.

# Neste projeto, vamos prever a rotatividade (churn) de clientes usando um conjunto de dados de telecomunicações. 
# Usaremos a regressão logística, a árvore de decisão e a floresta aleatória como modelos de Machine Learning. 

# Usaremos um dataset oferecido gratuitamente no portal IBM Sample Data Sets. 
# Cada linha representa um cliente e cada coluna contém os atributos desse cliente.

# https://www.ibm.com/communities/analytics/watson-analytics-blog/guide-to-sample-datasets/

# Definindo o diretório de trabalho
getwd()


# Carregando os pacotes
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)


##### Carregando e Limpando os Dados ##### 

# Os dados brutos contém 7043 linhas (clientes) e 21 colunas (recursos). 
# A coluna "Churn" é o nosso alvo.
churn <- read.csv('Telco-Customer-Churn.csv')
View(churn)
str(churn)


# Usamos sapply para verificar o número de valores ausentes (missing) em cada coluna. 
# Descobrimos que há 11 valores ausentes nas colunas "TotalCharges". 
# Então, vamos remover todas as linhas com valores ausentes.
sapply(churn, function(x) sum(is.na(x)))
churn <- churn[complete.cases(churn), ]


# Olhe para as variáveis, podemos ver que temos algumas limpezas e ajustes para fazer.
# 1. Vamos mudar "No internet service" para "No" por seis colunas, que são: 
# "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "streamingTV", 
# "streamingMovies".
# Tudo para simplificar a análise
str(churn)
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}


# 2. Vamos mudar "No phone service" para "No" para a coluna “MultipleLines”
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))


# 3. Como a permanência mínima é de 1 mês e a permanência máxima é de 72 meses, 
# podemos agrupá-los em cinco tempos de contrato (tenure): 
# “0-12 Mês”, “12–24 Mês”, “24–48 Meses”, “48–60 Mês” Mês ”,“> 60 Mês”
min(churn$tenure); max(churn$tenure)

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)


# 4. Alteramos os valores na coluna “SeniorCitizen” de 0 ou 1 para “No” ou “Yes”.
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))



# 5. Removemos as colunas que não precisamos para a análise.
churn$customerID <- NULL
churn$tenure <- NULL
View(churn)

############################################################### 
##### Análise exploratória de dados e seleção de recursos ##### 

# Correlação entre variáveis numéricas
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nGráfico de Correlação para Variáveis Numéricas", method="number")


# Os encargos mensais e os encargos totais estão correlacionados. 
# Então, um deles será removido do modelo. Nós removemos Total Charges.
# Estamos removendo para evitar a multicolinearidade (pode causar impactos na estimativa dos parâmetros)
churn$TotalCharges <- NULL

# Comparar as variaveis categorias
# Gráficos de barra de variáveis categóricas
p1 <- ggplot(churn, aes(x=gender)) + ggtitle("Gender") + xlab("Sexo") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Parceiros") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependentes") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)


p5 <- ggplot(churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Telefonia") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p6 <- ggplot(churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Múltiplas Linhas") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)


p9 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p10 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)


p13 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p14 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)


# Todas as variáveis categóricas parecem ter uma distribuição razoavelmente ampla, 
# portanto, todas elas serão mantidas para análise posterior.

###############################################################################################
##### Modelagem Preditiva ##### 

# Regressão Logística

# Primeiro, dividimos os dados em conjuntos de treinamento e testes

?createDataPartition #pacote caret

intrain <- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)  # para você ter os mesmos resultados que o meu, pois é aleatório
training <- churn[intrain,]
testing <- churn[-intrain,]


# Confirme se a divisão está correta
dim(training); dim(testing)


# Treinando o modelo de regressão logística
# Fitting do Modelo
?glm  
LogModel <- glm(Churn ~ ., family=binomial(link="logit"), data=training) # SIM acabamos de treinar o modelo (gastamos tempo no inicio)
print(summary(LogModel))


# Análise de Variância - ANOVA
# Verificar as variaveis mais importantes para o modelo
# Os três principais recursos mais relevantes incluem 
# Contract, tenure_group e PaperlessBilling.
?anova
anova(LogModel, test="Chisq")


# Analisando a tabela de variância, podemos ver a queda no desvio ao adicionar cada variável 
# uma de cada vez. Adicionar InternetService, Contract e tenure_group reduz 
# significativamente o desvio residual. 
# As outras variáveis, como PaymentMethod e Dependents, parecem melhorar menos o modelo, 
# embora todos tenham valores p baixos.
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"

# Avaliar o Modelo passando dados de teste "testing"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))


# Matriz de Confusão de Regressão Logística
print("Confusion Matrix Para Logistic Regression"); table(testing$Churn, fitted.results > 0.5)


# Odds Ratio

# Uma das medidas de desempenho interessantes na regressão logística é Odds Ratio. 
# Basicamente, odds ratio é a chance de um evento acontecer.
exp(cbind(OR=coef(LogModel), confint(LogModel)))

# Para cada aumento de unidade no encargo mensal (Monthly Charge), 
# há uma redução de 2,5% na probabilidade do cliente cancelar a assinatura (churn=sim).
# Isso porque o usuario que paga mais tem mais conhecimento sobre o produto. Acontece geralmente 
# nas classes alta renda etc

# Árvore de Decisão

# Visualização da Árvore de Decisão
# Para fins de ilustração, vamos usar apenas três variáveis para plotar 
# árvores de decisão, elas são “Contrato”, “tenure_group” e “PaperlessBilling”.
?ctree
tree <- ctree(Churn ~ Contract+tenure_group+PaperlessBilling, training)
plot(tree, type='simple')

# 1. Das três variáveis que usamos, o Contrato é a variável mais importante 
# para prever a rotatividade de clientes ou não.
# 2. Se um cliente em um contrato de um ano ou de dois anos, 
# não importa se ele (ela) tem ou não a PapelessBilling, ele (ela) é menos propenso 
# a se cancelar a assinatura.
# 3. Por outro lado, se um cliente estiver em um contrato mensal, 
# e no grupo de posse de 0 a 12 meses, e usando o PaperlessBilling, 
# esse cliente terá mais chances de cancelar a assinatura.


# Matriz de Confusão da Árvore de Decisão
# Estamos usando todas as variáveis para tabela de matriz de confusão de produto e fazer previsões.
pred_tree <- predict(tree, testing)
print("Confusion Matrix Para Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)


# Precisão da árvore de decisão
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))


##### Random Forest #####

set.seed(2017)
?randomForest
rfModel <- randomForest(Churn ~ ., data = training)
print(rfModel)
plot(rfModel)

# A previsão é muito boa ao prever "Não". 
# A taxa de erros é muito maior quando se prevê "sim".

# Prevendo valores com dados de teste
pred_rf <- predict(rfModel, testing)

# Confusion Matrix
print("Confusion Matrix Para Random Forest"); table(testing$Churn, pred_rf)

# Recursos mais importantes
?varImpPlot
varImpPlot(rfModel, sort=T, n.var = 10, main = 'Top 10 Feature Importance')



