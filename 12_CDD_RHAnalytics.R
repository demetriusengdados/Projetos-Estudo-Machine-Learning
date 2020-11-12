###########################################################################################################
#                                                                                                         #
#                                          Ciência dos Dados                                              #
#                                                                                                         #
#                                          RH Analytics em R                                              #
#                                                                                                         #
#                   Quais são os fatores decisivos para um Talento deixar seu emprego                     #        
#                                                                                                         #
###########################################################################################################

# RH Analytics em R


# O objetivo deste estudo é investigar as causas que fazem os funcionários deixarem seus empregos. 
# Vamos responder a estas duas perguntas:


###########################################################################################################
# Passo 1° - Qual o problema de negócio
###########################################################################################################

# 1- Quais são os aspectos mais importantes e que são decisivos para os colaboradores deixarem seus empregos?
# 2- Qual a melhor forma de fazer estas previsões: regressão logística ou árvore de decisão?

# Pasta de trabalho
setwd("Z:/Dropbox/DSA/Business-Analytics/R/Cap04")
getwd()


###########################################################################################################
# Passo 2° - Preparando os dados
###########################################################################################################



library(ggplot2)
library(scales)
library(gmodels) 
library(rpart)
library(ROCR)
library(corrplot)

# Carregando os dados
hr = read.csv("dados_func.csv")
dim(hr)
View(hr)
attach(hr)
summary(hr)

# Function para verificar valores missing e proporção de valores missing
propmiss <- function(dataframe) {
  m <- sapply(dataframe, function(x) {
    data.frame(
      nmiss = sum(is.na(x)),  
      n=length(x),
      propmiss = sum(is.na(x))/length(x) 
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  return(d[order(d$propmiss), ])
}
propmiss(hr)


###########################################################################################################
# Passo 3° - Análise Exploratória
###########################################################################################################
str(hr)  

# Frequência da variável target (deixou_empresa)
cbind( Freq = table(deixou_empresa),
       Cumul = cumsum(table(deixou_empresa)),
       relative = round((prop.table(table(deixou_empresa))*100),2))

# Pie Chart para a variável target
slices <- c(76, 24)
lbls <- c("0 - Ainda trabalhando na empresa (76%)", "1 - Deixaram a empresa (24%)")
pie(slices, labels = lbls, main = "Pie Chart para a variável target", col = c("green", "red"))

# Frequência de acidentes de trabalho
cbind( Freq = table(acidente_trabalho),
       Cumul = cumsum(table(acidente_trabalho)),
       relative = round((prop.table(table(acidente_trabalho))*100),2))

# Frequência de promoção nos últimos 5 anos
cbind( Freq = table(ultima_promocao_5anos),
       Cumul = cumsum(table(ultima_promocao_5anos)),
       relative = round((prop.table(table(ultima_promocao_5anos))*100),2))

# Colaboradores por departamento
vec_dept <- as.vector(area)
unique(vec_dept)
vec_dept <- factor(vec_dept)
qplot(vec_dept, xlab = "Departamento", ylab = "Total Colaboradores") + ggtitle("Distribuição por Departamentos")

# Colaboradores por salario
vec_salary <- as.vector(salario)
unique(vec_salary)
vec_salary <- factor(vec_salary)
qplot(vec_salary, xlab = "Salário", ylab = "Total Colaboradores") + ggtitle("Distribuição por Salários")

# Analisando o nível de satisfação
hist(nivel_satisfacao, freq = F, main = "Histograma do Nível de Satisfação")
lines(density(nivel_satisfacao))

# Analisando a última avaliação
hist(ultima_avaliacao, freq = F, main = "Histograma da Última Avaliação")
lines(density(ultima_avaliacao))

# Analisando o número de projetos
hist(numero_projetos, ylim = c(0,0.8), freq = F, main = "Histograma do Número de Projetos")
lines(density(numero_projetos))

# Analisando o número médio de horas trabalhas por mês
hist(horas_medias_por_mes, freq = F, main = "Histograma da Média de Horas Trabalhadas por Mês")
lines(density(horas_medias_por_mes))

# Tempo de trabalho na empresa
hist(hr$tempo_empresa,  ylim = c(0,1.2), freq = F, main = "Histograma do Tempo Trabalhado na Empresa")
lines(density(tempo_empresa))

# Calculando a correlação
cor(hr[sapply(hr, is.numeric)])

# Calculando a correlação
par(mar=c(4,3,2,2))
par(oma=c(1,1,2,2))
corrplot(cor(hr[,c(1,2,3,4,5,6,7,8)]), type = "lower", tl.col = "black", method = "ellipse")

# --> Nível de satisfação tem a maior correlação negativa com a variável target.

# Análise bivariada

# Saída da empresa x Acidente de trabalho - Barplot
t <- table(deixou_empresa, hr$acidente_trabalho)
barplot(prop.table(t,2), legend = paste(unique(deixou_empresa)),
        ylab = "Probabilidade Acumulada", xlab = "Acidente de Trabalho")

# Saída da empresa x Acidente de trabalho - Crosstable
CrossTable(deixou_empresa, hr$acidente_trabalho, prop.r = TRUE, prop.c = FALSE,
           prop.t = TRUE, prop.chisq = FALSE)

# Saída da empresa x Promoção nos últimos 5 anos - Crosstable
CrossTable(deixou_empresa, ultima_promocao_5anos, prop.r = TRUE, prop.c = FALSE,
           prop.t = TRUE, prop.chisq = FALSE)

# Saída da empresa x Area - Aggregate
aggregate(deixou_empresa ~ area, FUN = mean)

# Saída da empresa x Salario - Aggregate
aggregate(deixou_empresa ~ salario, FUN = mean)


# --> Pessoas que não tiveram uma promoção nos últimos 5 anos deixaram mais do que aqueles que tiveram.

# --> Pessoas da área de Gestão têm a média mais baixa de saída da empresa e pessoas de RH a média mais alta.

# --> Pessoas com Salário baixo têm maior média de saída da empresa em comparação com outras categorias.


###########################################################################################################
# Passo 4° - Transformação dos Dados
###########################################################################################################


# Categorizando a variável area de acordo com a taxa média de saída da empresa e criando grupos de áreas
group1 <- c('hr')
group2 <- c('accounting','sales','support','technical')
group3 <- c('marketing', 'IT','product_mng')
group4 <- c('management','RandD')

hr$area_grupo <- ifelse(area %in% group1, 1,
                       ifelse(area %in% group2, 2,
                              ifelse(area %in% group3, 3,4)))

aggregate(deixou_empresa ~ hr$area_grupo, FUN = mean)


##### -------------- Dados de Treino e Dados de Teste ---------------
  
# Dividindo 70% para treino e 30% para teste
set.seed(4)
hr_train <- sample(nrow(hr), floor(nrow(hr)*0.7))
train <- hr[hr_train,]
test <- hr[-hr_train,]

###########################################################################################################
# Passo 4° - Machine Learning 
###########################################################################################################



##### --------------Regressão Logística ---------------
###########################################################################################################


# Testando todas as variáveis
names(hr)
model <- glm(formula = (deixou_empresa) ~  nivel_satisfacao
             + ultima_avaliacao
             + numero_projetos
             + horas_medias_por_mes
             + tempo_empresa
             + acidente_trabalho
             + ultima_promocao_5anos
             + area
             + salario,
             family = binomial(logit), data = train)

summary(model)

anova(model, test = "Chisq")

# Todas as variáveis são relevantes, sendo as mais importantes o nível de satisfação, acidente de trabalho e salário, 
# nesta ordem.

# Previsões
p <- predict(model, test, type = "response")
pr <- prediction(p, test$deixou_empresa)

# Calculando a taxa de verdadeiro positivo e falso positivo
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# Area Under the Curve
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



##### -------------- Árvore de Decisão ---------------
###########################################################################################################


# Usando a mesma combinação de variáveis
tree1 <- rpart(formula = (deixou_empresa) ~  nivel_satisfacao
               + ultima_avaliacao
               + numero_projetos
               + horas_medias_por_mes
               + tempo_empresa
               + acidente_trabalho
               + ultima_promocao_5anos
               + area
               + salario,
               data = train,
               method = "class")

# Plot
plot(tree1, uniform = TRUE, main = "Classification Tree")
text(tree1, use.n = TRUE, all = TRUE, cex = .8)
printcp(tree1)

# Erros
tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"]

# Não é necessário fazer o pruning , que tem que ser feito quando a arvore aprende demais...overfiting
plotcp(tree1)

# --> Não será necessário fazer o prune da árvore

# Confusion Matrix - Dados de Treino
conf_matrix_tree <- table(train$deixou_empresa, predict(tree1, type = "class"))
rownames(conf_matrix_tree) <- paste("Actual", rownames(conf_matrix_tree), sep = ":")
colnames(conf_matrix_tree) <- paste("Pred", colnames(conf_matrix_tree), sep = ":")
print(conf_matrix_tree)

# Dados de Teste
test_tree = predict(tree1, test, type = "prob")

###########################################################################################################
# Passo 5° - Avaliando o Modelo 
###########################################################################################################

# Armazenando o score de performance do modelo
pred_tree <-prediction(test_tree[,2], test$deixou_empresa)

#  Area under the Curve
# O modelo acerta bastante. Poderia pensar que está com overfiting de tanto que ele acerta.
# Mas como calculamos com dados de teste é capaz que o modelo esteja bom mesmo....

perf_tree <- performance(pred_tree,"auc")
perf_tree

# Calculando a taxa de verdadeiro positivo e falso positivo
perf_tree <- performance(pred_tree, "tpr", "fpr")

# Plot Curva ROC 
plot(perf_tree,  lwd = 1.5)


###########################################################################################################
# Passo 6° - Entrega dos Resultados Conclusivos 
###########################################################################################################

  
# 1- Quais são os aspectos mais importantes e que são decisivos para os colaboradores deixarem seus empregos?
# Na regressão logística encontrei nível de satisfação, acidente de trabalho e salário como os aspectos mais relevantes.
# A árvore de decisão, os mais importantes são o nível de satisfação, o tempo gasto na empresa e número de projetos.


# 2- Qual a melhor forma de fazer estas previsões: regressão logística ou árvore de decisão?
# Para esse conjunto de dados, a árvore de decisão obteve um melhor desempenho no conjunto de dados de teste. 
# Podemos observar isso pelas curvas ROC abaixo.

# IMPORTANTE: Os 2 modelos estão corretos, mas com níveis de PRECISAO diferentes...
plot(prf)
title("Curva ROC - Regressão Logística")

plot(perf_tree,  lwd = 1.5)
title("Curva ROC - Decision Tree")


#############################################   FIM  ###################################################################







