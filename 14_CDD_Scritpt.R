###########################################################################################################
#                                                                                                         #
#                                          Ciência dos Dados                                              #
#                                                                                                         #
#                                             #NaPrática                                                  #
#                                                                                                         #
#                              Machine Learning - Clusterização com K-Means                               #
#                                                                                                         #
#                          Segmentação de Consumidores para Campanha no Facebook                          #        
#                                                                                                         #
###########################################################################################################

#############################################################################################################################
# 1° Passo - Problema de Negócio: Encontrar segmentos de consumidores para campanhas personalizadas de Marketing no Facebook.
#############################################################################################################################


# Clustering com K-Means
# https://cran.r-project.org/web/views/Cluster.html

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding
 
# Definindo o diretório de trabalho
getwd()
setwd("C:\Users\user\Videos\Ciência dos Dados\Cursos\YouTube\14 - K-Means")

# Pacotes
install.packages("factoextra")
install.packages("cluster")
install.packages("fpc")
install.packages("NbClust")
install.packages("clValid")
install.packages("clustertend")

library(factoextra)
library(cluster)
library(fpc)
library(NbClust)
library(clValid)
library(magrittr)
library(clustertend)

#############################################################################################################################
# 2° Passo - Manipulação dos dados 
#############################################################################################################################

# Carregando os dados
dados_clientes_v1 <- read.csv("dados/dados_clientes.csv")
str(dados_clientes_v1)
names(dados_clientes_v1)
View(dados_clientes_v1)
summary(dados_clientes_v1)


#############################################################################################################################
# 3° Passo - Análise Exploratória 
#############################################################################################################################

# Tabela de proporção do sexo dos clientes
tabela_sexos = table(dados_clientes_v1$Sexo)
tabela_sexos
table(dados_clientes_v1$Sexo, useNA = "ifany")

# Buscando valores missing para variáveis relacionadas a idade 
summary(dados_clientes_v1$Idade)

# Buscando a média de idade
mean(dados_clientes_v1$Idade)

# Barplot de proporção do sexo dos clientes
barplot(tabela_sexos,
        main = "Proporção de Sexo dos Clientes",
        ylab = "Total",
        xlab = "Sexo",
        col = rainbow(2),
        legend = rownames(tabela_sexos))

# Histograma com a distribuição de frequência das idades dos clientes
hist(dados_clientes_v1$Idade,
     col = "blue",
     main = "Histogramas de Idades",
     xlab = "Idade",
     ylab = "Frequência",
     labels = TRUE)

# Boxplot para análise descritiva da idade
boxplot(dados_clientes_v1$Idade, 
        col = 3, 
        main = "Boxplot Para Análise Descritiva da Idade")
        
# Histograma com a distribuição de frequência do salário mensal
names(dados_clientes_v1)
summary(dados_clientes_v1$Salario_Mensal_Milhar)
hist(dados_clientes_v1$Salario_Mensal_Milhar,
     col = "#660033",
     main = "Histograma de Salário Mensal",
     xlab = "Salário Mensal",
     ylab = "Frequência",
     labels = TRUE)

# Análise da Pontuação dos Clientes
summary(dados_clientes_v1$Pontuacao_Gasto)

boxplot(dados_clientes_v1$Pontuacao_Gasto,
        horizontal = TRUE,
        col = "#990000",
        main = "BoxPlot Para Análise Descritiva da Pontuação do Cliente")

hist(dados_clientes_v1$Pontuacao_Gasto,
     main = "Histograma de Pontuação de Gasto",
     xlab = "Pontuação de Gasto",
     ylab = "Frequência",
     col = "#6600cc",
     labels = TRUE)


#############################################################################################################################
# 4° Passo - Pré-Processamento dos Dados 
#############################################################################################################################

# 

# Pré-processando e analisando os dados independente do sexo
dados_clientes_v2 <- dados_clientes_v1[,-c(1,2)]
head(dados_clientes_v2)

# Padronizando as variáveis
dados_clientes_v2_scaled = scale(dados_clientes_v2)
head(dados_clientes_v2_scaled)



#############################################################################################################################
# 5° Passo - Machine Learning
#############################################################################################################################

# Avaliando a Tendência de Cluster
# Estatística Hopkins para o conjunto de dados
# Valores > .5 significam que o dataset não é "clusterizável"
# Valores < .5 significam que o dataset é "clusterizável"
# Quanto mais próximo de zero, melhor. 

set.seed(123)
?hopkins
hopkins(dados_clientes_v2_scaled, n = nrow(dados_clientes_v2_scaled)-1)

# Valor = 0.3137406 indica que o dataset é "clusterizável".


# K-means - Determinando o Número Ideal de Clusters

# Pacote NbClust: 30 índices para determinar o número de clusters em um conjunto de dados.
# Se index = 'all' - Executa 30 índices para determinar o número ideal de clusters.
# Se index = "silhouette" - Usa uma medida para estimar a diferença entre clusters.
# Um valor de silhueta mais alto é preferido para determinar o número ideal de clusters

# Opção 1:
?NbClust
num_clusters_opt1 <- NbClust(dados_clientes_v2_scaled,  
                             distance = "euclidean",
                             min.nc = 2, 
                             max.nc = 15, 
                             method = "kmeans",
                             index = "silhouette")

num_clusters_opt1$Best.nc
num_clusters_opt1$All.index


# Opção 2:
num_clusters_opt2 <- NbClust(dados_clientes_v2_scaled,  
                             distance = "euclidean", 
                             min.nc = 2, 
                             max.nc = 15, 
                             method = "kmeans",
                             index = "all")

# Um método recomenda 6 clusters e o outro recomenda 2. Usaremos 4 clusters 
# (Decisão do Cientista de Dados e da Equipe de Negócios).


# Criação do Modelo K-Means e Análise de Cluster
?kmeans
modelo <- kmeans(dados_clientes_v2_scaled, 4)
print(modelo)

# Verificando o tamanho dos clusters
modelo$size

# Verificando o centro dos clusters
modelo$centers


#############################################################################################################################
# 5° Passo - Entrega dos Resultados 
#############################################################################################################################


# Aplicando o ID dos clusters ao dataframe original
modelo$cluster
dados_clientes_v1$Cluster <- modelo$cluster
View(dados_clientes_v1)

# Média de idade por cluster
?aggregate
aggregate(data = dados_clientes_v1, Idade ~ Cluster, mean)

# Média de salário por cluster
aggregate(data = dados_clientes_v1, Salario_Mensal_Milhar ~ Cluster, mean)

# Média de pontuação de gasto por cluster
aggregate(data = dados_clientes_v1, Pontuacao_Gasto ~ Cluster, mean)

# Média de idade e salário por cluster
aggregate(data = dados_clientes_v1, cbind(Idade, Salario_Mensal_Milhar) ~ Cluster, mean)

# Total de pessoas por sexo e por cluster
with(dados_clientes_v1, table(Sexo, Cluster))

# Plot do Modelo
plot(dados_clientes_v2_scaled, col = modelo$cluster, pch = 15) 

# Melhorando a Visualização
?eclust
cluster_viz <- eclust(dados_clientes_v2_scaled, "kmeans", k = 4, nstart = 25, graph = FALSE)

# Visualizando Clusters K-Means
?fviz_cluster
fviz_cluster(cluster_viz, geom = "point", ellipse.type = "norm")

# Outra opção de visualização
?clusplot
clusplot(dados_clientes_v1, dados_clientes_v1$Cluster, color=TRUE, shade=TRUE, labels=5, lines=0)



#############################################################################################################################
                                                        # FIM #
#############################################################################################################################


