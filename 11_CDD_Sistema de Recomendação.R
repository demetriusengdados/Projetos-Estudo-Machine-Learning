###########################################################################################################
#                                                                                                         #
#                                          Ciência dos Dados                                              #
#                                                                                                         #
#                                     Sistema de Recomendação em R                                        #
#                                                                                                         #
#                           Recomendação de Filmes com base em usuários semelhantes                       #        
#                                                                                                         #
###########################################################################################################


# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Definindo o diretório de trabalho
getwd()
setwd("")

###########################################################################################################
# Passos que devem ser executados na construção do sistema de recomendação

# 1° Carregar e formatar dados.
# 2° Calcular a similaridade entre usuários. 
# 3° Prever das classificações desconhecidas para os usuários. 
# 4° Recomendar itens aos usuários com base na pontuação de similaridade de usuário.

###########################################################################################################

# O objetivo deste sistema de recomendação é oferecer filmes para usuários com base em usuários semelhantes

###########################################################################################################

# Pacotes
library(reshape2)
library(data.table)
library(dplyr)

# Esse pacote é uma junção dos pacotes data.table e dplyr
install.packages("dtplyr")
library(dtplyr)

# Carregando os Dados
# Esse dataset contém as avaliações de 6 filmes dadas por 6 usuários diferentes. 
# A escala de avaliação vai de 0 a 5
# Nem todos os usuários deram a avaliação a todos os filmes

ratings = read.csv("movie_ratings.csv")
dim(ratings)
str(ratings)
levels(ratings$critic)
levels(ratings$title)
View(ratings)

###########################################################################################################
# 1° Passo
###########################################################################################################


# Formatando e processando os dados
# Precisamos agora converter os dados em um formato de matriz, sendo:
# Usuário (critic) como colunas
# Filmes (title) como linhas
# Avaliações (ratings) como valores na interseção entre usuário e filmes 

?acast
movie_ratings = as.data.frame(acast(ratings, title~critic, value.var = "rating"))
movie_ratings
View(movie_ratings)

# Podemos perceber que o usuário Bruno Oliveira ainda não avaliou alguns filmes, pois provavelmente ele ainda não assistiu esses filmes
# Nosso sistema de recomendação deve recomendar filmes para usuários como Bruno Oliveira, com base em usuários similares

###########################################################################################################
# 2° Passo
###########################################################################################################

# Calculando a Similaridade
# Podemos usar medidas de distância como distância euclidiana, distância cosine ou coeficiente de Pearson para correlação
# O parâmetro "complete.obs" considera todas as observações no dataset
?cor
sim_users = cor(movie_ratings[,1:6], use = "complete.obs")
View(sim_users)

# Verificando a similaridade
sim_users[,2]

# Prevendo valores desconhecidos (filmes que poderão ser recomendados)
# Separando filmes ainda não avaliados
?setDT
rating_critic  = setDT(movie_ratings[colnames(movie_ratings)[2]],keep.rownames = TRUE)[]
names(rating_critic) = c('title','rating')

# Isolando filmes ainda não avaliados
titles_na_critic = rating_critic$title[is.na(rating_critic$rating)]
ratings_t = ratings[ratings$title %in% titles_na_critic,]
View(ratings_t)

# Adicionando valores de similaridade para cada usuário como uma nova variável
x = (setDT(data.frame(sim_users[,6]),keep.rownames = TRUE)[])
names(x) = c('critic','similarity')
ratings_t =  merge(x = ratings_t, y = x, by = "critic", all.x = TRUE)
View(ratings_t)

# Multiplicando ratings e valores de similaridade
ratings_t$sim_rating = ratings_t$rating * ratings_t$similarity
View(ratings_t)

###########################################################################################################
# 3° Passo
###########################################################################################################


# Prevendo títulos ainda não avaliados
result = ratings_t %>% group_by(title) %>% summarise(sum(sim_rating)/sum(similarity))
result

# Mas qual dos 3 filmes recomendar primeiro? 
# Aquele com score maior que a média de ratings do usuário Bruno Oliveira
mean(rating_critic $ rating, na.rm = T)

# Função para sistema de recomendação (une os itens realizados nos comandos anteriores) 
generateRecommendations <- function(userId){
  rating_critic  = setDT(movie_ratings[colnames(movie_ratings)[userId]],keep.rownames = TRUE)[]
  names(rating_critic) = c('title','rating')
  titles_na_critic = rating_critic$title[is.na(rating_critic$rating)]
  ratings_t =ratings[ratings$title %in% titles_na_critic,]
  x = (setDT(data.frame(sim_users[,userId]),keep.rownames = TRUE)[])
  names(x) = c('critic','similarity')
  ratings_t =  merge(x = ratings_t, y = x, by = "critic", all.x = TRUE)
  ratings_t$sim_rating = ratings_t$rating*ratings_t$similarity
  result = ratings_t %>% group_by(title) %>% summarise(sum(sim_rating)/sum(similarity))
  return(result)
}

###########################################################################################################
# 4° Passo
###########################################################################################################

# Gerando recomendações
generateRecommendations(1)
generateRecommendations(2)
generateRecommendations(3)
generateRecommendations(5)


ojg
###########################################################################################################
#                                          FIM
###########################################################################################################



