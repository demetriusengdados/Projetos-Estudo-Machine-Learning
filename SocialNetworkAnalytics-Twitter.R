###########################################################################################################
#                                                                                                         #
#                                          Ciência dos Dados                                              #
#                                                                                                         #
#                                          Twitter Analytics                                              #
#                                                                                                         #
#                           Usando o Twitter para fazer análise de dados                                  #        
#                                                                                                         #
###########################################################################################################


# Social Network Analytics - Twitter
# Comparando dados de diferentes temas : Machine Learning x DataScience

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/Users/user/Videos/Ciência dos Dados/Cursos/YouTube/15 - Twitter - Social Network Analytics")
getwd()

# Carrega biblioteca com functions auxiliares e autenticação
source('utils.R')
source('autentication.R')
1 # Selecione o número 1 (yes) e dá um run.

# Instalando os pacotes
install.packages(c("devtools", "httr"))
install.packages(c("tm", "wordcloud", "RColorBrewer"))
install.packages(c("twitteR", "ROAuth"))

# Carregando os pacotes
library(devtools)       # pacote com ferramentas diversas de análise de dados
library(twitteR)        # Conexão no Twitter
library(ROAuth)         # PAcote de Autenticação
library(tm)             # Text Mining
library(wordcloud)      # Gráfico de Nuvem
library(RColorBrewer)   # Gráfico
library(stringr)        # Manipulação de textos

###############################################################################
# Coletando os tweets
###############################################################################

?searchTwitter
bigdata_tweets = searchTwitter("BigData", n = 100, lang = "pt")
datascience_tweets = searchTwitter("DataScience", n = 100, lang = "pt")
dsa_Tweets = userTimeline(getUser('dsacademybr'), n = 100)

# Print
head(bigdata_tweets)
class(bigdata_tweets)

head(datascience_tweets)
class(datascience_tweets)

head(dsa_Tweets)
class(dsa_Tweets)

###############################################################################
# Convertendo os tweets para texto
###############################################################################

#getText é do pacote BASE. Não precisa chamar pois ao abrir a sessão ele já roda.

textos_dsa = sapply(dsa_Tweets, function(x) x$getText())
head(textos_dsa)

textos_bigdata = sapply(bigdata_tweets, function(x) x$getText())
head(bigdata_tweets)

textos_datascience = sapply(datascience_tweets, function(x) x$getText())
head(datascience_tweets)

textos_bigdata[1:10]
class(textos_bigdata)

###############################################################################
# Limpeza dos tweets
###############################################################################

textos_dsa_limpo = textos_dsa
textos_dsa_limpo <- limpaTweets(textos_dsa_limpo)
head(textos_dsa_limpo)
names(textos_dsa_limpo) = NULL
textos_dsa_limpo = textos_dsa_limpo[textos_dsa_limpo != ""]
textos_dsa_limpo[1:10]
class(textos_dsa_limpo)

textos_bigdata_limpo = textos_bigdata
textos_bigdata_limpo <- limpaTweets(textos_bigdata_limpo)
head(textos_bigdata_limpo)
names(textos_bigdata_limpo) = NULL
textos_bigdata_limpo = textos_bigdata_limpo[textos_bigdata_limpo != ""]
textos_bigdata_limpo[1:10]
class(textos_bigdata_limpo)

textos_datascience_limpo = textos_datascience
textos_datascience_limpo <- limpaTweets(textos_datascience_limpo)
head(textos_datascience_limpo)
names(textos_datascience_limpo) = NULL
textos_datascience_limpo = textos_datascience_limpo[textos_datascience_limpo != ""]
textos_datascience_limpo[1:10]
class(textos_datascience_limpo)



###############################################################################
# Converte para Corpus
###############################################################################

tweetcorpus_dsa <- Corpus(VectorSource(textos_dsa_limpo))
tweetcorpus_dsa <- limpaCorpus(tweetcorpus_dsa)

tweetcorpus_bigdata <- Corpus(VectorSource(textos_bigdata_limpo))
tweetcorpus_bigdata <- limpaCorpus(tweetcorpus_bigdata)

tweetcorpus_datascience <- Corpus(VectorSource(textos_datascience_limpo))
tweetcorpus_datascience <- limpaCorpus(tweetcorpus_datascience)

# Converte o texto para a matriz de termos
termo_por_documento_dsa         = as.matrix(TermDocumentMatrix(tweetcorpus_dsa), control = list(stopwords = c(stopwords("portuguese"))))
termo_por_documento_bigdata     = as.matrix(TermDocumentMatrix(tweetcorpus_bigdata), control = list(stopwords = c(stopwords("portuguese"))))
termo_por_documento_datascience = as.matrix(TermDocumentMatrix(tweetcorpus_datascience), control = list(stopwords = c(stopwords("portuguese"))))

# Verifica os primeiros 10 termos (linhas) com os primeiros 10 documentos (colunas)
termo_por_documento_dsa[1:10,1:10]
termo_por_documento_bigdata[1:10,1:10]
termo_por_documento_datascience[1:10,1:10]

###############################################################################
# Calcula a frequência de cada termo ao somar cada linha e coloca em ordem decrescente
###############################################################################

frequencia_dos_termos_dsa = sort(rowSums(termo_por_documento_dsa), decreasing = TRUE) 
head(frequencia_dos_termos_dsa)

frequencia_dos_termos_bigdata = sort(rowSums(termo_por_documento_bigdata), decreasing = TRUE) 
head(frequencia_dos_termos_bigdata)

frequencia_dos_termos_datascience = sort(rowSums(termo_por_documento_datascience), decreasing = TRUE) 
head(frequencia_dos_termos_datascience)

# Cria um dataframe com o termo (palavra) e sua respectiva frequência 
df_dsa = data.frame(termo = names(frequencia_dos_termos_dsa), frequencia = frequencia_dos_termos_dsa) 
df_bigdata = data.frame(termo = names(frequencia_dos_termos_bigdata), frequencia = frequencia_dos_termos_bigdata) 
df_datascience = data.frame(termo = names(frequencia_dos_termos_datascience), frequencia = frequencia_dos_termos_datascience) 

# Remove o termo mais frequente
df_dsa = df_dsa[-1,]
class(df_dsa)
df_bigdata = df_bigdata[-1,]
class(df_bigdata)
df_datascience = df_datascience[-1,]
class(df_datascience)

###############################################################################
# Desenha a nuvem de palavras
###############################################################################


wordcloud(df_dsa$termo, 
          df_dsa$frequencia, 
          max.words = 100,
          min.freq = 2,
          scale = c(3,.5),
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))


wordcloud(df_bigdata$termo, 
          df_bigdata$frequencia, 
          max.words = 100,
          min.freq = 2,
          scale = c(3,.5),
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

wordcloud(df_datascience$termo, 
          df_datascience$frequencia, 
          max.words = 100,
          min.freq = 3,
          scale = c(3,.5),
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Merge dos dataframes
df_merge <- merge(df_dsa, df_datascience, by = "termo")
head(df_merge)

df_merge$freq_total <- df_merge$frequencia.x + df_merge$frequencia.y 
head(df_merge)

# Wordcloud do novo dataframe
wordcloud(df_merge$termo, 
          df_merge$freq_total, 
          max.words = 100,
          min.freq = 2,
          scale = c(3,.5),
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))


#https://tarciziosilva.com.br/blog/o-que-se-esconde-por-tras-de-uma-nuvem-de-palavras/
######################################################################################
#                                   FIM
######################################################################################








