# Funções auxiliares

# Function para limpeza dos tweets
limpaTweets <- function(tweet){
  # Remove http links
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("http\\w+", "", tweet)
  # Remove retweets
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Remove “#Hashtag”
  tweet = gsub("#\\w+", " ", tweet)
  # Remove nomes de usuarios “@people”
  tweet = gsub("@\\w+", " ", tweet)
  # Remove pontuacão
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Remove os números
  tweet = gsub("[[:digit:]]", " ", tweet)
  # Remove espacos desnecessários
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # Convertendo encoding de caracteres e convertendo para letra minúscula
  tweet <- stringi::stri_trans_general(tweet, "latin-ascii")
  tweet <- tryTolower(tweet)
  tweet <- iconv(tweet, from = "UTF-8", to = "ASCII")
}

# Function para limpeza de Corpus
limpaCorpus <- function(myCorpus){
  library(tm)
  myCorpus <- tm_map(myCorpus, tolower)
  # Remove pontuação
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # Remove números
  myCorpus <- tm_map(myCorpus, removeNumbers)
}

# Converte para minúsculo
tryTolower = function(x)
{
  # Cria um dado missing (NA)
  y = NA
  # faz o tratamento do erro
  try_error = tryCatch(tolower(x), error=function(e) e)
  # se não der erro, transforma em minúsculo
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # Retorna o resultado
  return(y)
}


