# Autenticação

library(twitteR)

# Definindo as chaves de acesso
api_key <- "sua chave"
api_secret <- "sua chave"
access_token <- "sua chave"
access_token_secret <- "sua chave"

# Autenticando no Twitter
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

