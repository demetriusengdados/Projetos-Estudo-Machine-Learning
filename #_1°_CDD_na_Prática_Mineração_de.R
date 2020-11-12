###############################################################################################################
#                 Ciência dos Dados -  #naPrática - Mineração de Regra de Associação - MBA
###############################################################################################################

# Embora tenha mais de 20 anos, o Market Basket Analysis (MBA) (ou Mineração de Regras de Associação) 
# ainda pode ser uma técnica muito útil para obter insights em grandes conjuntos de dados transacionais. 

# O exemplo clássico são dados transacionais em um supermercado. Para cada cliente, sabemos quais são os produtos 
# individuais (itens) que ele colocou na cesta e comprou. Outros casos de uso para o MBA podem ser dados de clique da web, 
# arquivos de log e até questionários.

# Neste CDD na Pratica, vamos buscar a associação entre os clubes de futebol da Europa e responder a pergunta:

## Quais clubes mais realizam transações de compra e venda de jogadores, entre si?

# Usaremos um dataset oferecido pelo Kaggle: https://www.kaggle.com/hugomathien/soccer

# O dataset contêm cerca de 25.000 partidas de onze ligas de futebol europeias a partir da temporada 2008/2009 
# até a temporada 2015/2016. 

# Depois de realizar o trabalho de Data Wrangling, vamos gerar um conjunto de dados transacionais 
# adequado para análise de cesta de compras.

# Portanto, não temos clientes, mas jogadores de futebol, e não temos produtos, mas clubes de futebol. 

# No total, o conjunto de dados transacionais de futebol contém cerca de 18.000 registros. 
# Obviamente, esses registros não incluem apenas as transferências multimilionárias cobertas pela mídia, 
# mas também todas as transferências de jogadores que ninguém nunca ouviu falar.

# Como vamos aplicar o MBA?
# Em R você pode usar o pacote arules para mineração de regras de associação / MBA. 
# Alternativamente, quando a ordem das transações é importante, você deve usar o pacote arulesSequences. 
# Depois de executar o algoritmo, obteremos alguns resultados interessantes. 
  
# Por exemplo: neste conjunto de dados, a transferência mais frequente é da Fiorentina para o Gênova 
# (12 transferências no total). Vamos imprimir a tabela com todos os resultados ao final do processo.

# Visualização de gráfico de rede
# Todas as regras que obtemos da mineração de regras de associação formam um gráfico de rede. 
# Os clubes de futebol individuais são os nós do gráfico e cada regra "de ==> para" 
# é uma aresta (edge) do gráfico de rede.

# Em R, os gráficos de rede podem ser visualizados bem por meio do pacote visNetwork. 

# Vamos lá?
###############################################################################################################

# Não use diretórios com espaço no nome
setwd("C:")
getwd()

# Pacotes
install.packages("RSQLite")
install.packages("dplyr")
install.packages("tidyr")
install.packages("arules")
install.packages("arulesSequences")
install.packages("readr")
install.packages("visNetwork")
install.packages("igraph")
install.packages("lubridate")
install.packages("DT")

library(RSQLite)         # Conectar à fonte de dados
library(dplyr)           # Manipulação dos dados
library(tidyr)
library(arules)          # Machine Learning
library(arulesSequences)
library(readr)           # Gravar arquivo intermediário em .txt
library(stringr)
library(visNetwork)      # Visualização de Gráficos
library(igraph)
library(lubridate)       # Manipulação de Datas  
library(DT)              # Manipulação de tabela


# Os dados estão disponibilizados em um banco de dados SQLITE 
# que pode ser baixado do kaggle, mas está anexo a este script.

# Conectando no banco de dados
con = dbConnect(RSQLite::SQLite(), dbname="database.sqlite")


# Obtendo a lista de tabelas
alltables = dbListTables(con)
alltables


# Extraindo as tabelas
players       = dbReadTable(con, "Player")
players_stats = dbReadTable(con, "Player_Attributes")
teams         = dbReadTable(con, "Team")
league        = dbReadTable(con, "League")
Matches       = dbReadTable(con, "Match")

View(players)
View(players_stats)
View(teams)
View(league)
View(Matches)


# Substituindo espaço por underline nos nome muito longos
teams$team_long_name = str_replace_all(teams$team_long_name, "\\s", "_")
teams$team_long_name = str_replace_all(teams$team_long_name, "\\.", "_")
teams$team_long_name = str_replace_all(teams$team_long_name, "-", "_")
View(teams)

# Agrupando as equipes por país
CountryClub = Matches %>% 
  group_by(home_team_api_id,country_id) %>% 
  summarise(n=n()) %>% 
  left_join(league) %>%
  left_join(teams, by=c("home_team_api_id" = "team_api_id"))
View(CountryClub)

# Preparando os dados para mineração das regras de associação

# Os jogadores estão em colunas separadas, mas precisamos deles empilhados em uma coluna
tmp = Matches %>% 
  select(
    season, 
    home_team_api_id, 
    home_player_1:home_player_11
  )%>%
  gather(
    player, 
    player_api_id, 
    -c(season, home_team_api_id)
  ) %>%
  group_by(player_api_id, home_team_api_id ) %>% 
  summarise(season = min(season))
View(tmp)


# Unindo dados de jogador e clube
playerClubSequence = left_join(
  tmp,
  players
  ) %>% 
  left_join(
    teams, 
    by=c("home_team_api_id"="team_api_id")
  )
View(playerClubSequence)

playerClubSequence = playerClubSequence %>% 
  filter(
    !is.na(player_name), !is.na(team_short_name)
  )  %>%
  arrange(
    player_api_id, 
    season
  )
View(playerClubSequence)

# Adicionando um número sequencial por jogador
playerClubSequence$seqnr = ave( playerClubSequence$player_api_id, playerClubSequence$player_api_id, FUN = seq_along)
playerClubSequence$size = 1

View(playerClubSequence)


# Mineração de sequências com algoritmo cSPade do pacote arulesSequences

# Grava o conjunto de dados em um arquivo txt para facilitar a manipulação 
# da função read_basket em arulesSequence para criar um objeto de transação
write_delim( 
  playerClubSequence %>% select( c(player_api_id, seqnr, size, team_long_name)) ,
  delim ="\t", path = "player_transactions.txt", col_names = FALSE
  )


# Agora importamos as transações registradas no item anterior
playerstrxs <- read_baskets("player_transactions.txt", sep = "[ \t]+",info =  c("sequenceID","eventID","size"))
View(playerstrxs)
typeof(playerstrxs)
summary(playerstrxs)

# Executar mineração de sequência, por enquanto apenas com comprimento de duas sequências
?cspade

playersClubSeq <- cspade(
  playerstrxs, 
  parameter = list(support = 0.00010, maxlen=2),  # maxlen = 2 sequencias (quanto mais sequencias mais consumo computacional)
  control   = list(verbose = TRUE)
)

summary(playersClubSeq)

# Fazendo Data Wrangling para colocar os resultados do cspade em um organizado conjunto de dados 
# que é adequado para a visNetwork. A visNetwork precisa de dois conjuntos de dados:
# um conjunto de dados com as arestas "de --> para" e um conjunto de dados com os nós exclusivos
seqResult = as(playersClubSeq, "data.frame")
seqResult = seqResult %>% 
  mutate(
    sequence = as.character(sequence)
  )

View(seqResult)

seqResult = bind_cols(
  seqResult,
  as.data.frame(
    str_split_fixed(seqResult$sequence, pattern =",", 2), 
    stringsAsFactors = FALSE)
  )

View(seqResult)


seqResult$from = str_extract_all(seqResult$V1,"\\w+", simplify = TRUE)[,1] 
seqResult$to   = str_extract_all(seqResult$V2,"\\w+",simplify = TRUE)[,1]

View(seqResult)


seqResult$width = exp(3000*seqResult$support)
seqResult = seqResult %>% filter(V2 !="")
seqResult$title = paste(seqResult$sequence, "<br>", round(100*seqResult$support,2), "%")

seqResult$support_perc = paste(sprintf("%.4f", 100*seqResult$support), "%")

View(seqResult)

# Criando o dataframe com os nodes
nodes = unique(c(seqResult$from, seqResult$to))

nodesData = data.frame(id = unique(nodes), title = unique(nodes), label = unique(nodes), stringsAsFactors = FALSE) %>%
  left_join(CountryClub, by = c("id"="team_long_name")) %>% 
  rename(group = name)

View(nodes)

# Calcula as medidas de centralidade de betweeness
# usando o igraph, para que possamos ter tamanhos diferentes de
# nós no gráfico de rede
transferGraph = graph_from_data_frame(seqResult[,c(5,6)], directed = TRUE)

tmp = betweenness(transferGraph)
Clubs_betweenness = data.frame(id = names(tmp), value = tmp, stringsAsFactors = FALSE)
View(Clubs_betweenness)

nodesData = nodesData %>% 
  left_join(Clubs_betweenness) %>%
  mutate(title = paste(id, "betweeness ", round(value))) %>%
  arrange(id)

View(nodesData)

# Criando a rede interativa

# Preparando o dataframe final e removendo duplicidades
nodes = nodesData
nodes = nodes[!duplicated(nodes$id),]


# Cria a rede
?visNetwork

visNetwork(nodes, edges = seqResult, width = 900, height = 700) %>%
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%

    visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000)
  )
  

# Cria a tabela final para suportar a análise
seqResult$Ntransctions = seqResult$support*10542
DT::datatable(
  seqResult[,c(5,6,9,10)], 
  rownames = FALSE,
  options = list(
    pageLength=25)
  )



