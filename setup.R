# setup.R - Sismógrafo Político
# Instalação de dependências para ETL e Dashboard

pacotes <- c(
  # Dashboard
  "shiny", "bslib", "highcharter", "waiter", "bsicons",
  # Dados e Manipulação
  "tidyverse", "arrow", "lubridate", "stringi",
  # APIs e Web
  "gtrendsR"
)

# Instalação automática
novos <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if(length(novos)) install.packages(novos)

message("✅ Ambiente do Sismógrafo configurado!")