# ==============================================================================
# SISMÓGRAFO POLÍTICO - ETL 02: PROCESSAMENTO DE NLP (DISCURSOS)
# ==============================================================================
# Descrição:
# Lê arquivos brutos de discursos, aplica Regex para contar termos-chave
# e consolida tudo em um arquivo leve (.parquet) para o dashboard.
# ==============================================================================

library(tidyverse)
library(arrow)
library(stringi)
library(lubridate)

# --- CONFIGURAÇÃO ---
MODO_TESTE <- FALSE  # Deixe FALSE para rodar a base completa

print("🚑 INICIANDO PROCESSAMENTO DE TEXTO (NLP)...")

# 1. CAMINHOS
# Assume execução na raiz do projeto
DIR_RAW    <- file.path("dados_discursos_brutos") 
DIR_PROC   <- file.path("dados_processados")
FILE_CUBO  <- file.path(DIR_PROC, "cubo_discursos.parquet")

if(!dir.exists(DIR_PROC)) dir.create(DIR_PROC)

# 2. MAPA DE BUSCA (DEFINIÇÃO EXPLÍCITA)
# IDs hardcoded para garantir a integridade das colunas
mapa_completo <- tribble(
  ~id,              ~termo_bonito,            ~termos_busca,
  # --- POLÍTICA (IDs n_1 a n_23) ---
  "n_1",  "Reforma da Previdência", c("reforma da previdencia", "pec 6/2019", "pec 287", "nova previdencia"),
  "n_2",  "Reforma Tributária",     c("reforma tributaria", "pec 45", "pec 110", "iva ", "imposto sobre valor agregado"),
  "n_3",  "Teto de Gastos",         c("teto de gastos", "pec 241", "pec 55", "novo regime fiscal"),
  "n_4",  "Arcabouço Fiscal",       c("arcabouco fiscal", "lei complementar 200", "meta fiscal"),
  "n_5",  "Reforma Trabalhista",    c("reforma trabalhista", "lei 13467", "imposto sindical"),
  "n_6",  "Autonomia do BC",        c("autonomia do banco central", "independencia do bc", "lei complementar 179"),
  "n_7",  "PEC dos Precatórios",    c("pec dos precatorios", "pec 23", "calote dos precatorios"),
  "n_8",  "PEC da Transição",       c("pec da transicao", "pec 32/2022", "fura teto"),
  "n_9",  "Mensalão",               c("mensalao", "acao penal 470", "roberto jefferson", "jose dirceu"),
  "n_10", "Lava Jato",              c("lava jato", "petrolao", "sergio moro", "deltan"),
  "n_11", "Vaza Jato",              c("vaza jato", "the intercept", "glenn greenwald"),
  "n_12", "Impeachment Dilma",      c("impeachment", "pedaladas fiscais", "eduardo cunha", "fora dilma"),
  "n_13", "CPI da Covid",           c("cpi da covid", "cpi da pandemia", "renan calheiros", "omar aziz"),
  "n_14", "8 de Janeiro",           c("8 de janeiro", "atos golpistas", "invasao dos tres poderes"),
  "n_15", "Orçamento Secreto",      c("orcamento secreto", "emendas de relator", "rp9", "arthur lira"),
  "n_16", "Joias Bolsonaro",        c("joias bolsonaro", "rolex", "mauro cid"),
  "n_17", "Aborto",                 c("aborto", "adpf 442", "estatuto do nascituro"),
  "n_18", "Drogas",                 c("descriminalizacao das drogas", "pec das drogas", "porte de drogas"),
  "n_19", "Armas",                  c("estatuto do desarmamento", "cacs", "porte de armas"),
  "n_20", "Marco Temporal",         c("marco temporal", "terras indigenas", "pl 490"),
  "n_21", "Ideologia de Gênero",    c("escola sem partido", "ideologia de genero", "doutrinacao"),
  "n_22", "PL das Fake News",       c("pl das fake news", "pl 2630", "censura"),
  "n_23", "Cotas Raciais",          c("cotas raciais", "acoes afirmativas", "lei de cotas"),
  
  # --- PLACEBOS (IDs dummy explícitos) ---
  "dummy_futebol",  "Futebol",  c("futebol", "copa do mundo", "campeonato brasileiro", "selecao brasileira", "flamengo", "corinthians"),
  "dummy_enem",     "Enem",     c("enem", "exame nacional do ensino medio", "sisu", "prouni"),
  "dummy_carnaval", "Carnaval", c("carnaval", "desfile das escolas de samba", "quarta feira de cinzas", "trio eletrico")
)

# 3. LISTAR ARQUIVOS
arquivos <- list.files(DIR_RAW, pattern = "parquet", full.names = TRUE)
if(length(arquivos) == 0) stop("❌ Sem arquivos brutos na pasta dados_discursos_brutos!")

if(MODO_TESTE) arquivos <- head(arquivos, 5)

lista_processada <- list()
total <- length(arquivos)
i <- 1

# 4. LOOP DE PROCESSAMENTO (High Performance)
print(paste("📂 Processando", total, "arquivos de discursos..."))

for(arq in arquivos) {
  pct <- round((i / total) * 100, 1)
  cat(paste0("\n[", i, "/", total, "] ", pct, "% - ", basename(arq)))
  
  tryCatch({
    # Leitura otimizada (apenas colunas necessárias)
    df_raw <- read_parquet(arq, col_select = c("data_hora", "texto", "partido", "fase"))
    
    df_proc <- df_raw %>%
      mutate(
        data = as.Date(substr(data_hora, 1, 10)),
        mes = floor_date(data, "month"),
        texto_norm = stri_trans_general(str_to_lower(texto), "Latin-ASCII"), # Normalização
        partido = replace_na(str_trim(partido), "OUTROS"),
        fase_simples = case_when(
          str_detect(str_to_upper(fase), "EXPEDIENTE|COMUNICAÇÕES") ~ "TRIBUNA",
          str_detect(str_to_upper(fase), "ORDEM|ORIENTAÇÃO") ~ "VOTACAO",
          TRUE ~ "OUTROS"
        )
      ) %>%
      filter(!is.na(mes))
    
    # Matriz de contagem (Vetorizada = Mais rápida)
    matriz <- matrix(0, nrow = nrow(df_proc), ncol = nrow(mapa_completo))
    for(k in 1:nrow(mapa_completo)) {
      regex <- paste(mapa_completo$termos_busca[[k]], collapse = "|")
      matriz[, k] <- stri_count_regex(df_proc$texto_norm, regex)
    }
    
    df_contagens <- as_tibble(matriz, .name_repair = "minimal")
    colnames(df_contagens) <- mapa_completo$id 
    
    # Agrupamento Mensal (Redução de dimensionalidade)
    resumo <- bind_cols(df_proc %>% select(mes, partido, fase_simples), df_contagens) %>%
      group_by(mes, partido, fase_simples) %>%
      summarise(
        total_discursos = n(),
        across(all_of(mapa_completo$id), sum),
        .groups = "drop"
      )
    
    lista_processada[[basename(arq)]] <- resumo
    cat(" ✅")
    
  }, error = function(e) {
    cat(paste(" ❌ ERRO:", e$message))
  })
  
  rm(df_raw, df_proc, matriz, df_contagens)
  gc(verbose = FALSE) # Limpeza de memória forçada
  i <- i + 1
}

# 5. SALVAR CUBO FINAL
if(length(lista_processada) > 0) {
  print("\n\n💾 Consolidando e salvando Cubo OLAP...")
  df_final <- bind_rows(lista_processada) %>%
    group_by(mes, partido, fase_simples) %>%
    summarise(
      total_discursos = sum(total_discursos),
      across(all_of(mapa_completo$id), sum),
      .groups = "drop"
    )
  
  write_parquet(df_final, FILE_CUBO)
  print(paste("🎉 SUCESSO! Cubo salvo em:", FILE_CUBO))
  
} else {
  print("\n❌ NENHUM ARQUIVO PROCESSADO.")
}