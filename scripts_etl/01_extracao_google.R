# ==============================================================================
# SISMÓGRAFO POLÍTICO - ETL 01: EXTRAÇÃO DO GOOGLE TRENDS
# ==============================================================================
# Descrição:
# Baixa o volume de buscas mensal para termos políticos e de controle.
# Possui sistema "blindado" contra erro 429 (bloqueio de IP) e cache local.
# ==============================================================================

library(tidyverse)
library(gtrendsR)
library(arrow)
library(lubridate)
library(stringi)

print("🛡️ INICIANDO DOWNLOAD BLINDADO (GOOGLE TRENDS)...")

# 1. CONFIGURAÇÃO DE PASTAS
# Assume que o script está rodando na raiz do projeto (.Rproj)
path_google <- file.path("dados_google")
path_cache  <- file.path(path_google, "cache")
path_final  <- file.path(path_google, "google_FINAL.parquet")

# Garante que as pastas existem
if(!dir.exists(path_google)) dir.create(path_google)
if(!dir.exists(path_cache))  dir.create(path_cache)

print(paste("💾 Cache definido em:", path_cache))

# 2. LISTA DE TERMOS (POLÍTICA + PLACEBOS)
lista_termos <- c(
  # --- Pautas Econômicas e Reformas ---
  "Reforma da Previdência", "Reforma Tributária", "Teto de Gastos", 
  "Arcabouço Fiscal", "Reforma Trabalhista", "Autonomia do BC", 
  "PEC dos Precatórios", "PEC da Transição",
  
  # --- Crises e Escândalos ---
  "Mensalão", "Lava Jato", "Vaza Jato", "Impeachment Dilma", 
  "CPI da Covid", "8 de Janeiro", "Orçamento Secreto", "Joias Bolsonaro",
  
  # --- Pautas de Costumes/Sociais ---
  "Aborto", "Drogas", "Armas", "Marco Temporal", 
  "Ideologia de Gênero", "PL das Fake News", "Cotas Raciais",
  
  # --- Controles (Placebos) ---
  "Futebol", "Enem", "Carnaval"
)

# 3. LOOP DE DOWNLOAD SEGURO
total <- length(lista_termos)

for (i in seq_along(lista_termos)) {
  termo <- lista_termos[i]
  
  # Cria nome de arquivo seguro (sem acentos/espaços)
  nome_safe <- stri_trans_general(termo, "Latin-ASCII") %>% 
    str_replace_all(" ", "_") %>% 
    paste0(".parquet")
  
  arquivo_cache <- file.path(path_cache, nome_safe)
  
  # Se já existe no cache, pula
  if(file.exists(arquivo_cache)) {
    cat(paste0("\n[", i, "/", total, "] ⏭️  Já no cache: ", termo))
    next
  }
  
  cat(paste0("\n[", i, "/", total, "] ⬇️  Baixando: '", termo, "'... "))
  
  sucesso <- FALSE
  tryCatch({
    # Download do Google (All Time, Brasil)
    res <- gtrends(keyword = termo, geo = "BR", time = "all")
    
    if (!is.null(res$interest_over_time)) {
      df <- res$interest_over_time %>%
        mutate(
          termo_chave = termo,
          hits = as.numeric(replace(hits, hits == "<1", 0)),
          data = floor_date(as.Date(date), "month"),
          fonte = "Interesse Popular (Google)"
        ) %>%
        group_by(data, termo_chave, fonte) %>%
        summarise(valor = mean(valor, na.rm=T), .groups="drop")
      
      write_parquet(df, arquivo_cache)
      cat("✅ Salvo!")
      sucesso <- TRUE
    } else {
      cat("⚠️ VAZIO.")
    }
  }, error = function(e) {
    cat(paste("❌ ERRO:", e$message))
    if(grepl("429", e$message)) {
      stop("\n⛔ BLOQUEIO 429 DETECTADO! Pare o script e troque o IP (use 4G).")
    }
  })
  
  # Pausa aleatória para evitar bloqueio (Human Behavior Simulation)
  if(sucesso) {
    tempo <- runif(1, 10, 15)
    Sys.sleep(tempo)
  } else {
    Sys.sleep(5)
  }
}

# 4. CONSOLIDAÇÃO FINAL
print("\n\n🧩 Juntando arquivos...")
arquivos <- list.files(path_cache, full.names = TRUE, pattern = "parquet")

if(length(arquivos) > 0) {
  df_final <- map_dfr(arquivos, read_parquet) %>%
    distinct(data, termo_chave, .keep_all = TRUE)
  
  write_parquet(df_final, path_final)
  print(paste("🎉 Base consolidada salva em:", path_final))
} else {
  print("❌ Nenhum arquivo encontrado no cache.")
}