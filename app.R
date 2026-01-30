library(shiny)
library(bslib)
library(tidyverse)
library(arrow)
library(lubridate)
library(scales)
library(stringi)
library(bsicons)
library(waiter)
library(plotly) 

# ==============================================================================
# 0. CONFIGURAÇÃO (USANDO OS ARQUIVOS "FINAL")
# ==============================================================================
ARQUIVO_CUBO  <- "dados_processados/cubo_discursos.parquet"
ARQUIVO_DICIO <- "dados_processados/dicionario_FINAL.parquet" # Novo arquivo limpo
ARQ_GOOGLE    <- "dados_google/google_FINAL.parquet"          # Novo arquivo limpo

PARTIDOS_LISTA <- c("TODOS", "PT", "PSDB", "PMDB", "MDB", "PL", "PP", "PSOL", "DEM", "PFL", "PSB", "PDT", "PCdoB", "NOVO", "PSL", "PR", "PSD", "PTB", "UNIÃO")
FASES_OPCOES   <- c("Todas as Fases" = "TODAS", "🎙️ Tribuna Livre (Expediente)" = "TRIBUNA", "⚔️ Votação e Ordem do Dia" = "VOTACAO")

# ==============================================================================
# 1. UI
# ==============================================================================
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  fillable = FALSE, 
  title = div(
    style = "display: flex; align-items: center;", 
    img(src = "favicon_light.png", height = "45px", style = "margin-right: 15px;"), 
    div(div("Ars Metrica", style = "font-weight: 800; font-size: 1.1em; line-height: 1; color: #2c3e50;"),
        div("Sismógrafo Político & Social", style = "font-weight: 400; font-size: 0.8em; color: #7f8c8d;"))
  ),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon_light.png"),
    tags$style(HTML(".navbar, .navbar-static-top, header.navbar { background-color: #FFFFFF !important; border-bottom: 1px solid #e0e0e0 !important; } .kpi-discreto { background-color: #FFFFFF !important; border: 1px solid #ecf0f1 !important; border-left: 4px solid #2c3e50 !important; box-shadow: none !important; border-radius: 4px !important; color: #2c3e50 !important; padding: 5px 10px !important; min-height: 0 !important; } .kpi-discreto .value-box-value { font-size: 1.5rem !important; font-weight: 700; margin-bottom: 0 !important; line-height: 1.2; } .kpi-discreto .value-box-title { font-size: 0.75rem !important; text-transform: uppercase; letter-spacing: 1px; color: #7f8c8d !important; margin-bottom: 0 !important; } .accordion-button { font-weight: 600; color: #2c3e50; } @keyframes pulse { 0% { transform: scale(1); opacity: 1; } 50% { transform: scale(1.05); opacity: 0.8; } 100% { transform: scale(1); opacity: 1; } }"))
  ),
  use_waiter(), 
  waiter_show_on_load(html = tagList(img(src = "favicon_light.png", height = "180px", style = "margin-bottom: 20px; animation: pulse 2s infinite; filter: drop-shadow(0px 5px 5px rgba(0,0,0,0.1));"), h4("Ars Metrica", style = "color: #2c3e50; font-family: sans-serif; font-weight: 600; letter-spacing: 2px;"), div("Carregando o Sismógrafo...", style = "color: #7f8c8d; font-size: 0.9em; margin-top: 10px;")), color = "#FFFFFF"),
  
  sidebar = sidebar(
    title = "Filtros de Análise",
    selectInput("cesta", "Escolha a Cesta:", choices = c("Carregando..."), selected = NULL),
    selectInput("termo", "Termo Monitorado:", choices = c("Carregando..."), selected = NULL),
    hr(),
    selectInput("partido", "Recorte Partidário:", choices = PARTIDOS_LISTA, selected = "TODOS"),
    selectInput("contexto", "Contexto Legislativo:", choices = FASES_OPCOES, selected = "TODAS"),
    dateRangeInput("periodo", "Período:", start = "2019-01-01", end = Sys.Date(), min = "2003-01-01", max = Sys.Date(), format = "dd/mm/yyyy", language = "pt-BR", separator = " - "),
    hr(),
    actionButton("buscar", "ATUALIZAR DADOS", class = "btn-primary w-100", style = "font-weight: bold; margin-top: 10px;"),
    div(class = "mt-auto", style = "padding-top: 20px; text-align: center;", hr(style = "margin: 10px 0; border-top: 1px solid #e0e0e0;"), div(style = "color: #7f8c8d; font-size: 0.8em; line-height: 1.4;", span("Powered by", style = "font-weight: 300;"), br(), span(icon("microchip"), " AUTOMATA", style = "font-family: monospace; font-weight: bold; color: #2c3e50; letter-spacing: 1px;")))
  ),
  
  div(class = "main-content-wrapper", style = "padding-bottom: 50px;", 
      layout_columns(fill = FALSE, value_box(title = "Correlação (Sincronia)", value = textOutput("correlacao"), showcase = bs_icon("activity"), class = "kpi-discreto"), value_box(title = "Pico Político", value = textOutput("pico_camara"), showcase = bs_icon("megaphone-fill"), class = "kpi-discreto"), value_box(title = "Pico Popular", value = textOutput("pico_google"), showcase = bs_icon("search-heart-fill"), class = "kpi-discreto")),
      br(),
      card(fill = FALSE, style = "min-height: 500px;", card_header("Análise Cruzada: Discurso Parlamentar vs. Interesse Popular"), plotlyOutput("grafico_principal", height = "450px"), card_footer(textOutput("status_msg"), style = "font-size: 0.8em; color: #999; border-top: 1px solid #f0f0f0;")),
      br(),
      accordion(open = FALSE, accordion_panel(title = "📘 Notas Metodológicas: Entenda o Gráfico", icon = bs_icon("info-circle"), div(style = "color: #34495e; font-size: 0.95em;", h5("1. O Índice de Intensidade (0 a 1)", style = "font-weight: bold; margin-top: 10px;"), p("Aplicamos uma normalização Min-Max. O valor 1.0 representa o pico máximo histórico do termo."), h5("2. Fontes de Dados", style = "font-weight: bold;"), p("Parlamento: Dados Abertos da Câmara (Taquigrafia). Interesse Popular: Google Trends Brasil."), hr(), h5("3. Cálculo Matemático", style = "font-weight: bold;"), p("Primeiro, calculamos a frequência do termo a cada 1.000 discursos. Depois, normalizamos esse valor numa escala de 0 a 1 relativo ao histórico do próprio termo."))))
  )
)

# ==============================================================================
# 2. SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  base_cubo <- reactive({ req(file.exists(ARQUIVO_CUBO)); read_parquet(ARQUIVO_CUBO) })
  base_dicio <- reactive({ req(file.exists(ARQUIVO_DICIO)); read_parquet(ARQUIVO_DICIO) })
  
  observe({ Sys.sleep(0.5); waiter_hide() })
  
  # Menu Cascata
  observe({
    req(file.exists(ARQUIVO_DICIO))
    dicio <- base_dicio()
    categorias_disp <- sort(unique(dicio$categoria))
    updateSelectInput(session, "cesta", choices = categorias_disp, selected = categorias_disp[1])
  })
  
  observeEvent(input$cesta, {
    req(file.exists(ARQUIVO_DICIO))
    dicio <- base_dicio()
    termos_filtrados <- dicio %>% filter(categoria == input$cesta) %>% pull(termo_bonito) %>% sort()
    updateSelectInput(session, "termo", choices = termos_filtrados)
  })
  
  dados_processados <- eventReactive(input$buscar, {
    withProgress(message = 'Consultando base...', value = 0, {
      termo_selecionado <- input$termo
      dicio <- base_dicio()
      id_coluna <- dicio$id[dicio$termo_bonito == termo_selecionado]
      
      # DETECÇÃO DE PLACEBO (DUMMY)
      eh_dummy <- length(id_coluna) > 0 && str_detect(id_coluna, "dummy")
      
      if(length(id_coluna) == 0) { 
        showNotification("Erro: Termo não encontrado.", type="error"); return(NULL) 
      }
      
      # --- PARTE 1: CÂMARA ---
      df <- base_cubo()
      if(input$partido != "TODOS") df <- df %>% filter(partido == input$partido)
      if(input$contexto != "TODAS") df <- df %>% filter(fase_simples == input$contexto)
      
      data_ini <- floor_date(input$periodo[1], "month"); data_fim <- ceiling_date(input$periodo[2], "month")
      df <- df %>% filter(mes >= data_ini & mes <= data_fim)
      
      df_camara <- NULL
      if(nrow(df) > 0) {
        df_camara <- df %>% group_by(mes) %>% summarise(n_termo = sum(!!sym(id_coluna), na.rm=TRUE), total_discursos = sum(total_discursos, na.rm=TRUE)) %>%
          mutate(valor = if_else(total_discursos > 0, (n_termo/total_discursos)*1000, 0), fonte = if(input$partido == "TODOS") "Parlamento (Discursos)" else paste("Bancada", input$partido)) %>% select(data = mes, valor, fonte)
      } else {
        # Se for dummy, gera linha zerada
        df_camara <- tibble(
          data = seq(data_ini, data_fim, by="month"),
          valor = 0,
          fonte = "Parlamento (Discursos)"
        )
      }
      
      # --- PARTE 2: GOOGLE ---
      df_google <- NULL
      if(file.exists(ARQ_GOOGLE)) {
        try({ 
          df_full_google <- read_parquet(ARQ_GOOGLE)
          df_google <- df_full_google %>% 
            filter(termo_chave == termo_selecionado) %>% 
            filter(data >= data_ini & data <= data_fim) %>% 
            select(data, valor, fonte) %>%
            mutate(fonte = "Interesse Popular (Google)") 
        }, silent = TRUE)
      }
      
      lista_dfs <- list(df_camara, df_google); lista_dfs <- lista_dfs[!sapply(lista_dfs, is.null)]
      if(length(lista_dfs) == 0) { showNotification("Sem dados.", type = "warning"); return(NULL) }
      bind_rows(lista_dfs) %>% group_by(fonte) %>% mutate(min_v = min(valor), max_v = max(valor), valor_norm = if_else(max_v == min_v, 0.5, (valor-min_v)/(max_v-min_v))) %>% ungroup()
    })
  })
  
  output$grafico_principal <- renderPlotly({
    req(dados_processados()); df <- dados_processados()
    
    # Cores
    cores_mapa <- c("Interesse Popular (Google)" = "#C0392B") 
    nome_pol <- unique(df$fonte[df$fonte != "Interesse Popular (Google)"])
    if(length(nome_pol) > 0) cores_mapa[nome_pol] <- "#2980B9"
    
    df <- df %>% mutate(tooltip_txt = paste0("<b>Fonte:</b> ", fonte, "<br><b>Data:</b> ", format(data, "%b/%Y"), "<br><b>Intensidade:</b> ", round(valor_norm * 100, 1), "/100"))
    
    p <- ggplot(df, aes(x = data, y = valor_norm, color = fonte)) + 
      geom_area(data = subset(df, str_detect(fonte, "Google")), aes(fill = fonte), alpha = 0.15, show.legend = FALSE) + 
      geom_line(linewidth = 1.2) + 
      geom_point(aes(text = tooltip_txt), alpha = 0, size = 0.1) + 
      scale_color_manual(values = cores_mapa) + 
      scale_fill_manual(values = cores_mapa) + 
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") + 
      theme_minimal(base_size = 14) + 
      labs(x = NULL, y = NULL, color = NULL, fill = NULL) + 
      theme(legend.position="top", plot.title = element_blank(), panel.grid.minor = element_blank())
    
    fig <- ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.1, y = 1.1), hovermode = "x unified") %>% config(displayModeBar = FALSE)
    nomes_vistos <- c(); for (i in seq_along(fig$x$data)) { texto_original <- fig$x$data[[i]]$name; if (str_detect(texto_original, "Google")) { novo_nome <- "Interesse Popular (Google)" } else { novo_nome <- str_remove_all(texto_original, "\\(|,1\\)") }; fig$x$data[[i]]$name <- novo_nome; fig$x$data[[i]]$legendgroup <- novo_nome; if (novo_nome %in% nomes_vistos) { fig$x$data[[i]]$showlegend <- FALSE } else { fig$x$data[[i]]$showlegend <- TRUE; nomes_vistos <- c(nomes_vistos, novo_nome) } }
    return(fig)
  })
  
  output$correlacao <- renderText({ req(dados_processados()); df <- dados_processados(); df_pol <- df %>% filter(!str_detect(fonte, "Google")); df_web <- df %>% filter(str_detect(fonte, "Google")); if(nrow(df_pol) < 3 || nrow(df_web) < 3) return("N/A"); df_join <- inner_join(df_pol, df_web, by="data"); if(nrow(df_join) < 3) return("N/A"); paste0(round(cor(df_join$valor_norm.x, df_join$valor_norm.y) * 100, 1), "%") })
  output$pico_camara <- renderText({ req(dados_processados()); df <- dados_processados(); df_x <- df %>% filter(!str_detect(fonte, "Google")); if(nrow(df_x) == 0) return("---"); format(df_x %>% slice_max(valor, n=1, with_ties=F) %>% pull(data), "%b/%Y") })
  output$pico_google <- renderText({ req(dados_processados()); df <- dados_processados(); df_x <- df %>% filter(str_detect(fonte, "Google")); if(nrow(df_x) == 0) return("---"); format(df_x %>% slice_max(valor, n=1, with_ties=F) %>% pull(data), "%b/%Y") })
  output$status_msg <- renderText({ paste("Intervalo:", format(input$periodo[1], "%m/%Y"), "-", format(input$periodo[2], "%m/%Y")) })
}

shinyApp(ui, server)