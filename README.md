# 📉 Sismógrafo Político (Political Seismograph)

![R](https://img.shields.io/badge/R-4.4%2B-blue)
![Shiny](https://img.shields.io/badge/Shiny-1.8-blue)
![ETL](https://img.shields.io/badge/ETL-Pipeline-orange)

<div align="center">
  <a href="#-english">English</a> |
  <a href="#-português">🇧🇷 Português</a>
</div>

---

<div id="-english"></div>

## English

### Project Overview
The **Political Seismograph** is a data analysis tool that measures the correlation between **Public Interest** (Google Trends searches) and **Parliamentary Speech** (Official Chamber of Deputies speeches).

It answers the question: *"When people search for a crisis or reform, does Congress react immediately, or is there a lag?"*

### ⚙️ Engineering & Architecture
This project goes beyond a simple dashboard. It includes a full data engineering pipeline:

1.  **Extraction (Google Trends):** A robust script (`scripts_etl/01_extracao_google.R`) designed with **anti-blocking strategies** (random delays, cache saving) to bypass API rate limits (Error 429).
2.  **Processing (NLP):** A regex-based engine (`scripts_etl/02_processamento_nlp.R`) that processes over **20 years of unstructured text** from parliamentary speeches to count mentions of specific keywords (e.g., "Lava Jato", "Pension Reform").
3.  **Visualization:** A Shiny dashboard comparing the two time series with normalized indexes.

### 🛠️ Tech Stack
* **ETL:** `gtrendsR`, `stringi` (Regex), `lubridate`.
* **Storage:** `Apache Parquet` (via `arrow`) for high compression and speed.
* **Frontend:** `Shiny`, `bslib`, `Highcharter`.

### How to Run
1.  Clone the repo.
2.  Run `source("setup.R")`.
3.  Run `app.R`.
    * *Note: The raw speech data is not included due to size constraints, but the processed cubes are available for the app to run.*

---

<div id="-português"></div>

## 🇧🇷 Português

### Visão Geral
O **Sismógrafo Político** é uma ferramenta que mede a correlação entre o **Interesse Público** (buscas no Google) e o **Discurso Parlamentar** (discursos oficiais na Câmara).

O objetivo é responder: *"Quando o povo busca sobre uma crise, o Congresso reage na hora ou existe um atraso (delay)?"*

### ⚙️ Engenharia e Arquitetura
Este projeto inclui um pipeline completo de dados:

1.  **Extração (Google Trends):** Script robusto (`scripts_etl/01_extracao_google.R`) com estratégias de **backoff e cache** para evitar bloqueios de API (Erro 429).
2.  **Processamento (NLP):** Motor de processamento (`scripts_etl/02_processamento_nlp.R`) que varre mais de **20 anos de textos não estruturados** para contabilizar menções a termos chave via Regex.
3.  **Visualização:** Dashboard comparando as duas séries temporais normalizadas.

### 🛠️ Tecnologias
* **ETL:** `gtrendsR`, `stringi` (Regex), `lubridate`.
* **Armazenamento:** `Apache Parquet` para alta performance.
* **Frontend:** `Shiny`, `bslib`, `Highcharter`.

---
*Developed by Andéliton Soares*
