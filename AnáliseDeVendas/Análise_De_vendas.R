# Carregar bibliotecas
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

# Ler dados do Excel
base <- read_excel("D:/downloads/09-12 - Bases de Dados.xlsx", sheet = "Planilha1")

# Converte a coluna "Data da Venda" para formato de data
base$`Data da Venda` <- as.Date(base$`Data da Venda`)

# Calcular o lucro
base$lucro <- base$`Quantidade Vendida` * base$`Preco Unitario`

# Criar vetor com todos os meses
todos_meses <- unique(format(base$`Data da Venda`, "%Y-%m"))

# UI
ui <- fluidPage(
  titlePanel("Análise de Vendas"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("produto", "Selecione o Produto", choices = unique(base$Produto)),
      selectInput("grafico", "Selecione o Gráfico", choices = c("Tendências Mensais de Vendas", "Variação de Desempenho de Produtos ao Longo do Tempo")),
      textOutput("total_vendas_text"),
      textOutput("total_lucro_text"),
      br(),
      br(),
      actionButton("atualizar", "Atualizar Gráfico"),
      selectInput("mes_analise", "Selecione o Mês para Análise", choices = c("Todos", todos_meses), selected = "Todos")
    ),
    mainPanel(
      plotOutput("output_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  dados_filtrados <- reactive({
    if (input$mes_analise == "Todos") {
      base %>%
        filter(Produto == input$produto)
    } else {
      base %>%
        filter(Produto == input$produto, format(`Data da Venda`, "%Y-%m") == input$mes_analise)
    }
  })
  
  output$output_plot <- renderPlot({
    if (input$grafico == "Tendências Mensais de Vendas") {
      ggplot(dados_filtrados(), aes(x = format(`Data da Venda`, "%Y-%m"), y = `Quantidade Vendida`, fill = `Produto`)) +
        geom_bar(stat = "identity") +
        labs(title = "Tendências Mensais de Vendas", x = "Data da Venda (Ano-Mês)", y = "Quantidade Vendida", fill = "Produto") +
        theme_minimal() +
        theme(legend.position="top") +
        scale_fill_manual(values = c("#FF9999", "#66B2FF", "#99FF99", "#FFCC99"))  # Especifique as cores desejadas
    } else if (input$grafico == "Variação de Desempenho de Produtos ao Longo do Tempo") {
      ggplot(dados_filtrados(), aes(x = `Data da Venda`, y = `Quantidade Vendida`, color = `Produto`)) +
        geom_line() +
        labs(title = "Variação de Desempenho de Produtos ao Longo do Tempo", x = "Data da Venda", y = "Quantidade Vendida", color = "Produto") +
        theme_minimal() +
        theme(legend.position="top") +
        scale_color_manual(values = c("#FF9999", "#66B2FF", "#99FF99", "#FFCC99"))  # Especifique as cores desejadas
    }
  })
  
  output$total_vendas_text <- renderText({
    total_vendas <- sum(dados_filtrados()$`Quantidade Vendida`)
    paste("Total de Vendas: ", total_vendas)
  })
  
  output$total_lucro_text <- renderText({
    total_lucro <- sum(dados_filtrados()$lucro)
    paste("Total de Lucro: R$", format(total_lucro, big.mark = ".", decimal.mark = ","))
  })
  
  observeEvent(input$atualizar, {
    updateSelectInput(session, "produto", choices = unique(base$Produto))
  })
}

# Executar o aplicativo Shiny
shinyApp(ui, server)
