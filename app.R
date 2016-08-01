library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(ggplot2movies)
library(RColorBrewer)
df <- read.csv("BSelecao.csv", head = T, stringsAsFactors = FALSE)
ui <- dashboardPage(
  dashboardHeader(
    title = img(
      src = "logo_ifsc_horizontal.png",
      height = 74,
      align = "left"
    )
  ),
  dashboardSidebar(
    selectInput("Ingresso", "Ingresso:", choices = "2016-1"),
    selectInput("ensino", "Ensino:", choices = "Superior"),
    selectInput("campus", "Campus:", choices = NULL),
    selectInput("cursos", "Cursos:", choices = NULL)
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 11,
        valueBoxOutput("rate"),
        valueBoxOutput("count"),
        valueBoxOutput("users")
      ),
      
      column(
        width = 12,
        tabBox(side = "right",
          title = "Faixa de Renda",
          id = "Faixa de Renda",
          width = 12,
          tabPanel(title = "Percentual por Faixa de Renda", plotlyOutput("plot9", height = 250)),
          tabPanel(title = "Notas por Faixa de Renda", plotlyOutput("plot10", height = 250))
        )
      ),
      
      column(
        width = 12,
        tabBox(
          title = "Sexo",
          id = "Sexo",
          tabPanel(title = "Percentual por Sexo", plotlyOutput("plot1", height = 250)),
          
          tabPanel(title = "Notas por Sexo", plotlyOutput("plot2", height = 250))
        ),
        tabBox(
          title = "Raça",
          id = "Raça",
          tabPanel(title = "Percentual por Raça", plotlyOutput("plot3", height = 250)),
          tabPanel(title = "Notas por Raça", plotlyOutput("plot4", height = 250))
        )
      ),
      
      column(
        width = 12,
        tabBox(side = "right",
          title = "Estado Civil",
          id = "Civil",
          tabPanel(title = "Percentual por Estado Civil", plotlyOutput("plot5", height = 250)),
          tabPanel(title = "Notas por Estado Civil", plotlyOutput("plot6", height = 250))
        ),
        tabBox(side = "right",
          title = "Origem",
          id = "Origem",
          tabPanel(title = "Percentual por Origem", plotlyOutput("plot7", height = 250)),
          tabPanel(title = "Notas por Origem", plotlyOutput("plot8", height = 250))
        )
      ),
      column(
        width = 12,
        tabBox(
          title = "Escolaridade Pai/Mae",
          id = "Escolaridade Pai/Mae",
          width = 12,
          tabPanel(title = "Percentual Pai", plotlyOutput("plot11", height = 250)),
          tabPanel(title = "Notas por escolaridade Pai", plotlyOutput("plot12", height = 250)),
          tabPanel(title = "Percentual Mae", plotlyOutput("plot13", height = 250)),
          tabPanel(title = "Notas por escolaridade Mae", plotlyOutput("plot14", height = 250))
        )
      ),
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    df_campus <- unique(df$Campus)
    updateSelectInput(session,
                      'campus',
                      choices = df_campus,
                      selected = NULL)
  })
  observe({
    req(input$campus)
    df_cursos <- df[df$Campus == input$campus, ]
    df_cursos <- unique(df_cursos$Cursos)
    updateSelectInput(session,
                      'cursos',
                      choices = df_cursos,
                      selected = "Todos")
  })
  dados <- read.csv("dados.csv", head = T, dec = ",")
  observe({
    if ("Todos" == input$campus) {
      dados1 <- dados
    } else{
      dados1 <- subset(dados, Campus == input$campus)
    }
    
    if ("Todos" == input$cursos) {
      dados2 <- dados1
    } else{
      dados2 <- subset(dados1, Curso == input$cursos)
    }
    
    output$rate <- renderValueBox({
      x = nrow(dados2)
      valueBox(
        value = (x),
        subtitle = "Inscritos",
        icon = icon("users")
      )
    })
    
    output$count <- renderValueBox({
      y = nrow(subset(dados2, Situa == "APV"))
      valueBox(
        value = (y),
        subtitle = "Aprovados",
        icon = icon("education", lib = "glyphicon")
      )
    })
    
    output$users <- renderValueBox({
      x = nrow(dados2)
      y = nrow(subset(dados2, Situa == "APV"))
      z = x / y
      valueBox(
        value = formatC(z, digits = 1, format = "f"),
        subtitle = "Candidatos por vaga",
        icon = icon("pencil", lib = "glyphicon")
      )
    })
    
    output$plot1 <- renderPlotly({
      tabela <- data.frame(table(dados2$Sexo))
      plot_ly(tabela,
              labels = Var1,
              values = Freq,
              type = "pie")
    })
    
    output$plot2 <- renderPlotly({
      p <- plot_ly(dados2,
                   x = PontFinal,
                   color = Sexo,
                   type = "box")
    })
    
    output$plot3 <- renderPlotly({
      tabela <- data.frame(table(dados2$Raca))
      plot_ly(tabela,
              labels = Var1,
              values = Freq,
              type = "pie")
    })
    
    output$plot4 <- renderPlotly({
      p <- plot_ly(dados2,
                   x = PontFinal,
                   color = Raca,
                   type = "box")
    })
    
    output$plot5 <- renderPlotly({
      tabela <- data.frame(table(dados2$EstadoCivil))
      plot_ly(tabela,
              labels = Var1,
              values = Freq,
              type = "pie")
    })
    
    output$plot6 <- renderPlotly({
      p <- plot_ly(dados2,
                   x = PontFinal,
                   color = EstadoCivil,
                   type = "box")
    })
    
    output$plot7 <- renderPlotly({
      tabela <- data.frame(table(dados2$localiza))
      plot_ly(tabela,
              labels = Var1,
              values = Freq,
              type = "pie")
    })
    
    output$plot8 <- renderPlotly({
      p <- plot_ly(dados2,
                   x = PontFinal,
                   color = localiza,
                   type = "box")
    })
    
    output$plot9 <- renderPlotly({
      tabela <- data.frame(table(dados2$FaixaRenda))
      plot_ly(
        tabela,
        labels = Var1,
        values = Freq,
        type = "pie",
        hole = 0.6
      )
    })
    
    output$plot10 <- renderPlotly({
      p <- plot_ly(dados2,
                   x = PontFinal,
                   color = FaixaRenda,
                   type = "box")
      
    })
    
    output$plot11 <- renderPlotly({
      tabela <- data.frame(table(dados2$EscolaridadePai))
      plot_ly(
        tabela,
        labels = Var1,
        values = Freq,
        type = "pie",
        hole = 0.6
      )
    })
    
    output$plot12 <- renderPlotly({
      p <-
        plot_ly(dados2,
                x = PontFinal,
                color = EscolaridadeMae,
                type = "box")
    })
    output$plot13 <- renderPlotly({
      tabela <- data.frame(table(dados2$EscolaridadeMae))
      plot_ly(
        tabela,
        labels = Var1,
        values = Freq,
        type = "pie",
        hole = 0.6
      )
    })
    
    output$plot14 <- renderPlotly({
      p <-
        plot_ly(dados2,
                x = PontFinal,
                color = EscolaridadeMae,
                type = "box")
    })
                        
  })
}

shinyApp(ui, server)