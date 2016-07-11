library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(ggplot2movies)
library(RColorBrewer)
df <- read.csv("/home/dgc/shinyApp/BSelecao.csv", head=T,stringsAsFactors = FALSE)

ui <- dashboardPage(
  dashboardHeader(title=img(src="logo_ifsc_horizontal.png",height=74,align = "left")),
  #title = "Dados Ingresso",titleWidth = 200),
  dashboardSidebar(width = 250,
                   selectInput("sel1", "Campus:",choices=NULL, 
                               selected = NULL, 
                               selectize = FALSE),
                   
                   selectInput("sel2", "Cursos:",choices=NULL, 
                               selected = NULL, 
                               selectize = FALSE)),
  dashboardBody(
    fluidRow(
      column(width = 12,
             valueBoxOutput("rate"),
             valueBoxOutput("count"),
             valueBoxOutput("users")),
             tabBox(title= "Sexo", id= "Sexo", height ="250px",
               tabPanel("Percentual por Sexo",title = "Percentual por Sexo",status = "primary",solidHeader = TRUE, plotlyOutput("plot1", height = 250)),
             
             tabPanel(
               title = "Notas por Sexo", status = "primary", solidHeader = TRUE, plotlyOutput("plot2", height = 250))),
             tabBox(
               tabPanel(title = "Percentual por Raça",status ="success", plotlyOutput("plot3", height = "250px")),
             
             tabPanel(
               title = "Notas por Raça",solidHeader=TRUE,status = "primary",plotlyOutput("plot4", height = 250))
             ),
      column(width = 12,
             box(height = 250),
             
             box(height = 250)),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      )
    )
  )
)



server <- function(input, output,session) {
  observe({
    df_a <- unique(df$Campus)
    updateSelectInput(session,'sel1', choices = df_a, selected = NULL)
  })
  observe({
    req(input$sel1)
    df_b <- df[df$Campus == input$sel1,]
    df_b <- unique(sort(df_b$Cursos))
    updateSelectInput(session,'sel2', choices = df_b, selected = NULL)
  })
  
  output$rate <- renderValueBox({
    valueBox(
      value = ("x"),
      subtitle = "Inscritos",
      icon = icon("users")
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      value = ("y"),
      subtitle = "Aprovados",
      icon = icon("education", lib = "glyphicon")
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      value = formatC("z", digits = 1, format = "f"),
      subtitle="Nota media",
      icon = icon("pencil", lib = "glyphicon")
    )
  })
  
  dados <- read.csv("dados.csv", head=T, dec=",")
  output$plot1 <- renderPlotly({
    if("Todos"==input$sel1){
      dados1 <- dados
    }else{
        dados1 <- subset(dados, Campus==input$sel1)
    }
    if("Todos"==input$sel2){
      dados2 <- dados
    }else{
      dados2 <- subset(dados, Curso==input$sel2)
    }
    
    tabela<-data.frame(table(dados2$Sexo))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie")
  })
  
  output$plot2 <- renderPlotly({
    if("Todos"==input$sel1){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$sel1)
    }
    if("Todos"==input$sel2){
      dados2 <- dados
    }else{
      dados2 <- subset(dados, Curso==input$sel2)
    }
    p <- plot_ly(dados2, x=PontFinal, color = Sexo, type = "box")
  })
  
  
  output$plot3<- renderPlotly({
   if("Todos"==input$sel1){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$sel1)
    }
    if("Todos"==input$sel2){
      dados2 <- dados
    }else{
      dados2 <- subset(dados, Curso==input$sel2)
    }
   tabela<-data.frame(table(dados2$Raca))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie")
  })
  
  output$plot4 <- renderPlotly({
    if("Todos"==input$sel1){
      dados1 <- dados
    }else{
      dados1 <- subset(dados, Campus==input$sel1)
    }
    if("Todos"==input$sel2){
      dados2 <- dados
    }else{
      dados2 <- subset(dados, Curso==input$sel2)
    }
    p <- plot_ly(dados2, x=PontFinal, color = Raca, type = "box")
  })  
}

shinyApp(ui, server)
