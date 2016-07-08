library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(ggplot2movies)
library(RColorBrewer)
ui <- dashboardPage(
  dashboardHeader(title=img(src="logo_ifsc_horizontal.png",height=74,align = "left")),
  #title = "Dados Ingresso",titleWidth = 200),
  dashboardSidebar(width = 250,
                   selectInput("campus","Campus:", 
                               choices=c("Total",
                                         "Araranguá",
                                         "Caçador",
                                         "Canoinhas",
                                         "Chapecó",
                                         "Continente",
                                         "Criciúma",
                                         "Florianópolis",
                                         "Gaspar",
                                         "Itajaí",
                                         "Jaraguá do Sul",
                                         "Jaraguá do Sul – Rau",
                                         "Joinville",
                                         "Lages",
                                         "São José",
                                         "São Miguel do Oeste",
                                         "Urupema",
                                         "Xanxerê"
                               )),
                   selectInput("cursos", "Cursos:", 
                               choices=c("Total",
                                         "Agronomia",
                                         "Ciência da Computação",
                                         "Engenharia Civil",
                                         "Engenharia de Controle e Automação",
                                         "Engenharia de Mecatrônica",
                                         "Engenharia de Produção",
                                         "Engenharia de Telecomunicações",
                                         "Engenharia Elétrica",
                                         "Engenharia Eletrônica",
                                         "Engenharia Mecânica",
                                         "Engenharia Mecatrônica",
                                         "Gastronomia",
                                         "Hotelaria",
                                         "Licenciatura em Física",
                                         "Licenciatura em Química",
                                         "Química - Licenciatura",
                                         "Tecnologia em Alimentos",
                                         "Tecnologia em Análise e Desenvolvimento de Sistemas",
                                         "Tecnologia em Design de Moda",
                                         "Tecnologia em Design de Produto",
                                         "Tecnologia em Fabricação Mecânica",
                                         "Tecnologia em Gestão de Tecnologia da Informação",
                                         "Tecnologia em Gestão Hospitalar",
                                         "Tecnologia em Processos Gerenciais",
                                         "Tecnologia em Radiologia",
                                         "Tecnologia em Sistemas de Energia",
                                         "Tecnologia em Sistemas Eletrônicos",
                                         "Tecnologia em Viticultura e Enologia")
                   )
                   
                   
                   
  ),
  dashboardBody(
    fluidRow(
      column(width = 12,
             valueBoxOutput("rate"),
             valueBoxOutput("count"),
             valueBoxOutput("users")),
      column(width = 12,
             box(
               title = "Percentual por Sexo",status = "primary",solidHeader = TRUE, plotlyOutput("plot1", height = 250)),
             
             box(
               title = "Notas por Sexo", status = "primary", solidHeader = TRUE, plotlyOutput("plot2", height = 250))),
      column(width = 12,
             box(
               title = "Percentual por Raça",status ="success", plotlyOutput("plot3", height = "250px")),
             
             box(
               title = "Notas por Raça",solidHeader=TRUE,status = "primary",plotlyOutput("plot4", height = 250)
             )),
      column(width = 12,
             box(height = 250),
             
             box(height = 250)),
      tags$head(tags$style(HTML('
                                /*barra fixada*/
                              .navbar {
                              position: fixed;
                              min-height: 50px;
                              margin-bottom: 20px;
                              border: 1px solid transparent;
                              }
                              
                              .col-sm-12 {
                              width: 100%;
                              margin-top:75px;
                              }


                              /*cor fundo*/
                              .content {
                              min-height: 250px;
                              padding: 15px;
                              margin-right: auto;
                              margin-left: auto;
                              background-color:#FFFFFF
                              }
                              .skin-blue .wrapper {
                              background-color: #FFF;
                              }
  
                              /*informativos*/
                              .alert-info, .bg-aqua, .callout.callout-info, .label-info, .modal-info .modal-body {
                              background-color: #94C122 !important;
                              background-image: linear-gradient(45deg, #A2CD5A, #BCEE68);
                              margin-top:30px;
                              }

                              /* box-status */
                              .box.box-solid.box-primary>.box-header {
                              color:#fff;
                              background:#94C122
                              }
                              .box.box-solid.box-primary{
                              border-bottom-color:#666666;
                              border-left-color:#666666;
                              border-right-color:#666666;
                              border-top-color:#666666;
                              }
                                
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #BCEE68;
                              background-image: linear-gradient(to bottom, #A2CD5A, #FFFFFF);
                              min-height: 75px;
                              border: 1px solid white;
                              width:1400px;
                              position:fixed;
                              }
                               
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #A2CD5A;
                              position:fixed;
                              background-image: linear-gradient(to bottom, #A2CD5A, #FFFFFF)
                              }
                                
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #A2CD5A;
                              background-image: linear-gradient(to bottom, #A2CD5A, #FFFFFF);
                              position:fixed;
                              margin-left: 250px;
                              border: none;
                              min-height: 75px;
                              border-bottom: 1px solid white;
                              border-top: 1px solid white;
                              }        
                                
                               /* main sidebar */
                              .skin-blue .main-sidebar {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              background-color: #ABB876;
                              margin-top: 30px;
                              border: 1px solid white;
                              position:fixed;
                              background-image: linear-gradient(to bottom, #A2CD5A, #BCEE68)
                              }
                              .left-side, .main-sidebar {
                              position: absolute;
                              top: 0;
                              left: 0;
                              padding-top: 50px;
                              min-height: 0;
                              z-index: 810;
                              }
                          
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #BCEE68;
                              background-image: linear-gradient(to bottom, #A2CD5A, #BCEE68);
                              border: 1px solid white                                
                              }
                                
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #BCEE68;
                              background-image: linear-gradient(to bottom, #A2CD5A, #BCEE68);
                              color: #BCEE68;
                              border: 1px solid white                                
                              } 
                                
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #BCEE68;
                              background-image: linear-gradient(to bottom, #A2CD5A, #BCEE68);
                              border: 1px solid white
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #BCEE68;
                              background-image: linear-gradient(to bottom, #A2CD5A, #FFFFFF);
                              }
                                ')))
    )))



server <- function(input, output) {
  output$rate <- renderValueBox({
    valueBox(
      value = formatC(8, digits = 1, format = "f"),
      subtitle = "Dados 1",
      icon = icon("pencil", lib = "glyphicon")
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      value = (25),
      subtitle = "Dados 2",
      icon = icon("education", lib = "glyphicon")
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      value = (50),
      "Dados 3",
      icon = icon("users")
    )
  })
  
  output$plot1 <- renderPlotly({
    dados <- read.csv("dados.csv", head=T, dec=",")
    if("Total"==input$cursos){
      dados2 <- dados
    }else{
      dados2 <- subset(dados, Curso==input$cursos)
    }
    tabela<-data.frame(table(dados2$Sexo))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie")
  })
  
  output$plot2 <- renderPlotly({
    dados <- read.csv("dados.csv", head=T, dec=",")
    #if("Total"==input&){
    #dados3<- dados
    #}else{
    #dados3<- subset(dados, Campus==input$a)
    #}
    if("Total"==input$cursos){
      dados2 <- dados
    }else{
      dados2 <- subset(dados, Curso==input$cursos)
    }
    p <- plot_ly(dados2, x=PontFinal, color = Sexo, type = "box")
    p
  })
  
  
  output$plot3<- renderPlotly({
    dados <- read.csv("dados.csv", head=T, dec=",")
    if("Total"==input$cursos){
      dados2 <- dados
    }else{
      dados2 <- subset(dados, Curso==input$cursos)
    }
    tabela<-data.frame(table(dados2$Raca))
    plot_ly(tabela, labels = Var1, values = Freq, type = "pie")
  })
  
  output$plot4 <- renderPlotly({
    dados <- read.csv("dados.csv"
                      , head=T, dec=",")
    if("Total"==input$cursos){
      dados2 <- dados
    }else{
      dados2 <- subset(dados, Curso==input$cursos)
    }
    p <- plot_ly(dados2, x=PontFinal, color = Raca, type = "box")
    p
  })  
  
}

shinyApp(ui, server)
