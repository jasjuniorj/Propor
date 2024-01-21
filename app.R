#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(jsonlite)
library(fresh)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyjs)
library(readxl)
#library(rlang)
library(shinybusy)
library(MASS)
library(shinyWidgets)
#library(shinyBS)
#update.packages("rlang")
library(rmarkdown)
library(forcats)
library(Cairo)
library(utf8)



options(encoding = "UTF-8")

mytheme <- create_theme(
  
  adminlte_color(
    light_blue = "#3F5B72",
    
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#3F5B72", #lateral
    dark_hover_bg = "#CE0707", # detaque da lateral
    dark_color = "#FFFFFF" # fontes da lateral
  ),
  
  adminlte_global(
    content_bg = "#FFFFFF", #fundo do MainPainel
    box_bg = "#FCFCFC", # Detalhes da box
    info_box_bg = "#FFFFFF" # Detalhes das box
    
  )
)


style_tabs <- function(tab_numbers, css_properties) {
  css <- sapply(tab_numbers, function(tab_number) {
    paste0(".nav-tabs > li:nth-child(", tab_number, ") > a { ", css_properties, " }")
  })
  tags$head(
    tags$style(
      HTML(paste(css, collapse = "\n"))
    )
  )}


jsCode <- "
      var notification = $('.shiny-notification');
      var scrollTop = $(document).scrollTop();
      var windowHeight = $(window).height();
      var notificationHeight = notification.outerHeight();
      var topPosition = (windowHeight - notificationHeight) / 8 + scrollTop;
      notification.addClass('scrolling-notification');
      notification.css('top', topPosition);
      
      $(window).on('scroll', function() {
        scrollTop = $(document).scrollTop();
        topPosition = (windowHeight - notificationHeight) / 8 + scrollTop;
        notification.css('top', topPosition);
      });
      "

css_code <- tags$style(
  HTML("
    .transparent-button {
      background-color: transparent;
      border: none;
      color: inherit;
      padding: 0;
      text-decoration: underline;
      cursor: pointer;
    }
  ")
)

progressBarConfig <- function(id, value, color, text) {
  tags$div(
    class = "progress",
    tags$div(
      class = paste0("progress-bar ", color, " text-black"),
      role = "progressbar",
      "aria-valuenow" = value,
      "aria-valuemin" = "0",
      "aria-valuemax" = "100",
      style = paste0("width: ", value, "%;"),
      paste0(text, " (", value, "%)")
    ),
    class = "row justify-content-right"  # Classes para centralizar o progressBar
  )
}



confBox <- function(color) {
  tags$style(HTML(sprintf('
    .box.box-solid.box-success>.box-header {
      color: #FCFCFC;
      background: %s;
    }

    .box.box-solid.box-primary {
      border-bottom-color: %s;
      border-left-color: %s;
      border-right-color: %s;
      border-top-color: %s;
    }
  ', color, color, color, color, color)))
}

### Novas Funções



confbutton <- list(
  type = 'default',
  class = 'btn-group',
  style = "color:#F39C12; 40 width: 60px; height: 20px; border-radius: 1px; 
  background-color: #FFFFFF; font-size: 16px; 
  display: flex; align-items: center; justify-content: center;
  position: absolute; bottom: 10px; left: 10px;"
)

fixed_row <- tags$div(
  h3("| Definindo..."),
  style = "height: 120px; background-color: white; position: fixed; top: 0; width: 90%;
  z-index: 1000; border-bottom: 1px solid #ccc;",
  class = "fixed-row",
  progress_line(value = 0, shiny_id = "barfix", color = "#F39C12", trail_color = "#F39C12"),
  h5(HTML("| Gere um doc com as suas respostas ...")),
  downloadButton("gerar", "Gerar", type = confbutton$type,
                 #class = confbutton$class,
                 style = "color:#F39C12; 40 width: 60px; height: 20px; border-radius: 1px; 
  background-color: #FFFFFF; font-size: 16px; 
  display: flex; align-items: center; justify-content: center;
  position: absolute; bottom: 10px; right: 10px;")) 
  
#library(shiny)
#library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(disable = TRUE,
                  title = " ",
                  titleWidth = 500,
                  tags$li(
                    class = "dropdown",
                    tags$a(
                      href = "https://www.emorio.org/",
                      style = "color: white;",  
                      "Nosso Site"
                    )
                  )
  ),
  dashboardSidebar(
    tags$style(".sidebar-toggle { display: none; }"),
    collapsed = TRUE
  ),
  dashboardBody(
    use_theme(mytheme),
    useShinyjs(),
    h2(HTML("# <strong> PROPOR | </strong>")),
    h3(HTML("Ambiente para Construção de Políticas ...")),
    HTML(paste("Siga o fluxo de criação e veja exemplos e conceitos que auxliam no processo.",
               "O ambiente disponiliza orietanções para as etapas de " ,
               "formulação, implementação e avaliação de políticas.",
               "<br>",
               "<br>",
               "<div style='text-align: left;color:#F39C12'>",
               actionButton('saiba', 'Saiba Mais', class = 'transparent-button'),
               "</div>")),
    css_code,
    br(),
    h3(HTML("# <strong> Crie |</strong> sua política")),   
    br(),
    h4(HTML("Vamos começar ...")),
    br(),
    HTML(paste(
      "<div style='text-align: left;'>",
      actionButton(
        'inic', 
        'Iniciar',  
        type = "default", 
        class = 'btn-lg',
        style = "color:#F39C12; width: 100px; height: 30px; border-radius: 1px; background-color: #FFFFFF; font-size: 16px; display: flex; align-items: center; justify-content: center;"
      ),
      
      "</div>"
    )),
    css_code,
    
    br(),
    style_tabs(c(2, 3), "color: gainsboro;"),
    conditionalPanel(
      condition = "input.inic > 0",
      tabsetPanel(
        id = "tabs",
        tabPanel("Formulação",
                 
                 fluidPage(
                   br(),
                   br(),
                   fluidRow(
                     
                     column(
                       width = 4.0,
                       selectInput(
                         'area', 'Área/Setor', choices = c('(selecione)','Assistência Social','Cultura/Artes' ,'Direitos Humanos','Desenvolvimento Econômico',
                                                           'Educação', 'Emprego/Renda', 'Energia', 'Esporte/Lazer',
                                                           'Habitação', 'Igualdade Racial', 'Infraestrutura','Juventude','Mobilidade Urbana', 'Mulher' ,'Saneamento',
                                                           'Saúde', 'Segurança Pública', 'Transporte'), selected ='(selecione)'
                       )
                     ),
                     column(
                       width = 4.0,
                       selectInput(
                         'escopo', 'Alcance', choices = c('(selecione)','Local', 'Municipal', 'Estadual', 'Nacional'),
                         selected = '(selecione)'
                       )
                     ),
                     
                     column(
                       width = 4.0,
                       selectInput(
                         'inicia', 'Iniciativa', choices = c('(selecione)','Pública', 'Privada', 'Público-Privada'),
                         selected = '(selecione)'
                       )
                     )
                   ),
                   
                   br(),
                   br(),
                   
                   conditionalPanel(
                     condition = "input.inicia != '(selecione)'",
                     
                     ## Primeira Pergunta 
                     fluidRow(
                       box(
                         width = 12,
                         title = "Formulação",
                         status = "warning",
                         collapsible = TRUE,
                         solidHeader = TRUE,
                         HTML(paste("<strong>|</strong> Processo que compreende as etapas de definição do problemas, soluções", 
                                    "e agenda pública. Para formular é preciso identificar um plano de ações e a atenção dispensada",
                                    "ao problema alvo da política.<br>"))
                         
                       )),
                     
                     fluidRow(
                       
                       h3(HTML("# <strong> 10 Passos |  </strong> Para Sua Política"),
                          style = "text-align: left;",),
                       br(),
                       br(),
                       h5(HTML("1 | Qual problema/necessidade pública quer solucionar?"),
                          style = "text-align: left;",),
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgprob",
                           placeholder = "Resposta ...",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudaprob', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('concprob', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defprob', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxprob")
                     )),
                   # Segunda Pergunta
                   
                   conditionalPanel(
                     condition = "input.defprob > 0",
                     
                     tags$head(
                       tags$style(HTML("
                   .fixed-row {
                   /* Definido seu estilo aqui */
                        }
                          "))
                     ),
                     
                     fixed_row, 
                     fluidRow(
                       h5(HTML("2 | Quais as principais causas do problema?"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgcaus",
                           placeholder = "Resposta ...",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudacaus', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('conccaus', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defcaus', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxcaus")
                     ),
                     
                     
                   ),
                   conditionalPanel(
                     condition = "input.defcaus > 0",
                     fluidRow(
                       h5(HTML("3 | Quais os principais indicativos do problema? ( 
                              amplitude, gravidade, tendência e consequências)"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgind",
                           placeholder = "Resposta ...",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudaind', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('concind', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defind', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxind")
                     ),
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.defind > 0",
                     fluidRow(
                       h5(HTML("4 | Qual o principal objetivo da proposta?"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgobj",
                           placeholder = "Resposta ...",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudaobj', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('concobj', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defobj', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxobj")
                     ),
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.defobj > 0",
                     fluidRow(
                       h5(HTML("5 | Quais os principais resultados e impactos esperados?"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgres",
                           placeholder = "Resposta ...",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudares', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('concres', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defres', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxres")
                     ),
                     
                     
                   ),
                   conditionalPanel(
                     condition = "input.defres > 0",
                     fluidRow(
                       h5(HTML("6 | Qual o conjunto de ações necessárias para solução?"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgsol",
                           placeholder = "Resposta ...",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudasol', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('concsol', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defsol', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxsol")
                     ),
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.defsol > 0",
                     fluidRow(
                       h4(HTML("2 <strong> ATORES | AGENDA</strong>"),
                          style = "text-align: left;",),
                       br(),
                       h5(HTML("7 | Qual o público-alvo, os apoiadores e os opositiores da proposta?"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgpub",
                           placeholder = "Público-Alvo .. \n Apoiadores .. \n Opositores ..",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudapub', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('concpub', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defpub', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxpub")
                     ),
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.defpub > 0",
                     fluidRow(
                       
                       h5(HTML("8 | Qual a agenda governamental para proposta?"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgage",
                           placeholder = "Poder.. \n Percepção.. \n Potência.. \n Proximidade..",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudaage', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('concage', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defage', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxage")
                     ),
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.defage > 0",
                     fluidRow(
                       
                       h5(HTML("9 | Quais os fatores necessários para viabilizar a proposta?"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgfat",
                           placeholder = "Viabilidade.. \n Aceitação.. \n Custos Toleráveis..",
                           rows = 4,
                           style = "width: 100%; resize: vertical;"
                         ),
                         
                         div( 
                           style = "text-align: right;",
                           actionButton('ajudafat', '# Exemplo', class = "transparent-button",
                                        style ="width: 80px")),
                         div( 
                           style = "text-align: right;",
                           actionButton('concfat', '# Conceito', class = "transparent-button",
                                        style ="width: 80px;"))
                         
                         
                       ),
                       
                       
                     ),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'deffat', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     fluidRow(
                       infoBoxOutput(width = 12, "info_boxfat")
                     ),
                     
                     
                   ),
                   
                   conditionalPanel(
                     condition = "input.deffat > 0",
                     fluidRow(
                       
                       h5(HTML("10 | Qual o nome da sua política?"),
                          style = "text-align: left;",),
                       
                       tags$div(
                         style = "position: relative;",
                         tags$textarea(
                           id = "textMsgnome",
                           placeholder = "Nome..",
                           rows = 2,
                           style = "width: 100%; resize: vertical;"
                         )
                         
                       ),
                       
                       
                     ),
                     br(),
                     br(),
                     fluidRow(
                       column(
                         width = 4.0,
                         actionButton(
                           'defnome', 
                           'Definido', 
                           type = confbutton$type,
                           class = confbutton$class,
                           style = confbutton$style
                           
                         )
                         
                       )
                       
                     ),
                     
                     br(),
                     
                   ),
                   conditionalPanel(
                     condition = "input.defnome > 0",
                     fluidRow(
                       h3(HTML("Propostas Formulada <strong> | </strong> Clique em <strong> Gerar</strong> "),
                          style = "text-align: left;"),
                       h3(HTML(". . .",),
                          style = "text-align: left;",),
                       h3(HTML("<strong>Pro | Ponha</strong> Outra Política !",),
                          style = "text-align: left;",),
                       br(),
                       actionButton("reiniciar", "Reinicie",
                                    type = 'default',
                                    class = 'btn-group',
                                    style = "color:#F39C12; 40 width: 60px; height: 20px; border-radius: 1px; 
                                    background-color: #FFFFFF; font-size: 16px; 
                                    display: flex; align-items: center; justify-content: center;
                                    position: absolute; bottom: 10px; left: 10px;"
                       ),
                       tags$script('
          $(document).on("shiny:connected", function(event) {
            // Ouça o sinal enviado do lado do servidor
         Shiny.addCustomMessageHandler("reloadPage", function(message) {
           // Recarregue a página quando o sinal for recebido
              location.reload();
                    });
            });
             ')
                       
                       
                     )
                     
                   )
                   
                   
                 ),
                 
        ),
        
        tabPanel("Implementação", 
                 h2("Em construção."),
                 p("Em construção.")
        ),
        tabPanel("Avaliação", 
                 h2("Em construção."),
                 p("Em construção.")
        )
      ),
      
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## Etapas
  
  observeEvent(input$defprob,{
    update_progress("barfix", 10/100)
  })
  observeEvent(input$defcaus,{
    update_progress("barfix", 20/100)
  })
  observeEvent(input$defind,{
    update_progress("barfix", 30/100)
  })
  observeEvent(input$defobj,{
    update_progress("barfix", 40/100)
  })
  observeEvent(input$defres,{
    update_progress("barfix", 50/100)
  })
  observeEvent(input$defsol,{
    update_progress("barfix", 60/100)
  })
  observeEvent(input$defpub,{
    update_progress("barfix", 70/100)
  })
  observeEvent(input$defage,{
    update_progress("barfix", 80/100)
  })
  observeEvent(input$deffat,{
    update_progress("barfix", 90/100)
  })
  observeEvent(input$defnome,{
    update_progress("barfix", 100/100)
  })
  
  
  ##Ajuda
    textprob <- reactiveVal(NULL)
    
    observeEvent(input$ajudaprob, {
      textprob(h5(HTML("Exemplo: Proteção de defensoes de direitos humanos que sofreram ameaças em virtude da sua militância.",
                             "<br>")))
    })
    
    output$info_boxprob <- renderInfoBox({
      req(textprob())
      infoBox(
        input$area,
        textprob(),
        icon = icon("book"),
        color = 'yellow' 
      )
    })
    
    textcaus <- reactiveVal(NULL)
    observeEvent(input$ajudacaus, {
      textcaus(h5(HTML("Ameaça em decorrência da atuação na defesa de direitos humanos e",
                             "liberdades universalmente reconhecidas.",
                             "<br>")))
    })
    
    output$info_boxcaus <- renderInfoBox({
      req(textcaus())
      infoBox(
        input$area,
        textcaus(),
        
        icon = icon("book"),
        color = 'yellow' 
      )
    })
    
    ## Função para gerar o PDF
    
    gerarPDF <- function(filename, input) {
      # Crie um conteúdo temporário para o R Markdown
      rmd_content <- paste0(
        "---\n",
        "title: '**PRO | POR** Ambiente para Construção de Políticas'\n",
        "author: '**Resumo da Proposta**'\n",
        "output: pdf_document\n",
        "---\n\n",
        h4(HTML("**Dados Gerais**")),
        paste0(
          "Política: ", input$textMsgnome, "\n\n",
          "Área/Setor: ", input$area, "\n\n",
          "Alcance: ", input$escopo, "\n\n",
          "Inicativa: ", input$inicia, "\n\n",
          "...\n\n",
          h4(HTML("**Principais Componentes**")),
          "\ \n\n",
          h5(HTML("1 | Problema/necessidade"),
             style = "text-align: left;"),
          # Adicione mais inputs aqui
          "\n\n", input$textMsgprob, "\n\n",
          "...\n\n",
          h5(HTML("2 | Principais causas"),
             style = "text-align: left;"),
          "\n\n", input$textMsgcaus, "\n\n",
          "...\n\n",
          h5(HTML("3 | Principais indicativos"),
             style = "text-align: left;"),
          "\n\n", input$textMsgind, "\n\n",
          "...\n\n",
          h5(HTML("4 | Principal objetivo"),
             style = "text-align: left;"),
          "\n\n", input$textMsgobj, "\n\n",
          "...\n\n",
          h5(HTML("5 | Resultados esperados"),
             style = "text-align: left;"),
          "\n\n", input$textMsgres, "\n\n",
          "...\n\n",
          h5(HTML("6 | Solução proposta"),
             style = "text-align: left;"),
          "\n\n", input$textMsgsol, "\n\n",
          "...\n\n",
          h5(HTML("7 | Público-alvo, apoiadores e opositiores"),
             style = "text-align: left;"),
          "\n\n", input$textMsgpub, "\n\n",
          "...\n\n",
          h5(HTML("8 | Posição na agenda"),
             style = "text-align: left;"),
          "\n\n", input$textMsgage, "\n\n",
          "...\n\n",
          h5(HTML("9 | Viabiliade da proposta"),
             style = "text-align: left;"),
          "\n\n", input$textMsgfat, "\n\n"
          # E assim por diante...
        )
      )
      
      # Crie um arquivo Rmd temporário com o conteúdo
      documento_rmd_temp <- tempfile(fileext = ".Rmd")
      writeLines(rmd_content, documento_rmd_temp)
      
      # Renderize o R Markdown temporário para PDF
      rmarkdown::render(
        input = documento_rmd_temp,
        output_format = pdf_document(),
        output_file = filename
      )
    }
    
    output$criar <- downloadHandler(
      filename = function() {
        "documento_personalizado.pdf"  # Nome do arquivo PDF
      },
      content = function(file) {
        gerarPDF(file, input)
      }
    )
    output$gerar <- downloadHandler(
      filename = function() {
        "documento_personalizado.pdf"  # Nome do arquivo PDF
      },
      content = function(file) {
        gerarPDF(file, input)
      }
    )
    
    observeEvent(input$reiniciar, {
      # Envie um sinal para o lado do servidor
      session$sendCustomMessage("reloadPage", TRUE)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


