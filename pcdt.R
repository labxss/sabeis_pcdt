library(shinydashboard)
library(shinydashboardPlus) # flipbox
library(rmarkdown)
library(pyramid)
library(ggplot2)
library(dplyr)
library("gdalUtils") # mapas
library("rgdal") # mapas
library("sp") # mapas
library(treemap)

library(forecast)
library(tseries)
# library(fpp) # evita predicao zerada

library(tidyverse)
library(viridis)
library(patchwork)
# library(hrbrthemes)
library(circlize)
# library(chorddiag)  #devtools::install_github("mattflor/chorddiag")
library(qcc)


# library(plotly)
# https://fontawesome.com/icons?d=gallery&m=free -- free icons


ui <- dashboardPage(
  dashboardHeader(
    title = span(tagList(icon("couch"), "sabeis - PCDT"))
  ), # dashboardHeader
  
  ## Sidebar ================================================
  dashboardSidebar(
    sidebarMenu(
      
      p("Sala Aberta de Inteligência em Saúde"),
      
      hr(),
      
      menuItem(
        'Escolha',
        tabName = 'escolha', 
        icon = icon('tasks')
      ), # menuItem escolha
      
      selectizeInput(
        inputId = 'inPCDT', 
        label = 'PCDT', 
        choices=c('Acne'=1, 'Acromegalia'=2, 'Anemia Aplástica'=3, 'Anemia Hemolítica Autoimune'=4, 'Anemia na Insuficiência Renal Crônica'=5, 'Anemia por Deficiência de Ferro'=80, 'Angioedema Hereditário'=6, 'Aplasia Pura Adquirida Crônica da Série Vermelha'=7, 'Artrite Psoríaca'=8, 'Artrite Reativa - Doença de Reiter'=9, 'Artrite Reumatoide'=10, 'Asma'=11, 'Comportamento Agressivo no Transtorno de Espectro do Autismo'=12, 'Deficiência de Hormônio do Crescimento'=13, 'Dermatomiosite e Polimiosite'=14, 'Diabetes Insípido'=15, 'Diabetes Mellitus Tipo 1'=81, 'Dislipidemia'=16, 'Distonias Focais e Espasmo Hemifacial'=17, 'Doença de Alzheimer'=18, 'Doença de Crohn'=19, 'Doença de Gaucher'=20, 'Doença de Paget - Osteíte Deformante'=21, 'Doença de Parkinson'=22, 'Doença de Wilson'=23, 'Doença Falciforme'=24, 'Doença pelo HIV Resultando em Outras Doenças'=25, 'Doença Pulmonar Obstrutiva Crônica'=26, 'Dor Crônica'=27, 'Endometriose'=28, 'Epilepsia'=29, 'Esclerose Lateral Amiotrófica'=30, 'Esclerose Múltipla'=31, 'Esclerose Sistêmica'=32, 'Espasticidade'=33, 'Espondilite Ancilosante'=34, 'Espondilopatia Inflamatória'=35, 'Esquizofrenia'=36, 'Fenilcetonúria'=37, 'Fibrose Cística'=38, 'Glaucoma'=39, 'Hemangioma'=40, 'Hepatite Autoimune'=41, 'Hepatite B e Coinfecções'=42, 'Hepatite C e Coinfecções'=43, 'Hiperfosfatemia na Insuficiência Renal Crônica'=44, 'Hiperplasia adrenal congênita'=45, 'Hiperprolactinemia'=46, 'Hipertensão Arterial Pulmonar'=47, 'Hipoparatireoidismo'=48, 'Ictioses Hereditárias'=49, 'Imunodeficiência Primária'=50, 'Insuficiência Adrenal Primária - Doença de Addison'=51, 'Insuficiência Pancreática Exócrina'=52, 'Leiomioma de Útero'=53, 'Lúpus Eritrematoso Sistêmico'=54, 'Miastenia Gravis'=55, 'Mucopolissacaridose do tipo I'=82, 'Mucopolissacaridose do tipo II'=83, 'Neutropenia'=56, 'Osteodistrofia Renal'=57, 'Osteoporose'=58, 'Profilaxia da Reinfecção pelo Vírus da Hepatite B Pós-Transplante Hepático'=59, 'Psoríase'=60, 'Puberdade Precoce Central'=61, 'Púrpura Trombocitopênica Idiopática'=62, 'Raquitismo e Osteomalácia'=63, 'Retocolite Ulcerativa'=64, 'Síndrome de Guillain-Barré'=65, 'Síndrome de Ovários Policísticos e Hirsutismo'=66, 'Síndrome de Turner'=67, 'Síndrome Nefrótica'=68, 'Síndromes Coronarianas Agudas'=69, 'Sobrecarga de Ferro'=70, 'Transplante Cardíaco'=71, 'Transplante de Coração e Pulmão'=72, 'Transplante de Medula ou Pâncreas'=73, 'Transplante de Pulmão'=74, 'Transplante Hepático'=75, 'Transplante Renal'=76, 'Transtorno Afetivo Bipolar do Tipo I'=77, 'Transtorno Esquizoafetivo'=78, 'Uveítes Posteriores Não Infecciosas'=79),
        selected = 10
      ),
      
      menuItem(
        'Explore',
        tabName = 'explore', 
        icon = icon('tachometer-alt')
      ), # menuItem escolha
      
      menuItem(
        'Extrapole',
        tabName = 'extrapole', 
        icon = icon('chart-line')
      ), # menuItem escolha
      
      hr(),
      
      menuItem(
        'Wiki',
        tabName = 'wiki', 
        icon = icon('book')
      ), # menuItem escolha
      
      menuItem(
        'Sobre',
        tabName = 'sobre', 
        icon = icon('info-circle')
      ) # menuItem escolha
      
    ) # sidebarMenu -----------------------
  ), # dashboardSidebar
  
  ## Body ================================================
  dashboardBody(
    tabItems(
      
      # Tab Escolha
      tabItem(
        tabName = "explore",
        fluidRow(    

          valueBox(
            "Explore",
            "",
            icon = icon("tachometer-alt"),
            width = 12,
            color = "red"
          ),          
          
          valueBox(
            textOutput("td_pcdt_cid__no_pcdt1"),
            "Sala do Protocolo Clínico e Diretrizes Terapêuticas - PCDT",
            icon = icon("map-signs"),
            width = 6,
            color = "purple"
          ),
          
          valueBox(
            textOutput("tf_pcdt__vl_aprovado_ipca"),
            HTML("<p title='Valor segundo o SIASUS sem correção'><strong>Valor</strong></p>"), 
            icon = icon("dollar-sign"),
            width = 3,
            color = "purple"
          ),
          

          valueBox(
            textOutput("tf_pcdt__nu_competencia"),
            HTML("<p title='Período conforme o mês de competência (aaaamm)'><strong>Período</strong></p>"),
            icon = icon("calendar"),
            width = 3,
            color = "purple"
          )

        ), # fluidRow
        
        box(
          title = 'Quem - usuários do PCDT', 
          status = 'primary', 
          width = 6,
          solidHeader = TRUE, 
          collapsible = TRUE, 
          footer =  'Quem - Projeção do número de usuários segundo Cartão Nacional de Saúde por trimestre.',
          
          infoBox(
            title = HTML("<p title='segundo o CNS'><strong>Usuários</strong></p>"), 
            textOutput("tf_pcdt__qt_cns_pacientes"), icon = icon("users"), color = "green",
            fill = TRUE, 
            width = NULL
            #            subtitle = HTML("<div align=\"left\"><a id=\"button\" href=\"#\" class=\"action-button\" style='color:#FFFF00;' title='teste teste teste'><i class=\"fa fa-info-circle\">info</i></a></div>")
            # subtitle = HTML("<p align='right'><i class=\"fa fa-info-circle\" style='color:#FFFF00;' title='segundo o CNS'></i></p>")
          ),
          
          plotOutput("tf_pcdt_paciente")
        ),
        
        box(
          title = 'O que - Diagnósticos dos itens dispensados', 
          status = 'primary', 
          width = 6,
          solidHeader = TRUE, 
          collapsible = TRUE,
          footer = 'Dignóstico principal registrado no Sistema de Informação Ambulatorial SIASUS.',
          
          infoBox(
            title = HTML("<p title='Dispensações segundo o SIASUS'><strong>Dispensações</strong></p>"), 
            textOutput("tf_pcdt__qt_registros"), icon = icon("pills"), color = "green",
            fill = TRUE, 
            width = NULL
          ),
            
          plotOutput("tf_pcdt_doenca_trimestre")

        ),  
          
        box(
          title = 'O que - Market Share do SUS', 
          status = 'primary', 
          width = 6,
          solidHeader = TRUE, 
          collapsible = TRUE,
          footer = 'Um medicamento pode conter mais de um procedimento SIGTAP.',
                          
          infoBox(
            title = HTML("<p title='Produtos segundo código SIGTAP'><strong>Produtos</strong></p>"),
            textOutput("tf_pcdt__qt_sigtap"), icon = icon("list"), color = "green",
            fill = TRUE, 
            width = NULL
          ),
          
          plotOutput("tf_pcdt_produto_ano")
        ),
        
        box(
          title = 'O que - Usuários-ano de medicamento', 
          status = 'primary', 
          width = 6,
          solidHeader = TRUE, 
          collapsible = TRUE,
          
          plotOutput(
            "tf_procedimento_paciente_tempo",
            height = 450
          ),
          
          sliderInput(
            inputId = 'inANO2',
            label = 'Ano',
            min = 2008,
            max = 2019,
            value=2019
          )
          
        ), # box
        
        box(
          title = 'Onde - Cobertura populacional e de municípios no estado.', 
          status = 'primary', 
          width = 6,
          solidHeader = TRUE, 
          collapsible = TRUE,
          footer = 'Denominador: população conforme senso 2010.',
          
          infoBox(
            title = HTML("<p title='segundo município de residência do usuário'><strong>Municípios</strong></p>"),
            textOutput("tf_pcdt__qt_municipios_paciente"), icon = icon("city"), color = "green",
            fill = TRUE, 
            width = NULL
          ),
          
          plotOutput(
            "tf_pcdt_municipio_residencia",
                     height = 500
          )
        ),  
          
        box(
          title = 'Onde - Número de estabelecimentos por estado', 
          status = 'primary', 
          width = 6,
          solidHeader = TRUE, 
          collapsible = TRUE,
          footer = 'Denominador: população conforme senso 2010.',
          
          infoBox(
            title = HTML("<p title='segundo o CNES'><strong>Estabelecimentos</strong></p>"),
            textOutput("tf_pcdt_estabelecimento_uf_ano"), 
            icon = icon("hospital"), color = "green",
            fill = TRUE, 
            width = NULL
          ),
          
          plotOutput("tf_pcdt_estabelecimento_uf"),
          
          sliderInput(
            inputId = 'inANO',
            label = 'Ano',
            min = 2008,
            max = 2019,
            value=2019
          )
          
        ), # box          
          
        box(
          title = 'Quem - óbtidos dos usuários do PCDT', 
          status = 'primary', 
          width = 6,
          solidHeader = TRUE, 
          collapsible = TRUE,        
          footer = 'Óbitos registrados no Sistema de Informação Ambulatorial - SIA.',
            
          infoBox(
            title = HTML("<p title='Óbitos registrados no SIASUS'><strong>Óbitos</strong></p>"),
            textOutput("tf_pcdt__qt_obito_paciente"), icon = icon("user-minus"), color = "green",
            fill = TRUE, 
            width = NULL
          ),
            
          plotOutput("tf_pcdt_paciente_obito")
            
        ), # box

        box(
          title = 'O que - Troca de medicamentos', 
          status = 'primary', 
          width = 8,
          solidHeader = TRUE, 
          collapsible = TRUE,        
          footer = 'Log do número de usuários.',
          
          plotOutput("tf_troca", height = "1000px")
        ),        
                  
        box(
          title = 'O que - Procedimentos e diagnósticos', 
          status = 'primary', 
          width = 6,
          solidHeader = TRUE, 
          collapsible = TRUE, 
          footer = 'Óbitos registrados no Sistema de Informação Ambulatorial - SIA.',
          
          tableOutput("tf_apac_medicamento_sumario")
        )

      ),
      
      # Tab explore
      tabItem(
        tabName = "escolha",
        fluidRow( # fluidRow 1
          
          valueBox(
            "SABEIS",
            "Sala Aberta de Inteligência em Saúde", 
            icon = icon("couch"),
            width = 8,
            color = "red"
          ),
          
          valueBox(
            "PCDT",
            "Protocolos Clínicos e Diretrizes Terapêuticas - Medicamentos", 
            icon = icon("map-signs"),
            width = 4,
            color = "purple"
          ),
          
          box(
            title = '', 
            status = 'primary', 
            width = 5,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
          HTML("<h3>O Ministério da Saúde disseminou <b>254.433.772</b> registros do Sistema de Informações Ambulatoriais de Saúde - SIA para <b>6.505.722</b> de usuários atendidos entre <b>2008</b> e junho de <b>2019</b>.</h3>")
          
          ),
          
          
          box(
            title = '', 
            status = 'primary', 
            width = 7,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            HTML("<h3>Foram <b>714</b> diagnósticos segundo a Classificação Internacional de Doenças - CID10 para <b>537</b> procedimentos SIGTAP - Sistema de Gerenciamento da Tabela de Procedimentos, Medicamentos e OPM do SUS, sendo registrado <b>50.210.299.891,76</b> de reais não ajustados.</p></h3>")
            
          ),
          
          box(
            title = 'Usuários ao mês por habitante segundo o PCDT', 
            status = 'primary', 
            width = 12,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            plotOutput("tf_pcdt_cns")
            
          ),
          
          box(
            title = 'Atenção', 
            status = 'danger', 
            width = 12,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            HTML("<h3>As informações veiculadas devem ser ponderadas, sobretudo quanto à cobertura. Trata-se apenas de dados ambulatoriais focados em medicamentos de alto custo providos pelo Sistema Único de Saúde. Logo, não estão presentes dados do Sistema de Informação Hospitalar - SIH, Sistema Informação de Mortalidade - SIM, saúde suplementar entre outros.</h3>")
            
          )          
          
        ) # fluidRow 1
      ),
      
      # Tab extrapole
      tabItem(
        tabName = "extrapole",
        fluidRow(    
          
          valueBox(
            "Extrapole",
            "",
            icon = icon("chart-line"),
            width = 12,
            color = "red"
          ),          
          
          valueBox(
            textOutput("td_pcdt_cid__no_pcdt2"),
            "Sala do Protocolo Clínico e Diretrizes Terapêuticas - PCDT",
            icon = icon("map-signs"),
            width = 9,
            color = "purple"
          ),
          
          
          valueBox(
            textOutput("tf_pcdt__nu_competencia2"),
            HTML("<p title='Período conforme o mês de competência (aaaamm)'><strong>Período</strong></p>"),
            icon = icon("calendar"),
            width = 3,
            color = "purple"
          ),

          
          box(
            title = 'Escolha um medicamento', 
            status = 'primary', 
            width = 3,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            # selectizeInput(
            #   inputId = 'inABREV', 
            #   label = '', 
            #   choices=c("ADAL40","ABAT250","ETAN25","ETAN50")
            # )
            
            htmlOutput("td_pcdt_abrev")
            
          ),
          
          valueBox(
            textOutput("td_procedimento_abrev"),
            "Medicamento",
            icon = icon("pills"),
            width = 12,
            color = "blue"
          ),
          
          
          box(
            title = 'Número de usuários atentidos por trimestre e previstos com o método ARIMA', 
            status = 'primary', 
            width = 7,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            plotOutput("tf_pcdt_medicamento_trimestre_cns")
            
          ),
          
          box(
            title = 'Previsão de usuários ARIMA', 
            status = 'primary', 
            width = 5,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            dataTableOutput(
              "tf_pcdt_medicamento_trimestre_cns_dt"
            )
            
          ),
          
          box(
            title = 'O que - Projeção do número de dispensações por trimeste e previsão com o método ARIMA', 
            status = 'primary', 
            width = 7,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            plotOutput("tf_pcdt_medicamento_trimestre_dispensacao")
            
          ),

          box(
            title = 'Previsão de dispensações ARIMA', 
            status = 'primary', 
            width = 5,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            dataTableOutput("tf_pcdt_medicamento_trimestre_dispensacao_dt")
            
          ),          
                    
          box(
            title = 'O que - Projeção da quantidade aprovada por trimestre e prevista com o método ARIMA', 
            status = 'primary', 
            width = 7,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            plotOutput("tf_pcdt_medicamento_trimestre_aprovada")
            
          ),
          
          box(
            title = 'Previsão de quantidade aprovada ARIMA', 
            status = 'primary', 
            width = 5,
            solidHeader = TRUE, 
            collapsible = FALSE,
            
            dataTableOutput("tf_pcdt_medicamento_trimestre_aprovada_dt")
            
          )          
          
        )
      ),
      
      # Tab wiki
      tabItem(
        tabName = "wiki",
        h1("Wiki"),
        box(
#          title = "BoxTest",
          downloadButton("report", "Exportar wiki"),
          uiOutput('mymarkdown'),
          width = NULL
        )
      ),
      
      # Tab sobre
      tabItem(
        tabName = "sobre",
        h1("Sobre")
      )

    ) # tabItems
  ) # dashboardBody
) # dashboardPage ui


server <- function(input, output) {

  td_pcdt_cid = read.csv(file = 'db_sabeis.td_pcdt_cid.csv')
  tf_pcdt = subset(read.csv(file = 'db_sabeis.tf_pcdt.csv'), sg_uf == 'BR')
  
  tf_pcdt_doenca_trimestre     = read.csv(file = 'db_sabeis.tf_pcdt_doenca_trimestre.csv')
  tf_pcdt_estabelecimento      = read.csv(file = 'db_sabeis.tf_pcdt_estabelecimento.csv')
  tf_pcdt_municipio_residencia = read.csv(file = 'db_sabeis.tf_pcdt_municipio_residencia.csv')
  tf_pcdt_paciente             = read.csv(file = 'db_sabeis.tf_pcdt_paciente.csv')
  tf_pcdt_paciente$qt_paciente_m=tf_pcdt_paciente$qt_paciente_m/tf_pcdt_paciente$qt_registros_pcdt*100
  tf_pcdt_paciente$qt_paciente_f=tf_pcdt_paciente$qt_paciente_f/tf_pcdt_paciente$qt_registros_pcdt*100
  
  tf_pcdt_produto_ano          = read.csv(file = 'db_sabeis.tf_pcdt_produto_ano_outros.csv')
  tf_pcdt_produto_ano$nu_ano_competencia = as.factor(tf_pcdt_produto_ano$nu_ano_competencia)
  tf_pcdt_produto_ano$qt_cns_paciente_percentual = round(tf_pcdt_produto_ano$qt_cns_paciente_percentual)
  
  tf_pcdt_doenca_trimestre=read.csv(file="db_sabeis.tf_pcdt_doenca_trimestre.csv")
  tf_pcdt_doenca_trimestre$nu_trimestre = as.factor(tf_pcdt_doenca_trimestre$nu_trimestre)
  names(tf_pcdt_doenca_trimestre)[names(tf_pcdt_doenca_trimestre) == "co_cid10_diagnostico_principal"] <- "CID10"
  
  tf_pcdt_municipio_residencia=read.csv(file="db_sabeis.tf_pcdt_municipio_residencia_cobertura.csv")
  
  
  shp <- readOGR(".", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
  tf_pcdt_estabelecimento_uf=read.csv(file="db_sabeis.tf_pcdt_estabelecimento_uf.csv")
  
  tf_pcdt_estabelecimento_uf_ano = tf_pcdt_estabelecimento_uf %>%
    group_by(co_pcdt, nu_ano_competencia) %>%
    summarise(a_sum=sum(qt_cnes_estabelecimento))
  
  tf_pcdt_cns = read.csv(file="db_sabeis.tf_pcdt_cns.csv")
  
  
  
  
  
  td_pcdt_abrev = read.csv(file = 'db_sabeis.td_pcdt_abrev.csv')
  output$td_pcdt_abrev <- renderUI({
    
    available <- subset(td_pcdt_abrev, co_pcdt == input$inPCDT)$sg_procedimento
    
    selectInput(
      inputId = "inABREV", 
      label = "",
      choices = unique(available) 
      # , selected = unique(available)[1]
    )
    
  })
  
  # output$td_pcdt_abrev = renderUI({
  #   subset(td_pcdt_abrev, co_pcdt == 10) #$sg_procedimento
  # })
  
  output$td_pcdt_cid__no_pcdt1 = renderText({
      paste0(unique(subset(td_pcdt_cid, co_seq_pcdt == input$inPCDT)$no_pcdt))
  })  
  output$td_pcdt_cid__no_pcdt2 = renderText({
    paste0(unique(subset(td_pcdt_cid, co_seq_pcdt == input$inPCDT)$no_pcdt))
  })  
  
  output$td_pcdt_cid__no_pcdt3 = renderText({
    paste0(unique(subset(td_pcdt_cid, co_seq_pcdt == input$inPCDT)$no_pcdt))
  })  
  
  output$tf_pcdt__qt_registros = renderText({
    prettyNum(subset(tf_pcdt, co_pcdt == input$inPCDT)$qt_registros, big.mark = ".", decimal.mark = ",")
  })  
  
  
  output$tf_pcdt__qt_cns_pacientes = renderText({
    prettyNum(subset(tf_pcdt, co_pcdt == input$inPCDT)$qt_cns_pacientes, big.mark = ".", decimal.mark = ",")
  })    
  
  output$tf_pcdt__nu_competencia = renderText({
    paste0(
      subset(tf_pcdt, co_pcdt == input$inPCDT)$nu_competencia_min,
      " a ", 
      subset(tf_pcdt, co_pcdt == input$inPCDT)$nu_competencia_max
    )
  })    

  output$tf_pcdt__nu_competencia2 = renderText({
    paste0(
      subset(tf_pcdt, co_pcdt == input$inPCDT)$nu_competencia_min,
      " a ", 
      subset(tf_pcdt, co_pcdt == input$inPCDT)$nu_competencia_max
    )
  })    
  
    
  output$tf_pcdt_estabelecimento_uf_ano = renderText({
    prettyNum(subset(tf_pcdt_estabelecimento_uf_ano, co_pcdt == input$inPCDT & nu_ano_competencia == input$inANO)$a_sum, big.mark = ".", decimal.mark = ",")
  })    
  
  output$tf_pcdt__qt_municipios_paciente = renderText({
    prettyNum(subset(tf_pcdt, co_pcdt == input$inPCDT)$qt_municipios_paciente, big.mark = ".", decimal.mark = ",")
  })    
  
  output$tf_pcdt__qt_secretarias = renderText({
    prettyNum(subset(tf_pcdt, co_pcdt == input$inPCDT)$qt_secretarias, big.mark = ".", decimal.mark = ",")
  })    
  
  output$tf_pcdt__vl_aprovado_ipca = renderText({
    prettyNum(subset(tf_pcdt, co_pcdt == input$inPCDT)$vl_aprovado_ipca, big.mark = ".", decimal.mark = ",")
  })    
  
  output$tf_pcdt__qt_sigtap = renderText({
    prettyNum(subset(tf_pcdt, co_pcdt == input$inPCDT)$qt_sigtap, big.mark = ".", decimal.mark = ",")
  })      
  
  output$tf_pcdt__qt_obito_paciente = renderText({
    prettyNum(subset(tf_pcdt, co_pcdt == input$inPCDT)$qt_obito_paciente, big.mark = ".", decimal.mark = ",")
  })
  
  output$tf_pcdt_paciente = renderPlot({
    pyramid(
      subset(tf_pcdt_paciente, co_pcdt == input$inPCDT)[,c(2,3,6)], 
      Llab="Masculino",Rlab="Feminino", 
      Lcol="navy", 
      Ldens=5, 
      Rcol="red", 
      Rdens=10, GL=FALSE, Clab = "Idade", main = "usuários (%)")
  })
    
  output$tf_pcdt_paciente_obito = renderPlot({
    pyramid(subset(tf_pcdt_paciente, co_pcdt == input$inPCDT)[,c(4,5,6)], Llab="Masculino",Rlab="Feminino", Lcol="navy", Ldens=5, Rcol="red", 
            Rdens=10, GL=FALSE, Clab = "Idade", main = "usuários (n)")
  })
  
  output$tf_pcdt_produto_ano = renderPlot({
    ggplot(
      subset(tf_pcdt_produto_ano, co_pcdt==input$inPCDT), 
      aes(
        fill=sg_procedimento, 
        y=qt_cns_paciente_percentual, 
        x=nu_ano_competencia,
        label = qt_cns_paciente_percentual
      )
    ) + geom_bar(stat="identity") + 
      xlab('ano') +
      ylab('usuários (%)') + 
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      coord_flip() +
      theme(
        legend.text=element_text(size=rel(1.3)),
        axis.title.x = element_text(size=rel(1.3)),
        axis.title.y = element_text(size=rel(1.3)),
        axis.text.x = element_text(size=rel(1.3)),
        axis.text.y = element_text(size=rel(1.3))
      )
  })
    

  output$tf_pcdt_doenca_trimestre = renderPlot({
    ggplot(
      subset(tf_pcdt_doenca_trimestre, co_pcdt == input$inPCDT & co_seq_pcdt_cid10_trimestre > 1), 
      aes(
        x=nu_trimestre, y=qt_cns_paciente, group=CID10, color=CID10
      )) + 
      geom_line() + 
      labs(y = "usuários novos", x = "trimestre") +
      theme(
        legend.text=element_text(size=rel(1.3)),
        axis.title.x = element_text(size=rel(1.3)),
        axis.title.y = element_text(size=rel(1.3)),
        axis.text.x = element_text(angle = 45, hjust = 1, size=rel(1.3)),
        axis.text.y = element_text(size=rel(1.3))
      )
  })
      
  output$tf_pcdt_municipio_residencia = renderPlot({
    plot(
      subset(tf_pcdt_municipio_residencia, co_pcdt == input$inPCDT)$qt_cobertura ~ 
        subset(tf_pcdt_municipio_residencia, co_pcdt == input$inPCDT)$qt_cns_mes_habitante,
      xlab="usuarios / 1 milhão de habitantes ao mes",
      ylab="municipios cobertos no estado (%)",
      col= "white",
      ylim=c(0,100)
    )
    
    text(
      subset(tf_pcdt_municipio_residencia, co_pcdt == input$inPCDT)$qt_cobertura ~
        subset(tf_pcdt_municipio_residencia, co_pcdt == input$inPCDT)$qt_cns_mes_habitante, 
      labels=subset(tf_pcdt_municipio_residencia, co_pcdt == input$inPCDT)$sg_uf, cex= 1
    )
  })
  
  output$mymarkdown <- renderUI({  
    rmarkdown::render(input = "mymarkdown.Rmd",
                      output_format = html_document(self_contained = TRUE),
                      output_file = 'mymarkdown.html')  
    shiny::includeHTML('mymarkdown.html') 
  }) 
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "mymarkdown.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "mymarkdown.Rmd")
      file.copy("mymarkdown.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$tf_pcdt_estabelecimento_uf = renderPlot({
    shp@data$Y = subset(tf_pcdt_estabelecimento_uf, co_pcdt == input$inPCDT & nu_ano_competencia == input$inANO)$qt_cnes_estabelecimento[match(shp@data$CD_GEOCUF, subset(tf_pcdt_estabelecimento_uf, co_pcdt == input$inPCDT & nu_ano_competencia == input$inANO)$co_uf_ibge_estabelecimento)]
    spplot(shp, "Y",  col.regions=grey.colors(n=100, start = 1, end = 0))
  })
      
  
  tf_apac_medicamento_sumario = read.csv(file = 'db_sabeis.tf_apac_medicamento_sumario.csv')
    
  output$tf_apac_medicamento_sumario = renderTable({
    tb=subset(
      tf_apac_medicamento_sumario, 
      co_pcdt == input$inPCDT & co_sigtap_procedimento > 0
    )[,c(2,13,3,11,12,8,4,5)]
    
    colnames(tb)=c("CID10","diagnóstico principal","SIGTAP","sigla","procedimento","usuários","inicio","fim")
    as.data.frame(tb)
    
  })
  
  output$tf_pcdt_cns = renderPlot({
    treemap(tf_pcdt_cns,
            index=c("sg_uf","no_pcdt"),
            vSize="qt_cns_mes",
            type="index",
            title=""
    ) 
  })

  tf_pcdt_medicamento_trimestre = read.csv(file="db_sabeis.tf_pcdt_medicamento_trimestre.csv")  
  td_pcdt_abrev  = read.csv(file="db_sabeis.td_pcdt_abrev.csv")  
  td_trimestre = read.csv(file="td_trimestre.csv")
  td_trimestre.plot = read.csv(file="td_trimestre.plot.csv")
  
  
  
  output$tf_pcdt_medicamento_trimestre_cns = renderPlot({
    
    l=dim(subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT))[1] # numero de afericoes
    p=which(td_trimestre == subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)[1,2]) # inicio do vetor
    f=20 # trimestres para previsao
  
    
    plot(
      forecast(auto.arima(subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)$qt_cns_paciente, allowmean = TRUE), h = 20),
      xlab = "", 
      ylab="usuários", 
      main="", 
      las = 3  , xaxt='n'
    )
    
    axis(1, at=1:(l+f+1), labels=td_trimestre.plot[p:(p+l+f),], las = 3)
    
  })

  output$tf_pcdt_medicamento_trimestre_cns_dt = renderDataTable({
    tb=subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)
    l=dim(tb)[1] # numero de afericoes
    p=which(td_trimestre == tb[1,2]) # inicio do vetor
    trimestre=td_trimestre[(p+l):(p+l+19),]
    q=forecast(auto.arima(tb$qt_cns_paciente, allowmean = TRUE), h = 20)
    q=cbind(trimestre,round(q$mean),round(q$lower),round(q$upper))
    q[q<0] <- 0
    colnames(q)=c("trimestre","previsao","min80","min95","max80","max95")
    q
  }, options = list(pageLength = 6))  
  
  output$tf_pcdt_medicamento_trimestre_dispensacao = renderPlot({    

    l=dim(subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT))[1] # numero de afericoes
    p=which(td_trimestre == subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)[1,2]) # inicio do vetor
    f=20 # trimestres para previsao
    
    plot(
      forecast(auto.arima(subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)$qt_dispensacao/1000, allowmean = TRUE), h = 20),
      xlab = "", 
      ylab="Dispensações (x1000)", 
      main="", 
      las = 3  , xaxt='n'
    )
    
    axis(1, at=1:(l+f+1), labels=td_trimestre.plot[p:(p+l+f),], las = 3)
    
  }) 
    
  output$tf_pcdt_medicamento_trimestre_dispensacao_dt = renderDataTable({
    tb=subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)
    l=dim(tb)[1] # numero de afericoes
    p=which(td_trimestre == tb[1,2]) # inicio do vetor
    trimestre=td_trimestre[(p+l):(p+l+19),]
    q=forecast(auto.arima(tb$qt_dispensacao, allowmean = TRUE), h = 20)
    q=cbind(trimestre,round(q$mean),round(q$lower),round(q$upper))
    q[q<0] <- 0
    colnames(q)=c("trimestre","previsao","min80","min95","max80","max95")
    q
  }, options = list(pageLength = 6))  
  
  
  
  output$tf_pcdt_medicamento_trimestre_aprovada = renderPlot({      
    
    l=dim(subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT))[1] # numero de afericoes
    p=which(td_trimestre == subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)[1,2]) # inicio do vetor
    f=20 # trimestres para previsao
    
    plot(
      forecast(auto.arima(subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)$qt_aprovada/1000, allowmean = TRUE), h = 20),
      xlab = "", 
      ylab="Quantidade aprovada (x1000)", 
      main="", 
      las = 3  , xaxt='n'
    )
    
    axis(1, at=1:(l+f+1), labels=td_trimestre.plot[p:(p+l+f),], las = 3)
  })  

  output$tf_pcdt_medicamento_trimestre_aprovada_dt = renderDataTable({
    tb=subset(tf_pcdt_medicamento_trimestre, sg_procedimento == input$inABREV & co_pcdt == input$inPCDT)
    l=dim(tb)[1] # numero de afericoes
    p=which(td_trimestre == tb[1,2]) # inicio do vetor
    trimestre=td_trimestre[(p+l):(p+l+19),]
    q=forecast(auto.arima(tb$qt_aprovada, allowmean = TRUE), h = 20)
    q=cbind(trimestre,round(q$mean),round(q$lower),round(q$upper))
    q[q<0] <- 0
    colnames(q)=c("trimestre","previsao","min80","min95","max80","max95")
    q
  }, options = list(pageLength = 6))  
  
  
  td_procedimento_abrev = read.csv(file="db_sabeis.td_procedimento_abrev.csv")
  output$td_procedimento_abrev = renderText({
    paste0(subset(td_procedimento_abrev, sg_procedimento == input$inABREV)$no_procedimento)
  })
  
  tf_troca = read.csv(file="db_sabeis.tf_troca.csv")
  output$tf_troca = renderPlot({      
    
    data_long = subset(tf_troca, co_pcdt == input$inPCDT)
    data_long$co_pcdt=NULL
    data_long$sg_procedimentob2=NULL
    data_long$qt_cns_paciente=log(data_long$qt_cns_paciente)
    
    
    circos.clear()
    circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
    par(mar = rep(0, 4))
    
    # color palette
    mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
    mycolor <- mycolor[sample(1:10)]
    
    # Base plot
    chordDiagram(
      x = data_long, 
      #  grid.col = mycolor,
      transparency = 0.25,
      directional = 1,
      direction.type = c("arrows", "diffHeight"), 
      diffHeight  = -0.04,
      annotationTrack = "grid", 
      annotationTrackHeight = c(0.05, 0.1),
      link.arr.type = "big.arrow", 
      link.sort = TRUE, 
      link.largest.ontop = TRUE)
    
    # Add text and axis
    circos.trackPlotRegion(
      track.index = 1, 
      bg.border = NA, 
      panel.fun = function(x, y) {
        
        xlim = get.cell.meta.data("xlim")
        sector.index = get.cell.meta.data("sector.index")
        
        # Add names to the sector. 
        circos.text(
          x = mean(xlim), 
          y = 3.2, 
          labels = sector.index, 
          facing = "bending", 
          cex = 0.8
        )
        
        # Add graduation on axis
        circos.axis(
          h = "top", 
          major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
          minor.ticks = 1, 
          major.tick.percentage = 0.57,
          labels.niceFacing = FALSE)
      }
    )
    
    
  })
  
  # ----
  tf_procedimento_paciente_tempo=
    read.csv(
      file="db_sabeis.tf_procedimento_paciente_tempo.csv"
    )
  
  output$tf_procedimento_paciente_tempo = renderPlot({
    
    defect <- 
      subset(
        tf_procedimento_paciente_tempo, 
        nu_ano_competencia == input$inANO2 & co_pcdt==input$inPCDT
      )$qt_paciente_tempo
    
    names(defect) <- 
      subset(
        tf_procedimento_paciente_tempo, 
        nu_ano_competencia == input$inANO2 & co_pcdt==input$inPCDT
      )$sg_procedimento
    
    qcc.options(bg.margin = "white")
    
    pareto.chart(
      defect, 
      ylab = "usuários-ano", 
      ylab2 = "", 
      main="", 
      cumperc = seq(0, 100, by = 5)  # ranges of the percentages at the right  
    )
    
  })
  
}

shinyApp(ui, server)