library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(dplyr)
library(lubridate)
library(formattable)
library(DT)
library(readxl)
library(tidyverse)
library(httr)

options(spinner.color="grey",spinner.color.background="#ffffff",spinner.size=2)


header <- dashboardHeader(title = "IRAS - ANVISA")

sidebar <- dashboardSidebar(
  sidebarMenu(id="tab",
              dateRangeInput("date","Data",start=as.Date("2012-01-01"),end=as.Date("2020-12-31")),
              
              radioButtons("select1","Unidade Hospitalar",choices=c("UTI Adulto"="UTI.ADULTO","UTI Neonatal"="UTI.NEONATAL",
                                                                    "UTI Pediátrica"="UTI.PEDIATRICA","Centro Cirúrgico"="CENTRO.CIRURGICO")),
              tags$hr(),
              radioButtons("select2","Tipo de infecção",choices=c("IPCSL","ISC_PARTOS_CES","ISC_PROT_CARDIACA","ISC_PROT_NEURO","ITU","PAV"))
  )
)


page <- dashboardBody(
  fluidRow(
    box(width = 12,
        h3(textOutput("title1")),
        #column(6,plotlyOutput("plot1",height = 400)),
        column(6,withSpinner(plotlyOutput("plot1",height = 400),type=2)),
        column(6,div(style = "overflow-y:scroll;",dataTableOutput("table1")))
    )
  ),
  hr(),
  fluidRow(
    box(width = 12,
        h3(textOutput("title2")),
        #column(6,plotlyOutput("plot2",height = 400)),
        column(6,withSpinner(plotlyOutput("plot2",height = 400),type=2)),
        column(6,div(style = "overflow-y:scroll;",dataTableOutput("table2")))
    )
  )
)


ui <- dashboardPage(header,sidebar,page,skin='black')

server <- function(input, output) {
  db=data.frame()
  for(i in c(2012:2020)){
    temp <- readxl::read_excel(paste0("data/iras_",i,".xlsx"))
    db=dplyr::bind_rows(temp,db)
  }
  
  db$TIPO.INFEC=ifelse(db$TIPO.INFEC=="IPCSC","IPCSL",db$TIPO.INFEC)
  
  # for(i in c(2012:2020)){
  #   github_link <- paste0("https://github.com/barbosasil/IRAS/raw/main/iras_",i,".xlsx")
  #   temp_file <- tempfile(fileext = ".xlsx")
  #   req <- GET(github_link,
  #              authenticate(Sys.getenv("GITHUB_PAT"), ""),
  #              write_disk(path = temp_file))
  #   temp <- readxl::read_excel(temp_file)
  #   db=dplyr::bind_rows(temp,db)
  # }
  
  
  db=data.frame(db)
  db$DATA=as.Date(db$DATA)
  
  output$title1 <-renderText({
    paste0("Número de hospitais com ", input$select1 ," na notificação nacional de IRAS ",input$select2," de ",year(min(input$date))," a ",year(max(input$date)))
  })
  
  output$title2 <-renderText({
    paste0("Comparação dos indicadores de ",input$select2," de ",year(min(input$date))," a ",year(max(input$date)))
  })
  
  output$plot1 <-renderPlotly({
    
    db=db[db$Unidade.Hospitalar==input$select1 & db$TIPO.INFEC==input$select2,]
    
    #db=db[db$Unidade.Hospitalar=="UTI.ADULTO" & db$TIPO.INFEC=="IPCSL",]
    
    temp1=list()
    for(i in unique(db$ANO)){
      temp1[[i]]=data.frame(table(db[db$ANO==i,]$CNES))
    }
    
    temp1=bind_rows(temp1,.id="ano")
    temp1$ano=factor(temp1$ano,levels=c(1:9),labels=c(2012:2020))
    
    tab1=data.frame(table(temp1$ano))
    names(tab1)=c("ano","col1")
    tab1$col2=data.frame(table(temp1[temp1$Freq>=10,]$ano))[,2]
    
    plot_ly(data = tab1,x = ~ano,y = ~col1,type = "bar",name='Pelo menos 1 vez') %>%
      add_trace(y = ~col2, name = 'Pelo menos 10 vezes no ano') %>%
      layout(yaxis = list(title = 'Número'), barmode = 'group') %>%
      layout(legend = list(orientation = 'h')) %>%
      layout(yaxis = list(tickformat = ".0f"))
    
  })
  
  output$table1<-DT::renderDataTable({
    
    db=db[db$Unidade.Hospitalar==input$select1 & db$TIPO.INFEC==input$select2,]
    
    temp1=list()
    for(i in unique(db$ANO)){
      temp1[[i]]=data.frame(table(db[db$ANO==i,]$CNES))
    }
    
    temp1=bind_rows(temp1,.id="ano")
    temp1$ano=factor(temp1$ano,levels=c(1:9),labels=c(2012:2020))
    
    tab1=data.frame(table(temp1$ano))
    names(tab1)=c("ano","col1")
    tab1$col2=data.frame(table(temp1[temp1$Freq>=10,]$ano))[,2]
    
    tab1=rbind(tab1,data.frame(ano="p-valor",col1=summary(lm(tab1$col1~as.numeric(tab1$ano)))$coefficients[2,4],
                               col2=summary(lm(tab1$col2~as.numeric(tab1$ano)))$coefficients[2,4]))
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE)
    
    header.style <- "th { font-family: 'Arial'; font-size:22px ;font-weight: bold; color: white; background-color: #4472C4;}"
    header.names <- c("Ano",paste0("# ",input$select1,"  reportando \n pelo menos 1x/ano"),paste0("# ",input$select1,"  reportando \n pelo menos 10x/ano"))
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    my.table <- datatable(tab1,options = my.options,container = my.container,rownames = F,width = '100%') %>%
      formatStyle(columns = c(2:5),
                  width='200px',
                  fontFamily = "Arial",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width='200px',
                  fontFamily = "Arial",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word") %>%
      formatRound(c(names(tab1)[2],names(tab1)[3]),digits = 0,mark = ",")
    
    print(my.table)
    
  })
  
  output$plot2 <-renderPlotly({
    
    db=db[db$Unidade.Hospitalar==input$select1 & db$TIPO.INFEC==input$select2,]
    
    tab2=data.frame(ano=unique(db$ANO))
    tab2$col2=round(data.frame(tapply(db$No..de.infecção,db$ANO,sum,na.rm=T))[,1],0)
    tab2$col3=round(data.frame(tapply(db$No..Paciente.dia,db$ANO,sum,na.rm=T))[,1],0)
    tab2$col5=round(data.frame(tapply(db$Número.de.pacientes.com.dispositivo.dia,db$ANO,sum,na.rm=T))[,1],0)
    tab2$col4=round(tab2$col2/tab2$col5*1000,1)
    tab2$col6=round(tab2$col5/tab2$col3*100,1)
    
    plot_ly(data = tab2,x = ~ano,y = ~col2,type = "bar",name='Número de cateter') %>%
      add_trace(y = ~col3, name = 'Cateter central por dia') %>%
      add_trace(y = ~col5, name = 'Paciente por dia') %>%
      layout(yaxis = list(title = 'Número'), barmode = 'group') %>%
      layout(legend = list(orientation = 'h')) %>%
      layout(yaxis = list(tickformat = ".0f"))
    
  })
  
  output$table2<-DT::renderDataTable({
    
    db=db[db$Unidade.Hospitalar==input$select1 & db$TIPO.INFEC==input$select2,]
    
    tab2=data.frame(ano=unique(db$ANO))
    tab2$col2=round(data.frame(tapply(db$No..de.infecção,db$ANO,sum,na.rm=T))[,1],0)
    tab2$col3=round(data.frame(tapply(db$No..Paciente.dia,db$ANO,sum,na.rm=T))[,1],0)
    tab2$col5=round(data.frame(tapply(db$Número.de.pacientes.com.dispositivo.dia,db$ANO,sum,na.rm=T))[,1],0)
    tab2$col4=round(tab2$col2/tab2$col5*1000,1)
    tab2$col6=round(tab2$col5/tab2$col3*100,1)
    
    tab2=tab2[,c("ano","col2","col3","col4","col5","col6")]
    
    my.options <- list(autoWidth = FALSE,
                       searching = FALSE,
                       ordering = FALSE,
                       lengthChange = FALSE,
                       lengthMenu = FALSE,
                       pageLength = FALSE,
                       paging = FALSE,
                       info = FALSE)
    
    header.style <- "th { font-family: 'Arial'; font-size:22px ;font-weight: bold; color: white; background-color: #4472C4;}"
    header.names <- c("Ano",paste0("# ", input$select2),"# cateter por dia",paste0("di ", input$select2),"# paciente por dia","Taxa de utilização")
    my.container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
        )
      )
    ))
    
    
    my.table <- datatable(tab2,options = my.options,container = my.container,rownames = F,width = '100%') %>%
      formatStyle(columns = c(2:6),
                  width='200px',
                  fontFamily = "Arial",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "center",
                  wordWrap = "break-word") %>%
      formatStyle(columns = c(1),
                  width='200px',
                  fontFamily = "Arial",
                  fontSize = "14px",
                  borderRightWidth = "1px",
                  borderRightStyle = "solid",
                  borderRightColor = "white",
                  borderBottomColor = "#ffffff",
                  borderBottomStyle = "solid",
                  borderBottomWidth = "1px",
                  borderCollapse = "collapse",
                  verticalAlign = "middle",
                  textAlign = "left",
                  wordWrap = "break-word") %>%
      formatRound(c(names(tab2)[2],names(tab2)[3],names(tab2)[5]),digits = 0,mark = ",") %>%
      formatRound(c(names(tab2)[4],names(tab2)[6]),digits = 1,mark = ",")
    
    print(my.table)
    
  })
  
}

shinyApp(ui, server)







