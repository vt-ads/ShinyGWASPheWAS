library(shiny)
library(plotly)
library(datasets)
library(ggplot2)
library(stringr)
library(plyr)
library(tidyverse)
library(reshape2)
library(ggrepel)
library(periscope)
library(shinybusy)
#setwd("~/Insync/rafael.yassue@usp.br/Google Drive/Ubuntu_RY/PHEGWAS/ShinyGWASPheWAS")
options(shiny.maxRequestSize=100*1024^2) 
#setwd(gsub("shiny.R", "",rstudioapi::getActiveDocumentContext()$path))

ui <-shinyUI(fluidPage(# 1
  titlePanel("ShinyGWASPheWAS"),
    tabsetPanel(#1
    tabPanel("Introduction", #3
             mainPanel(
              hr(),
              tags$p("This Shiny app allows the users to interpret multiple phenotype GWAS results interactively using Two-way Manhattan and PheWAS plots."),
              hr(),
              h4("Two-way Manhattan"),
              tags$p("The two-way Manhattan plot helps visualize GWAS results, where there are two factors of interest. For example, different traits and management conditions."),
              tags$p("The interactive plots allow the user to identify candidate single nucleotide polymorphisms (SNPs) associated with several phenotypes along with additional information, such as p-value, chromosome, and genomic position."),
              imageOutput("myImage"),
              h5("Usage"),
              tags$li("Download the code"),
              tags$li("In R, run shiny::runApp()"),
              tags$li("The user must run the GWAS analysis externally using any software, such as GAPIT,rrBLUP, JWAS, etc..."),
              tags$li("The user input can be a file separated by a comma, semicolon, or tab and specify quote"),
              tags$li("The dataset is downloadable from the bottom of the app"),
              tags$li("The dataset contains GWAS analysis of 13,826 SNPs for plant height (PH), stalk diameter (SD), and shoot dry mass (SDM) under two management conditions (B+ and B-)"),
              tags$li("The user must identify the columns for Marker_ID, Marker position, posterior inclusion probability (PIP) or p value, chromosome, and factors 1 and 2 in the input that will be used for the plotting"),
              tags$li("Changing the threshold, ylim, point size, and Y and X axes is possible"),
              tags$li("Only a subset (85%) of the markers with PIP or p value < 0.05 are plotted to save computing time"),
              h4("PheWAS plot"),
              tags$p("Interpreting GWAS analysis from hundreds to thousands of different phenotypes can be challenging. In this sense, PheWAS plots can efficiently help visualize the associations between SNPs and phenotypes."),
              imageOutput("myImage2"),
              hr(),
              h5("Usage"),
              tags$li("Download the code"),
              tags$li("In R, run shiny::runApp()"),
              tags$li("The user must run the GWAS analysis externally using any software, such as GAPIT,rrBLUP, JWAS, etc..."),
              tags$li("The user input can be a file separated by a comma, semicolon, or tab and specify quote"),
              tags$li("The dataset is downloadable from the bottom of the app"),
              tags$li("The dataset contains the summary of GWAS analysis for 281 hyperspectral phenotypes and three manually measured phenotypes (PH, SD, and SDM) for 10 SNPs"),
              tags$li("The user must identify the columns for Marker_ID, phenotype group, phenotype ID (trait), and PIP or p value in the input that will be used for the plotting"),
              tags$li("Changing the threshold, ylim, point size, number of columns, and Y and X axes is possible"),
              h4("How to cite ShinyGWASPheWAS"),
              tags$li("Insert reference here"),
              h4("Contact Information and support:"),
              tags$li("Rafael Massahiro Yassue, rafael.yassue@gmail.com")
             )
             
    ),
    
    # 1 
    tabPanel("Two-way Manhattan plot", #3
             titlePanel("Two-way Manhattan plot"),
             sidebarLayout(  
               sidebarPanel( 
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')), #5
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                 radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"), '"'),
                 selectInput('marker_ID', 'Marker_ID', ""),
                 selectInput('posi', 'Position', ""),
                 selectInput('pvalue', 'PIP', "", selected = ""),
                 selectInput('chromosome', 'Chromosome', "", selected = ""),
                 checkboxInput('highlight', 'Threshold', TRUE),
                 selectInput('trait1', 'Factor 1', "", selected = ""),
                 selectInput('trait2', 'Factor 2', "", selected = ""),
                 numericInput("obs", "Threshold:", 1),
                 numericInput("ylim", "ylim:", 1),
                 numericInput("point1", "Point size:", 0.25),
                 textInput("xlab","Xlab"," "),
                 textInput("ylab","Ylab","PIP"),
                 downloadButton("downloadData", "Download two-way Manhattan data")
                 
               ),
               mainPanel(plotlyOutput('MyPlot'), add_busy_spinner(spin = "fading-circle"))
             )),
    # Second screen
    tabPanel("PheWAS plot",
             pageWithSidebar(
               headerPanel('PheWAS plot'),
               sidebarPanel(
                 fileInput('file2', 'Choose CSV File',
                           accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')), #5
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                 radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"), '"'),
                 selectInput('marker_ID2', 'Marker_ID', ""),
                 selectInput('pheno_cor', 'Type', ""),
                 selectInput('pheno_name', 'Pheno', ""),
                 selectInput('pvalue2', 'PIP', "", selected = ""),
                 checkboxInput('highlight2', 'Threshold', TRUE),
                 numericInput("obs2", "Threshold:", 1),
                 numericInput("ylim2", "ylim:", 1),
                 numericInput("point2", "Point size:", 0.25),
                 numericInput("ncols", ("Ncols"),2),
                 textInput("xlab2","Xlab"," "),
                 textInput("ylab2","Ylab","PIP"),
                 downloadButton("downloadData2", "Download PheWAS data")
                 
               ),
               mainPanel(plotlyOutput('PheWAS_plot'), add_busy_spinner(spin = "fading-circle"))
             ))
  )#3
)#2
)#1



server <- shinyServer(function(input, output, session) {
  output$myImage <- renderImage({
    list(src = "fig01.png",
         contentType = 'image/png',
         width = 1800/4,
         height = 1600/4,
         alt = "fig01")
  }, deleteFile = F)
  
  output$myImage2 <- renderImage({
    list(src = "fig02.png",
         contentType = 'image/png',
         width = 1800/4,
         height = 1600/4,
         alt = "fig02")
  }, deleteFile = F)
  data <- reactive({ 
    req(input$file1) 
    inFile <- input$file1 
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'posi', label = 'Position',
                      choices = names(df), selected = names(df)[6])
    updateSelectInput(session, inputId = 'pvalue', label = 'PIP',
                      choices = names(df), selected = names(df)[3])
    updateSelectInput(session, inputId = 'chromosome', label = 'Chromosome',
                      choices = names(df), selected = names(df)[5])
    updateSelectInput(session, inputId = 'trait1', label = 'Factor 1',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'trait2', label = 'Factor 2',
                      choices = names(df), selected = names(df)[4])
    updateSelectInput(session, inputId = 'marker_ID', label = 'Marker_ID',
                      choices = names(df), selected = names(df)[2])
    return(df)})
  data2 <- reactive({ 
    req(input$file2) 
    inFile2 <- input$file2 
    df2 <- read.csv(inFile2$datapath, header = input$header, sep = input$sep,
                    quote = input$quote)
    updateSelectInput(session, inputId = 'pheno_cor', label = 'Phenotype group',
                      choices = names(df2), selected = names(df2)[7])
    updateSelectInput(session, inputId = 'pheno_name', label = 'Phenotype ID',
                      choices = names(df2), selected = names(df2)[1])
    updateSelectInput(session, inputId = 'pvalue2', label = 'PIP',
                      choices = names(df2), selected = names(df2)[3])
    updateSelectInput(session, inputId = 'marker_ID2', label = 'Marker_ID',
                      choices = names(df2), selected = names(df2)[2])
    return(df2)})
  # Tab 1
  output$MyPlot <- renderPlotly({
    dados <- data()[, c(input$posi, input$pvalue, input$chromosome,input$trait1, input$trait2, input$marker_ID)]
    colnames(dados)<-c("Index", "pvalue", "Chromosome", "trait1", "trait2", "Marker_ID")
    dados = dados[-sample(which(dados$pvalue < 0.05), size = length(which(dados$pvalue < 0.05))*0.85),]
    Chromosome<-unique(dados$Chromosome)
    chromossos_size=c()
    for (i in 1:length(Chromosome)){chromossos_size[i]=max(dados$Index[dados$Chromosome==Chromosome[i]])}
    for (i in 2:length(Chromosome)){chromossos_size[i]<-chromossos_size[i]+chromossos_size[i-1]}
    chrom_max_size<-c()
    for (i in 1:length(Chromosome)){
      if (i == 1){chrom_max_size[i]<-max(dados$Index[dados$Chromosome==Chromosome[i]])/2 } else {
        chrom_max_size[i] <- chromossos_size[i-1] + max(dados$Index[dados$Chromosome==Chromosome[i]])/2
      }}
    
    for (i in 2:length(Chromosome)){
      dados$Index[dados$Chromosome==Chromosome[i]]<-dados$Index[dados$Chromosome==Chromosome[i]]+chromossos_size[i-1]
    }
    data_1 <- reactiveValues()
    a <-ggplot(dados, aes(x = Index,colour = Chromosome, label = Marker_ID, y=pvalue)) +
      geom_point(size = input$point1)+facet_grid(as.formula(paste("trait1~ trait2")))+
      theme_bw()  + ylab(input$ylab)+ xlab(input$xlab)+ ylim(0, input$ylim)+
      scale_x_continuous(breaks=chrom_max_size,labels=Chromosome) + 
      theme(legend.position = "none",axis.text.x = element_text(angle = 45, size= 7, hjust = 0),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    if(input$highlight){a <- a + geom_hline(yintercept = input$obs, color="red", alpha = 0.75, size = 0.5, linetype = 2)#+ 
    data_1$plot <- a
    }
    else{
      data_1$plot <- a
    }
    ggplotly(data_1$plot) 
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste("example_manhattan.csv")},
    content = function(file) {write.csv(read.csv("Manhatan_full.csv"), file, row.names = FALSE)})
  
  # Tab 2
  output$PheWAS_plot <- renderPlotly({
    dados2 <- data2()[, c(input$pheno_cor, input$pheno_name, input$pvalue2,input$marker_ID2)]
    colnames(dados2)<-c("Type", "Phenotype", "pvalue", "marker_ID2")
    data_2 <- reactiveValues()
    dados2$Index <- as.numeric(as.factor(dados2$Phenotype))
    PhenoType <- unique(dados2$Type) ; breaks <- c()
    for (i in 1:length(PhenoType)){breaks[i]<- median(dados2[dados2$Type==PhenoType[i], "Index"])}
    
    temp_graph<- ggplot(dados2, aes(x = Index, y=pvalue, colour = Type, label = Phenotype)) +
      geom_point(size = input$point2)+ theme_bw() +facet_wrap(~marker_ID2, ncol =  as.integer(input$ncols))+
      ylab(input$ylab2)+ xlab(input$xlab2)+ ylim(-0.40,input$ylim2)+
      scale_x_continuous(breaks=breaks,labels=unique(dados2$Type)) 
    
    if(input$highlight2){ temp_graph <- temp_graph + geom_hline(yintercept = input$obs2, color="red",alpha = 0.75, size = 0.5, linetype = 2) 
    data_2$plot <- ((temp_graph))
    }
    else{data_2$plot <- (temp_graph)}
    
    ggplotly(data_2$plot)
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function() {paste("example_PheWAS.csv")},
    content = function(file) {write.csv(read.csv("PheWAS.csv"), file, row.names = FALSE)})
  
})

shinyApp(ui, server)
