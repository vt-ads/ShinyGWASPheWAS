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
# Explain the dataset
# add more information
# Explain which dataset 


ui <-shinyUI(fluidPage(# 1
  titlePanel("ShinyGWASPheWAS"),
    tabsetPanel(#1
    tabPanel("Introduction", #3
             titlePanel("Read me"),
             mainPanel(
              h5("This Shiny app allows the users to dynamic interpret multiple phenotype GWAS results using two types of interactive graphics"),
              hr(),
              h4("Two-way Manhattan"),
              tags$p("The two-way Manhattan plot is helpful in visualizing GWAS results where there are two factors of interest, for example, different traits and management."),
              tags$p("The interactive plots allow the user to identify candidate SNPs that may be associated with several phenotypes as any other important information from the GWAS analysis (p.value, chromosome, and genomic position)"),
              imageOutput("myImage"),
              h5("Guidelines"),
              tags$li("The user must run the GWAS analysis externally using any software, such as GAPIT,rrBLUP, JWAS, etc..."),
              tags$li("The user input can be a file separated by a comma, semicolon, or tab and specify quote"),
              tags$li("In the left corner of the page is available for download an example file"),
              tags$li("The example file cointain GWAS analysis of 13,826 single nucleotide polymorphisms (SNPs) for three traits (SDM, SD, PH) under two managements (B+ and B-)"),
              tags$li("The user input must follow a similar structure from the example file. The names and oders of the columns may be different"),
              tags$li("The user must identify the referring column of the Marker_ID, Marker position, p.value, chromosome, factor 1 and 2 in the input that will be used for the plot"),
              tags$li("It is also possible to define the threshold, ylim, point size, Y and X axis labs"),
              h4("PheWAS plot"),
              tags$p("Interpreting GWAS analysis from hundreds to thousands of different phenotypes can be challenging. In this sense, PheWAS plots can help to visualize the associations between SNPs and phenotypes and identify SNPs associated with several phenotypes."),
              imageOutput("myImage2"),
              hr(),
              h5("Guidelines"),
              tags$li("The user must run the GWAS analysis externally using any software, such as GAPIT,rrBLUP, JWAS, etc..."),
              tags$li("The user input can be a file separated by a comma, semicolon, or tab and specify quote"),
              tags$li("In the left corner of the page is available for download examples file."),
              tags$li("The example file contains the summary of GWAS analysis for 281 hyperspectral phenotypes and three manually measured for 10 SNPs. The phenotype_ID is the name of the phenotype and group is the class, if it is a reflectance, index, or  manually measured phenotype"),
              tags$li("The user input must follow a similar structure from the example file. The names and oders columns may be different"),
              tags$li("The user must identify the referring column of the Marker_ID, phenotype group, phenotype ID (trait), and p.value"),
              tags$li("It is also possible to define the threshold, ylim, point size, number of columns in the plots (Ncols), and Y and X axis labs"),
              h4("How to cite ShinyGWASPheWAS"),
              tags$li("Insert reference here"),
              h4("Contact Information and suport:"),
              tags$li("Rafael Massahiro Yassue, rafael.yassue@gmail.com"),
              tags$li("Dr. Gota Morota, morota@vt.edu")
              
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
                 selectInput('posi', 'Posi', ""),
                 selectInput('pvalue', 'P value', "", selected = ""),
                 selectInput('chromosome', 'Chromosome', "", selected = ""),
                 checkboxInput('highlight', 'Threshold', TRUE),
                 selectInput('trait1', 'Factor 1', "", selected = ""),
                 selectInput('trait2', 'Factor 2', "", selected = ""),
                 numericInput("obs", "Threshold:", 1),
                 numericInput("ylim", "ylim:", 1),
                 numericInput("point1", "Point size:", 0.1),
                 textInput("xlab","Xlab"," "),
                 textInput("ylab","Ylab","p value"),
                 downloadButton("downloadData", "Download example data")
                 
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
                 selectInput('pvalue2', 'P value', "", selected = ""),
                 checkboxInput('highlight2', 'Threshold', TRUE),
                 numericInput("obs2", "Threshold:", 1),
                 numericInput("ylim2", "ylim:", 1),
                 numericInput("point2", "Point size:", 0.1),
                 numericInput("ncols", ("Ncols"),5),
                 textInput("xlab2","Xlab"," "),
                 textInput("ylab2","Ylab","p value"),
                 downloadButton("downloadData2", "Download example Phewas data")
                 
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
         width = 400*1.3,
         height = 300*1.3,
         alt = "fig01")
  }, deleteFile = F)
  
  output$myImage2 <- renderImage({
    list(src = "fig02.png",
         contentType = 'image/png',
         width = 400*1.3,
         height = 300*1.3,
         alt = "fig02")
  }, deleteFile = F)
  data <- reactive({ 
    req(input$file1) 
    inFile <- input$file1 
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'posi', label = 'Marker position',
                      choices = names(df), selected = names(df)[6])
    updateSelectInput(session, inputId = 'pvalue', label = 'p value',
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
    updateSelectInput(session, inputId = 'pvalue2', label = 'p value',
                      choices = names(df2), selected = names(df2)[3])
    updateSelectInput(session, inputId = 'marker_ID2', label = 'Marker_ID',
                      choices = names(df2), selected = names(df2)[2])
    return(df2)})
  # Tab 1
  output$MyPlot <- renderPlotly({
    dados <- data()[, c(input$posi, input$pvalue, input$chromosome,input$trait1, input$trait2, input$marker_ID)]
    colnames(dados)<-c("Index", "pvalue", "Chromosome", "trait1", "trait2", "Marker_ID")
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
      ylab(input$ylab2)+ xlab(input$xlab2)+ ylim(-0.25,input$ylim2)+
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
