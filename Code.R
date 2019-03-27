library(shiny)
library(DT)
library(gtools)
library(markdown)
library(tidyr)
library(igraph)
require(RCurl)
library(visNetwork)
library(reshape2)
library(ggplot2)
library(RISmed)
library(wordcloud)
library(tidytext)
library(topicmodels)
library(dplyr)


# Define UI for the app
ui <- navbarPage(
  #title = 'PgenePapers Tools:',
  title=div(a(img(src="https://cdn0.iconfinder.com/data/icons/infographic-element-4/32/32_connection_nodes_communication_network_seo_social_community_relation-512.png",
                  height = 30,
                  style = "margin:0px 0px; padding-bottom: 5px"),
              "PgenePapers Tools", href="")
  ),
  
  tabPanel('Pseudogene-Gene-Role Table', downloadButton(outputId = "download_filtered",
                                                        label = "Download Filtered Data"),
           DT::dataTableOutput('ex1')
           
  ),
  
  tabPanel('All Papers', downloadButton(outputId = "download_filtered1",
                                        label = "Download Filtered Data"),
           DT::dataTableOutput('ex2')),
  
  tabPanel('NLP Network', uiOutput('graph_presentation')),
  tabPanel("Gene-Pseudogene Correlation Table",
           #sidebarLayout(
             sidebarPanel(
          
             # Input: Selector for choosing dataset ----
             selectInput(inputId = "genedata",
                         label = "Select Cancer Type:",
                         choices = c("ACC","BLCA","BRCA", "CESC","CHOL","COAD","DLBC","ESCA","GBM","HNSC",
                                     "KICH","KIRC","KIRP","LGG","LIHC","LUAD","LUSC","MESO","OV","PCPG",
                                     "PRAD","READ","SARC","SKCM","STAD","TGCT","THCA","THYM",
                                     "UCEC","UCS","UVM")),
             
             br(),
             h2("Key to different terms:"),
             br(),
             h3("Coding Gene"),
             p("It refers to a protein coding gene."),
             br(),
             h3("Pseudogene"),
             p("It refers to a psedogene gene."),
             br(),
             h3("Literature Count"),
             p("It refers to the number of publications reporting a particular gene-pseudogene pair association in our NLP database (refer to Pseudogene-Gene-Role Table)."),
             p("A higher literature count means more evidence of the corresponding gene-pseudogene pair in the literature."),
             br(),
             h3("Correlation"),
             p("It is the Spearman's correlation coefficient between the expression values of the corresponding gene-pseudogene pair."),
             br(),
             h3("p-value"),
             p("It is the probability that is used in testing the null hypothesis that the correlation coefficient between the gene-pseudogene pair is 0."),
             p("A lower p-value means more evidence that the true correlation between the gene-pseudogene pair is significantly different from 0."),
             
             width = 3
             
             
           ),
           
           # Main panel for displaying outputs ----
           mainPanel(
             
            column(6, plotOutput('g1', height = 500)), column(6, verbatimTextOutput('g2')), 
            column(6, verbatimTextOutput('g3')), column(12, verbatimTextOutput('g5')),
            column(6, plotOutput('g4', height = 500)),
            DT::dataTableOutput("view2")
           )
           
  ),
  
  tabPanel("miRNA-Gene Correlation Table",
           #sidebarLayout(
           # Sidebar panel for inputs ----
           sidebarPanel(
             
             # Input: Selector for choosing dataset ----
             selectInput(inputId = "miRNAdata",
                         label = "Select Cancer Type:",
                         choices = c("ACC","BLCA","BRCA", "CESC","CHOL","COAD","DLBC","ESCA","HNSC",
                                     "KICH","KIRC","KIRP","LGG","LIHC","LUAD","LUSC","MESO","PCPG",
                                     "PRAD","READ","SARC","SKCM","STAD","TGCT","THCA","THYM",
                                     "UCEC","UCS","UVM")),
             
             br(),
             h2("Key to different terms:"),
             br(),
             h3("Gene"),
             p("It refers to a protein coding gene."),
             br(),
             h3("miRNA"),
             p("It refers to a miRNA."),
             br(),
             h3("Correlation"),
             p("It is the spearman correlation coefficient between the expression values of the corresponding gene-miRNA pair."),
             br(),
             h3("p-value"),
             p("It is the probability that used in testing the null hypothesis that the correlation coefficient between the gene-pseudogene pair is 0."),
             p("A lower p-value means more evidence that the true correlation between the gene-pseudogene pair is significantly different from 0."),
             h3("q-value"),
             p("It is the p-value taking into account the multiple corrections."),
             h3("Evidence"),
             p("The number of times a particular gene-miRNA pair has occurred in the predictions database."),
             
             width = 3.0
             
           ),
           
           # Main panel for displaying outputs ----
           mainPanel(
             
             column(6, plotOutput('r1', height = 500)), column(6, verbatimTextOutput('r2')), 
             DT::dataTableOutput("view1")
           )
           
  ),
  
  
  tabPanel('Gene-Drug Interactions',
           #sidebarLayout(
           # Sidebar panel for inputs ----
           sidebarPanel( 
             
             
             
             # Input: Selector for choosing dataset ----
             selectInput(inputId = "dataset",
                         label = "Select Cancer Type:",
                         choices = c("ACC","BLCA","BRCA", "CESC","CHOL","COAD","DLBC","ESCA","HNSC","GBM",
                                     "KICH","KIRC","KIRP","LGG","LIHC","LUAD","LUSC","MESO","OV","PCPG",
                                     "PRAD","READ","SARC","SKCM","STAD","TGCT","THCA","THYM",
                                     "UCEC","UCS","UVM")), 
             
             
             
             br(),
             h2("Key to different terms:"),
             br(),
             h3("Gene"),
             p("It refers to a protein coding gene."),
             br(),
             h3("Drug"),
             p("It refers to the drug that targets a specific gene."),
             br(),
             h3("Interaction Type"),
             p("It refers to the type of interaction of the drug with the gene."),
             br(),
             h1("All the drugs in this database are FDA approved."),
             
             width = 2.0
             
           ),
           
           # Main panel for displaying outputs ----
           mainPanel(
             
             DT::dataTableOutput("view")
           )
           
  ),
  
  tabPanel('Gene-Pseudogene-miRNA-Drug Network', 
  
               
           
  fluidPage(
    titlePanel("Select the cancer type:"),
    
    fluidRow(
      column(4,selectInput("cancertype",
                           "Cancer Type:",
                           c("ACC","BLCA","BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA",
                             "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LGG", "LIHC", "LUAD",
                             "LUSC", "MESO","PCPG", "PRAD", "READ", "SARC", "SKCM",
                             "STAD", "TGCT", "THCA", "THYM", "UCEC", "UCS", "UVM")))
    )),
    
    fluidPage(
      titlePanel("Select the parameters:"),
      
      fluidRow(
        column(2,
               selectInput("pseudogene1",
                           "Pseudo Gene:",
                           c("All"))
        ),
        
        column(2,
               selectInput("gene1",
                           "Coding Gene:",
                           c("All"))
                             
        ),
        
        
        column(2,
               selectInput("miRNA1",
                           "miRNA:",
                           c("None"))
        ),
        
        column(2,
               selectInput("Drug",
                           "Drug:",
                           c("None"))
        )
      )),
       
  #actionButton("reset_input", "RESET INPUT"), width = 2.0, position = c("right"),
      
    
  uiOutput('graph_presentation1')),
  
  
  tabPanel('Latest Papers', downloadButton(outputId = "download_filtered2",
                                        label = "Download Filtered Data"),
           DT::dataTableOutput('ex3')),

  
  tabPanel('Readme', uiOutput('readme')),
  
  tabPanel('FAQ', uiOutput('readme1')),


  tabPanel("About", h2("About Us", style="color: STEELBLUE; padding-bottom: 20px"),
           h4("The Yan Zhang Lab at OSUMC studies statistical and computational methods and their applications to genomic and proteomic research, such as (1) functional and evolutionary impact of structural variations (such as insertions, deletions and retroduplications) in both normal and abnormal populations; (2) association and eQTL analysis of structural variations; (3) integrative analysis of genomic and proteomic data; (4) statistical modeling of biological networks, integrating data of multiple levels."),
           tags$div(
             tags$img(src='https://ccme.osu.edu/Images/OSULogoEmail.jpg',
                      height="125",
                      alt="OSUMC", class="center", style="padding: 30px"),
             style="text-align: center; padding: 20px"
           ),
           h3("Development Team", style="color: STEELBLUE"),
           tags$ul(
             tags$li("Achal Awasthi"),
             tags$li("Yan Zhang")
           ),
           h3("Prof. Yan Zhang's Laboratory", style="color: STEELBLUE"),
           tags$ul(
             tags$li("Yan Zhang"),
             tags$li("Achal Awasthi")
           )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  
  
  # Return the requested dataset ----
  drug_data <- reactive({
    switch(input$dataset,
           "ACC" = read.csv("ACC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "BLCA" = read.csv("BLCA_Final_Drug.csv", header = T,stringsAsFactors = F),
           "BRCA" = read.csv("BRCA_Final_Drug.csv", header = T,stringsAsFactors = F),
           "CESC" = read.csv("CESC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "CHOL" = read.csv("CHOL_Final_Drug.csv", header = T,stringsAsFactors = F),
           "COAD" = read.csv("COAD_Final_Drug.csv", header = T,stringsAsFactors = F),
           "DLBC" = read.csv("DLBC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "ESCA" = read.csv("ESCA_Final_Drug.csv", header = T,stringsAsFactors = F),
           "HNSC" = read.csv("HNSC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "GBM" = read.csv("GBM_Final_Drug.csv", header = T,stringsAsFactors = F),
           "KICH" = read.csv("KICH_Final_Drug.csv", header = T,stringsAsFactors = F),
           "KIRC" = read.csv("KIRC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "KIRP" = read.csv("KIRP_Final_Drug.csv", header = T,stringsAsFactors = F),
           "LGG" = read.csv("LGG_Final_Drug.csv", header = T,stringsAsFactors = F),
           "LIHC" = read.csv("LIHC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "LUAD" = read.csv("LUAD_Final_Drug.csv", header = T,stringsAsFactors = F),
           "LUSC" = read.csv("LUSC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "MESO" = read.csv("MESO_Final_Drug.csv", header = T,stringsAsFactors = F),
           "PCPG" = read.csv("PCPG_Final_Drug.csv", header = T,stringsAsFactors = F),
           "PRAD" = read.csv("PRAD_Final_Drug.csv", header = T,stringsAsFactors = F),
           "READ" = read.csv("READ_Final_Drug.csv", header = T,stringsAsFactors = F),
           "SARC" = read.csv("SARC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "SKCM" = read.csv("SKCM_Final_Drug.csv", header = T,stringsAsFactors = F),
           "STAD" = read.csv("STAD_Final_Drug.csv", header = T,stringsAsFactors = F),
           "TGCT" = read.csv("TGCT_Final_Drug.csv", header = T,stringsAsFactors = F),
           "THCA" = read.csv("THCA_Final_Drug.csv", header = T,stringsAsFactors = F),
           "THYM" = read.csv("THYM_Final_Drug.csv", header = T,stringsAsFactors = F),
           "UCEC" = read.csv("UCEC_Final_Drug.csv", header = T,stringsAsFactors = F),
           "UCS" = read.csv("UCS_Final_Drug.csv", header = T,stringsAsFactors = F),
           "UVM" = read.csv("UVM_Final_Drug.csv", header = T,stringsAsFactors = F))
  })
  
  output$view <- DT::renderDataTable({
    DT::datatable(drug_data(), options = list(searchHighlight = TRUE, pageLength = 25))
  })
  
##############################################################################################
  # Download and format data
  dat <- read.csv("ABC.csv",header = T,stringsAsFactors = FALSE)
  colnames(dat) = c("Coding Gene/Disease","Pseudogene", "PubMed ID", "Role")
  
  ref <- read.csv("ABC1.csv",header = T,stringsAsFactors = FALSE)
  colnames(ref) = c("Coding Gene/Disease","Pseudogene", "PubMed ID","Disease/Cancer Type", "Tissue Type" ,"Abstract")
  ref[,sapply(ref,is.character)] <- sapply(
    ref[,sapply(ref,is.character)],
    iconv,"WINDOWS-1252","UTF-8")

  
  # ref <- read.delim("ABC1.txt", 
  #                   header = TRUE, sep = "|", check.names = FALSE, stringsAsFactors = FALSE)
  # ref.2 <- apply(ref, 2, function(x) gsub("^\\s+", "", x))
  # ref.3 <- apply(ref.2, 2, function(x) gsub("\\s+$", "", x))
  # ref <- data.frame(ref.3, stringsAsFactors = FALSE)
  # names(ref) <- c("Pseudogene Name", "Coding Gene Name", "PMID", "Abstract")
  # rm(ref.2)
  # rm(ref.3)
  
  ## Gene-Pseudogene Correlation Datasets
  
  ACC = read.csv("Final_ACC.csv",header = T)
  colnames(ACC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  BLCA = read.csv("Final_BLCA.csv",header = T)
  colnames(BLCA) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  BRCA = read.csv("Final_BRCA.csv",header = T)
  colnames(BRCA) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  CESC = read.csv("Final_CESC.csv",header = T)
  colnames(CESC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  CHOL = read.csv("Final_CHOL.csv",header = T) 
  colnames(CHOL) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  COAD = read.csv("Final_COAD.csv",header = T)
  colnames(COAD) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  DLBC = read.csv("Final_DLBC.csv",header = T)
  colnames(DLBC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  ESCA = read.csv("Final_ESCA.csv",header = T)
  colnames(ESCA) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  GBM = read.csv("Final_GBM.csv",header = T)
  colnames(GBM) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  HNSC = read.csv("Final_HNSC.csv",header = T)
  colnames(HNSC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  KICH = read.csv("Final_KICH.csv",header = T)
  colnames(KICH) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  KIRC = read.csv("Final_KIRC.csv",header = T)
  colnames(KIRC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  KIRP = read.csv("Final_KIRP.csv",header = T)
  colnames(KIRP) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  LGG = read.csv("Final_LGG.csv",header = T)
  colnames(LGG) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  LIHC = read.csv("Final_LIHC.csv",header = T)
  colnames(LIHC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  LUAD = read.csv("Final_LUAD.csv",header = T)
  colnames(LUAD) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  LUSC = read.csv("Final_LUSC.csv",header = T)
  colnames(LUSC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  MESO = read.csv("Final_MESO.csv",header = T)
  colnames(MESO) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  OV = read.csv("Final_OV.csv",header = T)
  colnames(OV) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  PCPG = read.csv("Final_PCPG.csv",header = T)
  colnames(PCPG) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  PRAD = read.csv("Final_PRAD.csv",header = T)
  colnames(PRAD) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  READ = read.csv("Final_READ.csv",header = T)
  colnames(READ) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  SARC = read.csv("Final_SARC.csv",header = T)
  colnames(SARC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  SKCM = read.csv("Final_SKCM.csv",header = T)
  colnames(SKCM) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  STAD = read.csv("Final_STAD.csv",header = T)
  colnames(STAD) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  TGCT = read.csv("Final_TGCT.csv",header = T)
  colnames(TGCT) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  THCA = read.csv("Final_THCA.csv",header = T)
  colnames(THCA) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  THYM = read.csv("Final_THYM.csv",header = T)
  colnames(THYM) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  UCEC = read.csv("Final_UCEC.csv",header = T)
  colnames(UCEC) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  UCS = read.csv("Final_UCS.csv",header = T)
  colnames(UCS) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  UVM = read.csv("Final_UVM.csv",header = T)
  colnames(UVM) = c("Coding Gene", "Pseudogene", "Literature Count", "Corr", "p-value")
  
  ## miRNA-Gene/Pseudogene Datasets
  
  ACCmiRNA = read.csv("miRNA_ACC.csv",header = T)
  BLCAmiRNA = read.csv("miRNA_BLCA.csv",header = T)
  BRCAmiRNA = read.csv("miRNA_BRCA.csv",header = T) 
  CESCmiRNA = read.csv("miRNA_CESC.csv",header = T)
  CHOLmiRNA = read.csv("miRNA_CHOL.csv",header = T) 
  COADmiRNA = read.csv("miRNA_COAD.csv",header = T)
  DLBCmiRNA = read.csv("miRNA_DLBC.csv",header = T)
  ESCAmiRNA = read.csv("miRNA_ESCA.csv",header = T)
  #GBMmiRNA = read.csv("miRNA_GBM.csv",header = T)
  HNSCmiRNA = read.csv("miRNA_HNSC.csv",header = T)
  KICHmiRNA = read.csv("miRNA_KICH.csv",header = T)
  KIRCmiRNA = read.csv("miRNA_KIRC.csv",header = T)
  KIRPmiRNA = read.csv("miRNA_KIRP.csv",header = T)
  LGGmiRNA = read.csv("miRNA_LGG.csv",header = T)
  LIHCmiRNA = read.csv("miRNA_LIHC.csv",header = T)
  LUADmiRNA = read.csv("miRNA_LUAD.csv",header = T)
  LUSCmiRNA = read.csv("miRNA_LUSC.csv",header = T)
  MESOmiRNA = read.csv("miRNA_MESO.csv",header = T)
  #OVmiRNA = read.csv("miRNA_OV.csv",header = T)
  PCPGmiRNA = read.csv("miRNA_PCPG.csv",header = T)
  PRADmiRNA = read.csv("miRNA_PRAD.csv",header = T)
  READmiRNA = read.csv("miRNA_READ.csv",header = T)
  SARCmiRNA = read.csv("miRNA_SARC.csv",header = T)
  SKCMmiRNA = read.csv("miRNA_SKCM.csv",header = T)
  STADmiRNA = read.csv("miRNA_STAD.csv",header = T)
  TGCTmiRNA = read.csv("miRNA_TGCT.csv",header = T)
  THCAmiRNA = read.csv("miRNA_THCA.csv",header = T)
  THYMmiRNA = read.csv("miRNA_THYM.csv",header = T)
  UCECmiRNA = read.csv("miRNA_UCEC.csv",header = T)
  UCSmiRNA = read.csv("miRNA_UCS.csv",header = T)
  UVMmiRNA = read.csv("miRNA_UVM.csv",header = T)
  
##########################################################################################  

##########################################################################################  
  # Tab1
  output$ex1 <- DT::renderDataTable(
    DT::datatable(dat, options = list(searchHighlight = TRUE, pageLength = 25))
  )

  output$download_filtered <- 
    downloadHandler(
      filename = "Filtered Data.csv",
      content = function(file){
        write.csv(dat[input[["ex1_rows_all"]], ],
                  file)
      }
    )
  
  # Tab2
  output$ex2 <- DT::renderDataTable(
    DT::datatable(ref, options = list(searchHighlight = TRUE, pageLength = 25))
  )
  
  output$download_filtered1 <- 
    downloadHandler(
      filename = "Filtered Data.csv",
      content = function(file){
        write.csv(ref[input[["ex2_rows_all"]], ],
                  file)
      }
    )
  
  ## Tab 3
  # Return the requested dataset of gene=pseudogene pairs -
  gene_data <- reactive({
    switch(input$genedata,
           "ACC" = ACC,
           "BLCA" = BLCA,
           "BRCA" = BRCA,
           "CESC" = CESC,
           "CHOL" = CHOL,
           "COAD" = COAD,
           "DLBC" = DLBC,
           "ESCA" = ESCA,
           "GBM" = GBM,
           "HNSC" = HNSC,
           "KICH" = KICH,
           "KIRC" = KIRC,
           "KIRP" = KIRP,
           "LGG" = LGG,
           "LIHC" = LIHC,
           "LUAD" = LUAD,
           "LUSC" = LUSC,
           "MESO" = MESO,
           "OV" = OV,
           "PCPG" = PCPG,
           "PRAD" = PRAD,
           "READ" = READ,
           "SARC" = SARC,
           "SKCM" = SKCM,
           "STAD" = STAD,
           "TGCT" = TGCT,
           "THCA" = THCA,
           "THYM" = THYM,
           "UCEC" = UCEC,
           "UCS" = UCS,
           "UVM" = UVM
    )
  })
  
  output$view2 <- DT::renderDataTable({
    DT::datatable(gene_data(), filter = 'top', options = list(#columnDefs = list(list(targets = c(1,2), searchable = FALSE)),
                                                              searchHighlight = TRUE,pageLength = 15))
  })
  
  # Histogram of Correlations 
  output$g1 = renderPlot({
    hist(gene_data()$Corr,15,F, main="Distribution of correlations of all gene-pseudogene pairs",
         xlab = "Correlation", ylab="Density",col = c("blue"))
    lines(density(gene_data()$Corr),lwd = 2.5,col = c("red")) 
  })
  
  # Print highly correlated genes
  output$g2 = renderPrint({
    s = gene_data()[gene_data()$Corr > 0.7,]
    s = s[order(-s$Corr),]
    if (nrow(s) > 0) {
      cat('Strongly (positive) Correlated Gene-Pseudogene Pairs\n\n')
      print(s[1:4])
    }
    else{cat('Strongly (positive) Correlated Gene-Pseudogene Pairs\n\n')
      print("There are no gene-pseudogene pairs")
    }
  })
  
  # Print highly correlated genes
  output$g3 = renderPrint({
    s1 = gene_data()[gene_data()$Corr < -0.7,]
    s1 = s1[order(-s1$Corr),]
    if (nrow(s1) > 0) {
      cat('Strongly (negative) Correlated Gene-Pseudogene Pairs\n\n')
      print(s1[1:4])
    }
    else{cat('Strongly (negative) Correlated Gene-Pseudogene Pairs\n\n')
      print("There are no gene-pseudogene pairs.")
      }
  })
  
  # Heatmap of searchable gene-pseudogene pairs
  
  output$g4 = renderPlot({
    s2 = gene_data()[input[["view2_rows_all"]], ]
    s2 = s2[, -c(3,5)]
    colnames(s2) = c("Coding_Gene","Pseudo_Gene","Corr")
    if (nrow(s2)<25){
    #s3 = tidyr::spread(s2[, -c(3,5)], "Coding Gene", Corr, fill = 0)
    #s4 = as.matrix(s3[, -1])
    
      ggheatmap <- ggplot(s2, aes(Coding_Gene, Pseudo_Gene, fill = Corr))+
        geom_tile(color = "white")+
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Spearman\nCorrelation") +
        theme_minimal()+ # minimal theme
        theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1))+
        coord_fixed()
    
      print(ggheatmap)
      ggheatmap + 
        geom_text(aes(Coding_Gene, Pseudo_Gene, label = Corr), color = "black", size = 4) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1, 0),
          legend.position = c(0.8, 1.0),
          legend.direction = "horizontal")+
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1.5,
                                     title.position = "top", title.hjust = 0.5))
    }
    else{
      output$g5 <- renderPrint("Please enter a gene/pseudogene name in the search box to get the corresponding heatmap.")
      }
  })
  
  
#########################################################################################
  
  ## Tab 4
  
  # Return the requested dataset of miRNA-Gene pairs-
  miRNA_data <- reactive({
    switch(input$miRNAdata,
           "ACC" = ACCmiRNA,
           "BLCA" = BLCAmiRNA,
           "BRCA" = BRCAmiRNA,
           "CESC" = CESCmiRNA,
           "CHOL" = CHOLmiRNA,
           "COAD" = COADmiRNA,
           "DLBC" = DLBCmiRNA,
           "ESCA" = ESCAmiRNA,
           #"GBM" = GBMmiRNA,
           "HNSC" = HNSCmiRNA,
           "KICH" = KICHmiRNA,
           "KIRC" = KIRCmiRNA,
           "KIRP" = KIRPmiRNA,
           "LGG" = LGGmiRNA,
           "LIHC" = LIHCmiRNA,
           "LUAD" = LUADmiRNA,
           "LUSC" = LUSCmiRNA,
           "MESO" = MESOmiRNA,
           #"OV"   = OVmiRNA,
           "PCPG" = PCPGmiRNA,
           "PRAD" = PRADmiRNA,
           "READ" = READmiRNA,
           "SARC" = SARCmiRNA,
           "SKCM" = SKCMmiRNA,
           "STAD" = STADmiRNA,
           "TGCT" = TGCTmiRNA,
           "THCA" = THCAmiRNA,
           "THYM" = THYMmiRNA,
           "UCEC" = UCECmiRNA,
           "UCS" = UCSmiRNA,
           "UVM" = UVMmiRNA)
  })
  
  # Show the first "n" observations ----
  output$view1 <- DT::renderDataTable({
    DT::datatable(miRNA_data(), filter = 'top', options = list(#columnDefs = list(list(targets = c(1,2), searchable = FALSE)),
                                                               searchHighlight = TRUE, pageLength = 15))
  })
  
  # Draw Histogram of correlation values
  output$r1 = renderPlot({
    hist(miRNA_data()$Corr,15,F, main="Distribution of correlations of all gene-miRNA pairs",
         xlab = "Correlation", ylab="Density",col = c("blue"))
    lines(density(miRNA_data()$Corr),lwd = 2.5,col = c("red"))
  })
  
  # Print highly correlated genes
  output$r2 = renderPrint({
    r = miRNA_data()[miRNA_data()$Corr < -0.6 & miRNA_data()$Evidence > 2,]
    r = r[order(r$Corr),]
    if (nrow(r) > 0) {
      cat('Highly (negative) Correlated miRNA-Gene Pairs\n\n')
      print(r[, -c(4,5)])
    }
    else{cat('Highly (negative) Correlated miRNA-Gene Pairs\n\n')
      print("There are no miRNA-Gene pairs")
    }
  })
  
  ##############################################################################################  
  # Tab5 (NLP Network)
  output$graph_presentation <- renderUI({  
    fluidPage(
      titlePanel("Select one/all pseudogenes or genes:"),
       h4("Takes time to load. Please be patient"),
      
      fluidRow( 
        column(4,
               selectInput("pseudogene", 
                           "Pseudogene:",
                           c("All",
                             sort(unique(as.character(dat$"Pseudo Gene")))))
        ),
        column(4,
               selectInput("gene",
                           "Coding Gene:",
                           c("All",
                             sort(unique(as.character(dat$"Coding Gene/Disease")))))
        )
      ),
      
      # Creat a button to save network data
      fluidRow(
        column(12, "Click the button to", downloadButton("downloadData", "Download filtered data"), 
               ". You can also right click on your mouse and print the graph into a PDF file. ")
      ),
      
      # Creat a row to add instructions
      fluidRow(
        column(12, "Scroll your mouse (up and down, side to side) to view the graph. You can also pick and drag a node in the graph. The connected subnetwork will be highlighted.")
      ),
      
      # Create a new row for the graph
      fluidRow(
        column(12, align = 'left',
               visNetworkOutput('vis', width = "100%", height = "850px"))
      )
    )
  })
  
  output$vis <- renderVisNetwork({

    data <- dat
    if (input$pseudogene != "All") {
      data <- data[data$"Pseudo Gene" == input$pseudogene,]
    }
    if (input$gene != "All") {
      data <- data[data$"Coding Gene/Disease" == input$gene,]
    }
    
    if (nrow(data) > 0) {
      graph1 <- graph.data.frame(data, directed=F)
      E(graph1)$weight <- 1
      #graph <- simplify(graph, edge.attr.comb=list(weight = "Corr", transaction_amount = "Corr", function(x)length(x)))
      networks1 <- clusters(as.undirected(graph1))
      V(graph1)$network <- networks1$membership
      nodes1 <- get.data.frame(graph1, what="vertices")
      nodes1 <- data.frame(id = nodes1$name, title = nodes1$name, group = nodes1$network, stringsAsFactors = F)
      nodes1 <- nodes1[order(nodes1$id, decreasing = F),]
      edges1 <- get.data.frame(graph1, what="edges")[1:3]
      #colnames(edges1) = c("from","to","label")
    
     
      visNetwork(nodes1, edges1, main = "Gene-Pseudogene NLP Network") %>%
        visPhysics(solver = "repulsion") %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
        visGroups(groupname = "1", color = "maroon")
    }
      #withProgress(message = 'Drawing the network',
       #            detail = 'This may take a while...', value = 0, {
        #             for (i in 1:300) {
         #              incProgress(1/300)
          #             Sys.sleep(0.2)
           #          }
            #       })
  })
  
  # Download csv of filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$pseudogene, "_", input$gene, ".csv", sep = "")
    },
    content = function(file) {
      data <- dat
      if (input$pseudogene != "All") {
        data <- data[data$"Pseudo Gene" == input$pseudogene,]
      }
      if (input$gene != "All") {
        data <- data[data$"Coding Gene/Disease" == input$gene,]
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  ###########################################################################################
  ## Defining datasets for correlation network
  
  miRNA_data1 <- reactive({read.csv(paste("miRNA_",as.character(input$cancertype),".csv",sep=""),header = T)})
  drug_data1 <- reactive({read.csv(paste(as.character(input$cancertype),"_Final_Drug.csv",sep=""), header = T,stringsAsFactors = F)})
  
  ###########################################################################################  
  # Tab6
  
  
  library(gtools)
  
  output$graph_presentation1 <- renderUI({  
    
      fluidPage(
      #   titlePanel("Select the parameters:"),
      #   
      #   fluidRow(
      #     column(2,
      #            selectInput("pseudogene1",
      #                        "Pseudo Gene:",
      #                        c("All",
      #                          sort(unique(as.character(gene_data()$"Pseudo Gene")))))
      #     ),
      #     
      #     column(2,
      #            selectInput("gene1",
      #                        "Coding Gene:",
      #                        c("All",
      #                          sort(unique(as.character(gene_data()$"Coding Gene")))))
      #     ),
      #     
      #     
      #     column(2,
      #            selectInput("miRNA1",
      #                        "miRNA:",
      #                        c("None",sort(unique(as.character(miRNA_data1()$"miRNA")))))
      #     ),
      #     
      #     column(2,
      #            selectInput("Drug",
      #                        "Drug:",
      #                        c("None",sort(unique(as.character(drug_data1()$"drug")))))
      #     )
      #   ),
        
        # Creat a button to save network data
        fluidRow(
          column(12, "Click the button to", downloadButton("downloadData1", "Download filtered data"),
                 ". You can also right click on your mouse and print the graph into a PDF file. ")
        ),
        
        # 
        # # Creat a button to reset network data
        # fluidRow(
        #   column(4, 
        #          actionButton("reset_input", "RESET INPUT"),pos="Right")
        #   ),
        
        # Creat a row to add instructions
        fluidRow(
          column(12,
                 verbatimTextOutput("console"))
        ),
        
        # Create a new row for the graph
        fluidRow(
          column(12, align = 'left',
                 visNetworkOutput('vis1', width = "100%", height = "850px"))
        )
      )
  })
  
 
  
  
  output$vis1 <- renderVisNetwork({
    
    data1 = read.csv(paste("Final_",as.character(input$cancertype),".csv",sep=""),header = T)
    colnames(data1) = c("Coding Gene", "Pseudo Gene", "Literature Count", "Corr", "p-value")
    
    data2 = read.csv(paste("miRNA_",as.character(input$cancertype),".csv",sep=""),header = T)
    
    data3 = read.csv(paste(as.character(input$cancertype),"_Final_Drug.csv",sep=""), header = T,stringsAsFactors = F)
    
  
###############################################################################################
    
################################################################################################    
## Adding tags to nodes 
    
    if (input$pseudogene1 != "All" & input$pseudogene1 %in% data1$"Pseudo Gene"){
      data1 <- data1[data1$"Pseudo Gene" == input$pseudogene1,]
      data1$"Pseudo Gene" = paste(data1$"Pseudo Gene","(PG)",sep=" ")
      data1$"Coding Gene" = paste(data1$"Coding Gene","(G)",sep=" ")
    }
    
    if (input$gene1 != "All" & input$gene1 %in% data1$"Coding Gene") {
      data1 <- data1[data1$"Coding Gene" == input$gene1,]
      data1$"Coding Gene" = paste(data1$"Coding Gene","(G)",sep=" ")
      data1$"Pseudo Gene" = paste(data1$"Pseudo Gene","(PG)",sep=" ")
    }
    
##############################################################################################        
    data2 <- data2[data2$"miRNA" == input$miRNA1,]
    if (input$miRNA1 != "None" & input$miRNA1 %in% data2$"miRNA") { 
      data2$"miRNA" <- paste(data2$"miRNA","(R)",sep=" ")
      data2$"Gene" <- paste(data2$"Gene","(G)",sep=" ")
    }

    data3 <- data3[data3$"drug" == input$Drug,]
    if (input$Drug != "None" & input$Drug %in% data3$drug) { 
      data3$"drug" <- paste(data3$"drug","(D)",sep=" ")
      data3$"gene" <- paste(data3$"gene","(G)",sep=" ")
    }
    
    
###############################################################################################     
    colnames(data2) = c("Pseudo Gene", "Coding Gene", "Corr", "p-value", "q-value", "Evidence")
    colnames(data3) = c("Coding Gene", "Pseudo Gene", "Interaction Type", "Sources", "PMIDs")
    
    data4 <- data.frame(smartbind(data1, data2, data3))
    
## Download Button
    
    ## Renaming the columns
    temp_data <- data4
    colnames(temp_data)[2] <- "Pseudogene/miRNA/Drug"

## Downloadable csv of selected dataset ----
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste(input$cancertype,"_",input$gene1,"_",input$pseudogene1,".csv", sep = "")
      },
      content = function(file) {
        write.csv(temp_data, file, row.names = FALSE)
      }
    )
###########################################################################################   
      if (input$pseudogene1 == "All"){
      updateSelectInput(session,"pseudogene1", "Pseudo Gene:",choices =  c("All", sort(unique(as.character(data1$"Pseudo Gene")))),selected = NULL)
      }
    
      if (input$gene1 == "All"){
      updateSelectInput(session,"gene1", "Coding Gene:",choices =  c("All", sort(unique(as.character(data1$"Coding Gene")))),selected = NULL)
      }
    
      if (input$miRNA1 == "None"){
      updateSelectInput(session,"miRNA1", "miRNA", choices = c("None","All",sort(unique(as.character(miRNA_data1()$"miRNA")))),selected = NULL)
      }
    
      if (input$Drug == "None"){
      updateSelectInput(session,"Drug", choices = c("None","All",sort(unique(as.character(drug_data1()$"drug")))),selected=NULL)
      }
    
    
###########################################################################################      
    # observeEvent(input$reset_input, {
    #   
    #   if (input$cancertype!= "ACC"){
    #    updateSelectInput(session,"cancertype", "Cancer Type:", choices =
    #                      c("ACC","BLCA","BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA",
    #                        "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LGG", "LIHC", "LUAD",
    #                        "LUSC", "MESO","PCPG", "PRAD", "READ", "SARC", "SKCM",
    #                        "STAD", "TGCT", "THCA", "THYM", "UCEC", "UCS", "UVM"),selected=NULL)
    #    updateSelectInput(session,"pseudogene1", "Pseudo Gene:", choices =  c("All", sort(unique(as.character(gene_data()$"Pseudo Gene")))), selected = NULL)
    #    updateSelectInput(session,"gene1", "Coding Gene:",choices =  c("All", sort(unique(as.character(gene_data()$"Coding Gene")))),selected = NULL)
    #    updateSelectInput(session,"miRNA1", "miRNA", choices = c("None",sort(unique(as.character(miRNA_data1()$"miRNA")))),selected = NULL)
    #    updateSelectInput(session,"Drug", choices = c("None",sort(unique(as.character(drug_data1()$"drug")))),selected=NULL)
    #   }
    #   else{
    #     updateSelectInput(session,"pseudogene1", "Pseudo Gene:", choices =  c("All", sort(unique(as.character(gene_data()$"Pseudo Gene")))), selected = NULL)
    #     updateSelectInput(session,"gene1", "Coding Gene:",choices =  c("All", sort(unique(as.character(gene_data()$"Coding Gene")))),selected = NULL)
    #     updateSelectInput(session,"miRNA1", "miRNA", choices = c("None",sort(unique(as.character(miRNA_data1()$"miRNA")))),selected = NULL)
    #     updateSelectInput(session,"Drug", choices = c("None",sort(unique(as.character(drug_data1()$"drug")))),selected=NULL)
    #   }
    # })
    
    
###########################################################################################
      if (nrow(data4)>0) {
        graph <- graph.data.frame(data4, directed=F)
        E(graph)$weight <- 1
      #graph <- simplify(graph, edge.attr.comb=list(weight = "Corr", transaction_amount = "Corr", function(x)length(x)))
        networks <- clusters(as.undirected(graph))
        V(graph)$network <- networks$membership
        nodes <- get.data.frame(graph, what="vertices")
        nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$network,stringsAsFactors = F)
        nodes <- nodes[order(nodes$id, decreasing = F),]
      #print(length(nodes))
        edges <- get.data.frame(graph, what="edges")[1:4]
        #print(edges)
        colnames(edges) = c("from","to","LC","label")
        edges$width <- 1+(abs(edges$label)*5)
      
        lNodes = data.frame(label = c("(G = Gene)","(PG = Pseudogene)","(R = miRNA)","(D = Drug)"),
                          shape = c("circle"), color = c("lightblue"), title = "Legend")
    
        visNetwork(nodes, edges, main = "Gene-Pseudogene-miRNA-Drug Network") %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
        #visOptions(manipulation = TRUE)%>%
          visGroups(groupname = "1", color = "maroon")%>%
          visLegend(useGroups = FALSE, addNodes = lNodes, zoom = TRUE,
                  width = 0.3, position = "left")#, main = "Group")
      
        
        #else{
          #reactive({renderText("PLEASE CHANGE THE SELECTION")})
         # stop(safeError("PLEASE CHANGE THE SELECTION"))
        #}
        
      }
    

  })
  
  
##########################################################################################
#   Tab7
   res1 <- EUtilsSummary("Pseudogene", 
                         type = "esearch", 
                         db = "pubmed",
                         datetype = "pdat",
                         retmax = 500,
                         mindate = 2017, 
                         maxdate = 2019)
   fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")
  # 
   abstracts <- data.frame(Title = fetch@ArticleTitle,
                           Abstract = fetch@AbstractText, 
                           Journal = fetch@Title,
                           PMID = fetch@PMID, 
                           Year = fetch@YearPubmed)
   
  ### ensure abstracts are character fields (not factors)
   abstracts <- abstracts %>% mutate(Abstract = as.character(Abstract))
   
   output$ex3 <- DT::renderDataTable(
     DT::datatable(abstracts, options = list(searchHighlight = TRUE, pageLength = 25))
   )
   
   output$download_filtered <- 
     downloadHandler(
       filename = "Filtered Data.csv",
       content = function(file){
         write.csv(dat[input[["ex3_rows_all"]], ],
                   file)
       }
     )
 
##########################################################################################   
 
########################################################################################## 
  
  # Tab8
  output$readme <- renderUI({  
    fluidRow(
      column(width = 8, offset = 1,
             includeMarkdown("README.md")
      )
    )
  })
  

########################################################################################## 
  ## Tab9
 
  output$readme1 <- renderUI({  
    fluidRow(
      column(width = 8, offset = 1,
             includeMarkdown("FAQ.md")
      )
    )
  })
}


# Create Shiny app
shinyApp(ui = ui, server = server)


