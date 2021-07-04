
# Allow files up to 10 Mb
options(shiny.maxRequestSize=10*1024^2)

## volcano plots
require("tidyverse")
require("ggplot2") #Best plots
require("ggrepel") #Avoid overlapping labels
require("shiny")
require("shinyBS")
require("gridExtra")
require("DT")
require("plyr")
require("dplyr")
require("reshape2")

test_fun <- function()
{
  for (i in 1:15) {
    incProgress(1/15)
    sum(runif(1000000,0,1))
  }
}

test_fun2 <- function()
{
  for (i in 1:15) {
    incProgress(1/15)
    sum(runif(2000000,0,1))
  }
}

error_message_1 <- c("No data found"," ","types of errors","    (1) Symbol doesn't match; in test-data MAR10 has been converted to Mar-10 in excel","    (2) Not using a human database","    (3) Not a characterised protein","    (4) Was added to the database after June 2021")

ID.conversion <- read.csv("ID/uniprot.d.anno.210611.csv",row.names = 1)
names(ID.conversion) <- c("Ensembl","Uniprot_human","UNIPROT","Chrom","Gene.Name","Biotype")

filtered_table <- c("upregulated" = "upregulated",
                    "downregulated" ="downregulated",
                    "all significant values" = "all_significant",
                    "own list" = "own list")

sort_by <- c("x-axis" = 2,
             "y-axis" =4)
fonts <- c("Arial" = "sans", 
           "Times New Roman" = "serif", 
           "Courier" = "mono")

selected_present <- c("no labels","range (both directions)","range (up direction)","range (down direction)","own list")
y_options <- c("-Log10(p-value)","FDR", "adjusted")
legend_location <- c("right","bottom","left","top","none")
species <- c("BOVIN","CHICK","ECOLI","HORSE","HUMAN","MAIZE","MOUSE","PEA", "PIG","RABIT","RAT","SHEEP","SOYBN","TOBAC","WHEAT","YEAST")
lab <- c("significant up","significant down","non-significant")
names.conversion <- names(ID.conversion)[c(1:3,5)]
name.conversion.required <- c(FALSE,TRUE) 
label3 <- c(TRUE,FALSE)
direction <- c("all","up","down","same","opposite")
sort_direction <- c(TRUE,FALSE)

error_message_val1 <- "No data found"
error_message_val2 <- "Suggest uploading file\nheaders=ID, logFC, Pvalue"
error_message_val3 <- "No data found\n \nSuggest uploading file\nheaders=ID, logFC, Pvalue"
# user interface  ----


ui <- navbarPage("ggVolcanoR",
                 # Volcano plot ----
                 
                 tabPanel("Volano plot",
                          sidebarLayout(
                            sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 700px; position:relative;", width=4,
                                         tags$head(tags$style(HTML(".shiny-notification {position:fixed;top: 50%;left: 30%;right: 30%;}"))),
                                         tags$head(tags$style(HTML('.progress-bar {background-color: blue;}'))),
                                         selectInput("dataset", "Choose a dataset:", choices = c("test-data", "own")),
                                         fileInput('file1', 'ID, logFC, Pvalue',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         fluidRow(
                                           column(6,radioButtons('sep', 'Separator', c( Tab='\t', Comma=','), ',')),
                                           column(6,radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'))
                                         ),
                                         
                                         tags$hr(),
                                         h4("Type of graph"),
                                         p("there are 5 labelling options: none, both, up, down or own list"),
                                         selectInput('selected', 'type of output', selected_present),
                                         fileInput('file2', 'Choose selected gene file (.csv)',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         h4("select font for graph"),
                                         selectInput('font',
                                                     'font type',
                                                     choices = fonts, 
                                                     selected = fonts[1]),
                                         h4("Cut-offs"),
                                         fluidRow(
                                           column(6,numericInput("Pvalue", "p-value", value=0.05)),
                                           column(6,numericInput("FC", "absolute logFC", value=0.58))
                                         ),
                                         textInput(inputId = "sig_lines", label = "significance lines",value = "grey"),
                                         h4("Axis parameters"),
                                         textInput(inputId = "expression_y2", 
                                                   label = "Y-axis label",
                                                   value = "p-value"),
                                         
                                         fluidRow(
                                           column(4,numericInput("yhigh","y-axis upper range",value = 100)),
                                           column(4,numericInput("ybreaks","y-axis tick marks",value = 10))
                                           
                                         ),
                                         fluidRow(
                                           column(4,numericInput("xlow","x-axis lower range",value = -10)),
                                           column(4,numericInput("xhigh","x-axis upper range",value = 10)),
                                           column(4, numericInput("xbreaks","x-axis tick marks",value = 1))
                                         ),
                                         sliderInput("axis", "axis text size", min=0, max=100, value=30, step=0.1),
                                         sliderInput("axis_text", "axis numeric text size", min=0, max=100, value=30, step=0.1),
                                         h4("point size, shape and transparancy"),
                                         sliderInput("alpha1", "Transparency of significant (highlighted only)", min=0.01, max=1, value=1,step = 0.01),
                                         sliderInput("alpha2", "Transparency of significant", min=0.01, max=1, value=0.5,step = 0.01),
                                         sliderInput("alpha3", "Transparency of non-significant", min=0.01, max=1, value=0.25,step = 0.01),
                                         # img(class="img-polaroid", src = "http://www.sthda.com/sthda/RDoc/images/points-symbols.png",width=200,height=200),
                                         fluidRow(
                                           column(4,numericInput("shape1","Shape of highlighted",value = 19)),
                                           column(4,numericInput("shape2","Shape of significant",value = 19)),
                                           column(4,numericInput("shape3","Shape of non-significant",value = 1))
                                         ),
                                         fluidRow(
                                           column(4, numericInput("size1","Size of highlighted",value = 3)),
                                           column(4,numericInput("size2","Size of significant",value = 3)),
                                           column(4,numericInput("size3","Size of non-significant",value = 1))
                                         ),
                                         h4("Colour of points"),
                                         fluidRow(
                                           column(6,textInput(inputId = "up", label = "up-regulated",value = "red")),
                                           column(6,textInput(inputId = "down",  label = "down-regulated", value = "steelblue1")),
                                           column(6,textInput(inputId = "NS", label = "Non-significant",value = "grey")),
                                           
                                         ),
                                         p(" "),
                                         fluidRow(
                                           column(6,textInput(inputId = "col_lab1", label = "label one",value = "darkblue")),
                                           column(6,selectInput(inputId = "lab1", 
                                                                label = "select label 1", 
                                                                choices = lab, 
                                                                selected = "significant down")),
                                           column(6,textInput(inputId = "col_lab2", label = "label two",value = "orange")),
                                           column(6,selectInput(inputId = "lab2", 
                                                                label = "select label 2", 
                                                                choices = lab, 
                                                                selected = "significant up")),
                                           column(6,textInput(inputId = "col_lab3", label = "label three",value = "purple")),
                                           column(6,selectInput(inputId = "lab3", 
                                                                label = "select label 3", 
                                                                choices = lab, 
                                                                selected = "non-significant in list"))
                                         ),
                                         
                                         h4("Label parameters"),
                                         fluidRow(
                                           column(6,numericInput("min", "label range (min)", value=1)),
                                           column(6, numericInput("max", "label range (max)", value=20))
                                         ),
                                         sliderInput("dist", "distance of label", min=0, max=2, value=0.25,step = 0.01),
                                         sliderInput("label", "size of labels", min=0, max=60, value=8,step =0.1),
                                         h4("Legend parameters"),
                                         fluidRow(
                                           column(4,selectInput('legend_location', 'Legend location', legend_location)),
                                           column(4,numericInput("col", "# of legend columns", value=1, step = 1)),
                                           column(4, numericInput("legend_size", "legend text size", min=1, max=60, value=12))
                                         ),
                            ),
                            mainPanel(tabsetPanel(
                              tabPanel("Volcano plot", textInput(inputId = "title", 
                                                                 label = "",
                                                                 value = "Volcano plot: x vs y"),
                                       textOutput("number_of_points"),
                                       textOutput("sig_values_test"),
                                       plotOutput("ggplot",height = "600px"),
                                       h4("Exporting the ggVolcanoR plot"),
                                       fluidRow(
                                         column(4,numericInput("width", "width of PDF", value=10)),
                                         column(4,numericInput("height", "height of PDF", value=8)),
                                         column(4,downloadButton('downloadPlot','Download PDF'))
                                       ),
                                       
                                       fluidRow(
                                         column(3,numericInput("width_png","width of png", value = 1600)),
                                         column(3,numericInput("height_png","height of png", value = 1200)),
                                         column(3,numericInput("resolution_PNG","resolution of PNG", value = 144)),
                                         column(3,downloadButton('downloadPlotPNG','Download PNG'))
                                       ),
                                       
                              ),
                              tabPanel("Table with links", 
                                       selectInput("species", label = "List of the 16 common species used in uniprot",species,selected = "HUMAN"),
                                       
                                       div(DT::dataTableOutput("myoutput",height="600px"))),
                              
                              tabPanel("Summary table",DT::dataTableOutput("summary_table",height="300px"),
                                       selectInput(inputId = "export", 
                                                   label = "Selected Filtered csv file", 
                                                   choices = filtered_table, 
                                                   selected = filtered_table[1]),
                                       downloadButton("downloadTABLE", "Filtered Table"))
                              
                            ))
                          )
                 ),
                 # correlation graphs -----
                 tabPanel("Correlation graph",
                          sidebarLayout(
                            sidebarPanel(id = "tPanel2",style = "overflow-y:scroll; max-height: 800px; position:relative;", width=4,
                                         h4("Corrleation plot parameters"),
                                         selectInput("dataset2", "Choose a dataset:", choices = c("test-data", "own")),
                                         fileInput('file3', 'ID, logFC, Pvalue (x-axis)',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         fluidRow(
                                           column(6,radioButtons('sep3', 'Separator', c( Tab='\t', Comma=','), ',')),
                                           column(6,radioButtons('quote3', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'))
                                         ),
                                         tags$hr(),
                                         fileInput('file4', 'ID, logFC, Pvalue (y-axis)',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         fluidRow(
                                           column(6,radioButtons('sep4', 'Separator', c( Tab='\t', Comma=','), ',')),
                                           column(6,radioButtons('quote4', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'))
                                         ),
                                         
                                         selectInput('font2',
                                                     'font type',
                                                     choices = fonts, 
                                                     selected = fonts[1]),
                                         h4("axis text labels in the brackets"),
                                         fluidRow(column(6,textInput(inputId = "expression_x", 
                                                                     label = "x-axis label",
                                                                     value = "Proteomics")),
                                                  column(6,textInput(inputId = "expression_y", 
                                                                     label = "y-axis label",
                                                                     value = "Transcriptomics"))
                                                  
                                         ),
                                         h4("cut-off"),
                                         fluidRow(
                                           column(6,numericInput("Pvalue1", "p-value (x-axis)", value=0.05)),
                                           column(6,numericInput("FC1", "absolute logFC (x-axis)", value=0.58)),
                                           column(6,numericInput("Pvalue2", "p-value (y-axis)", value=0.05)),
                                           column(6,numericInput("FC2", "absolute logFC (y-axis)", value=0.58))
                                         ),
                                         h4("axis text size"),
                                         fluidRow(
                                           column(6,numericInput("axis2", "axis text size", min=0, value=24)),
                                           column(6,numericInput("axis_text2", "axis numeric text size", min=0, value=24))
                                         ),
                                         h4("colour of points"),
                                         fluidRow(
                                           column(3,textInput(inputId = "col1", label = "same up",value = "red")),
                                           column(3,textInput(inputId = "col2", label = "same down",value = "blue")),
                                           column(3, textInput(inputId = "col3", label = "opposite",value = "orange")),
                                           column(3,textInput(inputId = "col4", label = "other",value = "grey"))
                                         ),
                                         h4("axis tick marks"),
                                         fluidRow(
                                           column(6,numericInput("cor_xbreaks","x-axis tick marks",value = 1)),
                                           column(6,numericInput("cor_ybreaks","y-axis tick marks",value = 2))
                                         ),
                                         h4("colours of correlation line"),
                                         fluidRow(
                                           column(6,textInput(inputId = "cor_sig_lines", label = "significance lines",value = "grey")),
                                           column(6,textInput(inputId = "linecolour", label = "Correlation line colour",value = "red")),
                                           column(6,textInput(inputId = "CI95_fill", label = "95% CI colour",value = "grey"))
                                         ),
                                         h4("Data point"),
                                         sliderInput("cor_alpha1", "transparency of up", min=0.01, max=1, value=1,step = 0.01),
                                         sliderInput("cor_alpha2", "transparency of down", min=0.01, max=1, value=1,step = 0.01),
                                         sliderInput("cor_alpha3", "transparency of down", min=0.01, max=1, value=1,step = 0.01),
                                         sliderInput("cor_alpha4", "transparency of other", min=0.01, max=1, value=0.25,step = 0.01),
                                         fluidRow(
                                           column(3, numericInput("cor_shape1","shape of up",value = 19)),
                                           column(3,numericInput("cor_shape2","shape of down",value = 19)),
                                           column(3,numericInput("cor_shape3","shape of opposite",value = 19)),
                                           column(3,numericInput("cor_shape4","shape of other",value = 1))
                                         ),
                                         
                                         fluidRow(
                                           column(3,numericInput("cor_size1","Size of up",value = 3)),
                                           column(3,numericInput("cor_size2","Size of down",value = 3)),
                                           column(3,numericInput("cor_size3","Size of opposite",value = 3)),
                                           column(3,numericInput("cor_size4","Size of other",value = 1))
                                         ),
                                         fluidRow(
                                           column(6,selectInput('legend_location2', 'Legend location', legend_location)),
                                           column(6,numericInput("legend_size2", "legend text size", min=1, max=60, value=12))
                                         )
                                         
                                         
                            ),
                            mainPanel(tabsetPanel(
                              tabPanel("correlation graph", 
                                       fluidRow(
                                         column(3, textInput(inputId = "title2", 
                                                             label = "",
                                                             value = "Correlation plot: x vs y")),
                                         
                                         column(2,checkboxInput("label3", label = "Display labels", value = FALSE)),
                                         column(2,checkboxInput("reg.line", label = "Display regression line", value = FALSE)),
                                         column(2,checkboxInput("sort_direction", label = "Swap direction", value = TRUE)),
                                         column(3,sliderInput("sort_by","2=x-axis & 4=y-axis",min=2,max=4,value=2,step=2))),
                                       
                                       fluidRow(
                                         column(3,numericInput("min2", "label range (min)", value=1)),
                                         column(3,numericInput("max2", "label range (max)", value=20)),
                                         column(3,numericInput("label2", "size of labels",value=8)),
                                         column(3,numericInput("dist2", "distance of label", value=1))),
                                       
                                       plotOutput("cor_graph",height = "600px"),
                                       p(" "),
                                       textOutput("cor_test"),
                                       p(" "),
                                       textOutput("cor_test_sig"),
                                       h4("Exporting the correlation plot"),
                                       fluidRow(
                                         column(4,numericInput("cor_width", "width of PDF", value=10)),
                                         column(4,numericInput("cor_height", "height of PDF", value=8)),
                                         column(4,downloadButton('downloadPlot2','Download PDF'))
                                         
                                       ),
                                       
                                       fluidRow(
                                         column(3,numericInput("cor_width_png","width of png", value = 1600)),
                                         column(3,numericInput("cor_height_png","height of png", value = 1200)),
                                         column(3,numericInput("cor_resolution_PNG","resolution of PNG", value = 144)),
                                         column(3,downloadButton('downloadPlotPNG2','Download PNG'))
                                       ),
                                       
                                       
                                       
                                       
                              ),
                              tabPanel("Datatable",DT::dataTableOutput("Table5"),
                                       p("download the data that has significant overlap (logFC in the same direction)"),
                                       downloadButton("downloadTABLE2", "Filtered Table")
                                       
                                       
                              ),
                              tabPanel("Bar graph", 
                                       
                                       fluidRow(h4("bar graph parameters"),
                                                
                                                column(3, selectInput("direction","Select which direction to show",
                                                                      choices = direction,
                                                                      selected = "all")),
                                                column(3,numericInput("bar_xbreaks","x-axis tick marks",value = 1)),
                                                column(3,textInput(inputId = "col5", label = "dataset 1 (x-axis)",value = "lightgreen")),
                                                column(3,textInput(inputId = "col6", label = "dataset 2 (y-axis)",value = "grey"))
                                                
                                       ),
                                       fluidRow(
                                         column(4,sliderInput("axis3", "x-axis text size", min=0, max=100, value=30, step=0.1)),
                                         column(4,sliderInput("bar_text_y", "Size of ID in bar graph", min=0, max=100, value=5, step=0.1)),
                                         column(4,sliderInput("bar_text_x", "Size of number in bar graph", min=0, max=100, value=12, step=0.1))
                                       ),
                                       
                                       
                                       plotOutput("logFC_direction", height = "600px"),
                                       
                                       fluidRow(
                                         column(3,numericInput("bar_width", "width of PDF", value=10)),
                                         column(3,numericInput("bar_height", "height of PDF", value=8)),
                                         downloadButton('downloadPlot3','Download PDF')
                                       ),
                                       
                                       fluidRow(
                                         column(3,numericInput("bar_width_png","width of png", value = 1600)),
                                         column(3,numericInput("bar_height_png","height of png", value = 1200)),
                                         column(3,numericInput("bar_resolution_PNG","resolution of PNG", value = 144)),
                                         column(3,downloadButton('downloadPlotPNG3','Download PNG'))
                                       ),
                                       
                              )
                              
                            )
                            )
                          )
                 ),
                 
                 
                 tabPanel("Convert ID",
                          sidebarLayout(
                            sidebarPanel(id = "tPanel3",style = "overflow-y:scroll; max-height: 800px; position:relative;", width=4,
                                         h4("Corrleation plot parameters"),
                                         
                            ),
                            mainPanel(tabsetPanel(
                              tabPanel("Panel1",
                                       selectInput("name.conversion.required","For human data, does the ID need to be converted to merge the two datasets?",
                                                   choices = name.conversion.required,
                                                   selected = FALSE),
                                       selectInput("conversion1","Merge file 1 by ensemble, Symbol or Uniprot ID",
                                                   choices = names.conversion,
                                                   selected = "Gene.Name"),
                                       selectInput("conversion2","Merge file 2 by ensemble, Symbol or Uniprot ID",
                                                   choices = names.conversion,
                                                   selected = "Gene.Name"),
                              ),
                              tabPanel("Panel2"
                              ),
                              tabPanel("Panel3"
                              )
                              
                            )
                            )
                          )
                 ),
                 
                 navbarMenu("More",
                            tabPanel("Volcano plot Read Me file",
                                     fluidRow(includeMarkdown("README.md")
                                              
                                     )
                            ),
                            
                            tabPanel("Correlation plot Read Me file",
                                     fluidRow(includeMarkdown("README_Correlationplot.md")
                                              
                                     )
                            ),
                            
                            tabPanel("Session info", 
                                     tabPanel("Session info", verbatimTextOutput("sessionInfo"))
                            )
                 ))
# sever -----
server  <- function(input, output, session) {
  
  vals <- reactiveValues(ggplot=NULL)
  vals2 <- reactiveValues(cor_graph=NULL)
  vals3 <- reactiveValues(logFC_direction=NULL)
  
  output$sessionInfo <- renderPrint({
    print(sessionInfo())
  })
  
  options(shiny.sanitize.errors = F)
  
  input.data <- reactive({switch(input$dataset,"test-data" = test.data(),"own" = own.data())})
  
  test.data <- reactive({
    dataframe = read.csv("test-data/Proteomics data.csv") })
  
  own.data <- reactive({
    inFile <- input$file1 
    if (is.null(inFile)) return(NULL)
    
    else {
      dataframe <- read.csv(
        inFile$datapath,
        header=TRUE,
        sep=input$sep,
        quote=input$quote)}
    
  })
  
  input.data2 <- reactive({switch(input$dataset,"test-data" = test.data2(),"own" = own.data2())})
  
  test.data2 <- reactive({ 
    dataframe2= read.csv("test-data/Refined list.csv")})
  
  own.data2 <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2)) return(NULL)
    else {
      dataframe2 <- read.csv(
        inFile2$datapath,
        header=T)}
  })
  
  output$summary_table <-DT::renderDataTable({
    
    dat <- input.data();
    
    validate(
      need(nrow(dat)>0,
           error_message_val1)
    )
    
    dat2 <- input.data2();
    list <- dat2$ID
    dat <- as.data.frame(dat)
    neg <- -1*input$FC
    pos <- input$FC
    
    if (input$selected=="own list") {
      sub.mutateddf.gene3 <- mutate(dat,
                                    significance=ifelse(dat$ID %in% list & abs(dat$logFC)>pos & dat$Pvalue<input$Pvalue,"sig_list",
                                                        ifelse(dat$ID %in% list, "list not significant","not in list")))
      sub.mutateddf.gene3$count <- 1
      summary <- as.data.frame(ddply(sub.mutateddf.gene3,c("significance"),numcolwise(sum)))[c(1,4)]
      a <- c('total',sum(summary$count))
      summary <- as.data.frame(t(summary))
      summary$V4 <- a
      summary <- as.data.frame(t(summary))
      rownames(summary) <- 1:4
      summary
    }
    
    else if (input$selected=="no labels") {
      sub.mutateddf.gene3 <- mutate(dat,
                                    significance=ifelse(dat$Pvalue<input$Pvalue & dat$logFC>pos,"upregulated",
                                                        ifelse(dat$Pvalue<input$Pvalue & dat$logFC<neg,"downregulated","non significant")))
      
      sub.mutateddf.gene3$count <- 1
      summary <- as.data.frame(ddply(sub.mutateddf.gene3,c("significance"),numcolwise(sum)))[c(1,4)]
      a <- c('total',sum(summary$count))
      summary <- as.data.frame(t(summary))
      summary$V4 <- a
      summary <- as.data.frame(t(summary))
      rownames(summary) <- 1:4
      summary
    }
    
    else {
      sub.mutateddf.gene3 <- mutate(dat,
                                    significance=ifelse(dat$Pvalue<input$Pvalue & dat$logFC>pos,"upregulated",
                                                        ifelse(dat$Pvalue<input$Pvalue & dat$logFC<neg,"downregulated","non significant")))
      
      sub.mutateddf.gene3$count <- 1
      summary <- as.data.frame(ddply(sub.mutateddf.gene3,c("significance"),numcolwise(sum)))[c(1,4)]
      a <- c('total',sum(summary$count))
      summary <- as.data.frame(t(summary))
      summary$V4 <- a
      summary <- as.data.frame(t(summary))
      rownames(summary) <- 1:4
      summary}
    
  })
  
  plotInput <- function() {
    dat <- input.data();
    
    validate(
      need(nrow(dat)>0,
           error_message_val2)
    )
    
    
    dat2 <- input.data2();
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    
    list <- dat2$ID
    list2 <- dat2$ID
    neg <- -1*input$FC
    pos <- input$FC
    
    maximum <- input$max
    
    mutateddf <- mutate(dat, sig=ifelse(dat$Pvalue<0.05, "Pvalue<0.05", "Not Sig")) 
    sig <- subset(dat, dat$Pvalue<input$Pvalue & abs(dat$logFC)>input$FC)
    top <- sig[(input$min:input$max),]
    
    gene_list <- top$ID
    
    mutateddf.gene <- mutate(mutateddf, top=ifelse(mutateddf$ID %in% gene_list, "top", "other"))
    mutateddf.gene
    
    # no labels 
    sub.mutateddf.gene <- mutate(mutateddf.gene,
                                 colour=ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC>pos,"sig_up",
                                               ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")),
                                 alpha=ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$alpha2,
                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$alpha2,input$alpha3)),
                                 shape=ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$shape2,
                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$shape2,input$shape3)),
                                 size=ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$size2,
                                             ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$size2,input$size3)))
    # range of genes
    sub.mutateddf.gene2 <- mutate(mutateddf.gene,
                                  colour=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, "top_up",
                                                ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, "top_down",                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,"sig_up",
                                                                                                                                                                                                                                                                      ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")))),
                                  alpha=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,
                                               ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$alpha2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$alpha2,input$alpha3)))),
                                  size=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$size1,
                                              ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$size1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$size2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$size2,input$size3)))),
                                  shape=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$shape1,
                                               ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$shape1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$shape2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$shape2,input$shape3))))
    )
    
    
    colour_class <- c("NS","sig_down","sig_up","top_down","top_up")
    
    sub.mutateddf.gene2$colour <- factor(sub.mutateddf.gene2$colour, levels = colour_class)
    
    y_lable1 <- bquote("-"~Log[10]~(.(input$expression_y2)))
    y_lable1
    if (input$selected=="range (both directions)") {
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene2$logFC, y=-log10(sub.mutateddf.gene2$Pvalue),
                       col=sub.mutateddf.gene2$colour,shape=sub.mutateddf.gene2$colour),size=sub.mutateddf.gene2$size,alpha=sub.mutateddf.gene2$alpha) +
        geom_text_repel(data=sub.mutateddf.gene2[sub.mutateddf.gene2$ID %in% gene_list,]
                        ,aes(x=sub.mutateddf.gene2$logFC[sub.mutateddf.gene2$ID %in% gene_list], 
                             y=  -log10(sub.mutateddf.gene2$Pvalue)[sub.mutateddf.gene2$ID %in% gene_list],
                             label= sub.mutateddf.gene2$ID[sub.mutateddf.gene2$ID %in% gene_list]),
                        size=input$label,
                        family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines'), 
                        max.overlaps = Inf) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab1,input$col_lab2),
                           labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2)) +
        scale_shape_manual(values=c(input$shape3,input$shape2,input$shape2,input$shape1,input$shape1),
                           labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2)) +
        
        theme_bw(base_size = 18)+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        geom_vline(xintercept=pos, linetype="dashed", color = input$sig_lines) +
        geom_vline(xintercept=neg, linetype="dashed", color = input$sig_lines) +
        geom_hline(yintercept=-log10(input$Pvalue), linetype="dashed", color = input$sig_lines) +
        theme(text=element_text(size=20,family=input$font),
              axis.title = element_text(colour="black", size=input$axis,family=input$font),
              axis.text.x = element_text(colour="black",size=input$axis_text,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.text.y = element_text(colour="black",size=input$axis_text,angle=0,hjust=1,vjust=0,face="plain",family=input$font),
              axis.title.x=element_text(colour="black",size=input$axis,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.title.y = element_text(colour="black",size=input$axis,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size),
              legend.position = input$legend_location, 
              legend.box="vertical",
              legend.margin=margin(),
              legend.justification = "top")+
        labs(y=y_lable1,
             x=expression(Log[2]~Fold~Change),
             title=input$title) +
        guides(size=FALSE, col = guide_legend(ncol=input$col))+
        scale_alpha(guide = 'none')+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      vals$ggplot
      
    }
    else if (input$selected=="range (up direction)") {
      mutateddf.gene2 <- subset(mutateddf.gene,mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue)
      Ordered_df <-  mutateddf.gene2[order(mutateddf.gene2$Pvalue,decreasing = F),]
      
      top <- mutateddf.gene2[(input$min:input$max),]
      gene_list <- top$ID
      sub.mutateddf.gene2 <- mutate(mutateddf.gene,
                                    colour=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, "top_up",
                                                  ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, "top_down",                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,"sig_up",
                                                                                                                                                                                                                                                                        ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")))),
                                    alpha=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,
                                                 ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$alpha2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$alpha2,input$alpha3)))),
                                    size=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$size1,
                                                ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$size1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$size2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$size2,input$size3)))),
                                    shape=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$shape1,
                                                 ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$shape1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$shape2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$shape2,input$shape3))))
      )
      
      colour_class <- c("NS","sig_down","sig_up","top_down","top_up")
      
      sub.mutateddf.gene2$colour <- factor(sub.mutateddf.gene2$colour, levels = colour_class)
      
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene2$logFC, y=-log10(sub.mutateddf.gene2$Pvalue),
                       col=sub.mutateddf.gene2$colour,shape=sub.mutateddf.gene2$colour),size=sub.mutateddf.gene2$size,alpha=sub.mutateddf.gene2$alpha) +
        geom_text_repel(data=sub.mutateddf.gene2[sub.mutateddf.gene2$ID %in% gene_list,]
                        ,aes(x=sub.mutateddf.gene2$logFC[sub.mutateddf.gene2$ID %in% gene_list], 
                             y=  -log10(sub.mutateddf.gene2$Pvalue)[sub.mutateddf.gene2$ID %in% gene_list],
                             label= sub.mutateddf.gene2$ID[sub.mutateddf.gene2$ID %in% gene_list]),
                        size=input$label,
                        family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines'), 
                        max.overlaps = Inf) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        scale_color_manual(name= "legend",values=c(input$NS,input$down,input$up,input$col_lab2),labels=c("non-significant","down-regulated","up-regulated",input$lab2)) +
        scale_shape_manual(name= "legend",values=c(input$shape3,input$shape2,input$shape2,input$shape1),labels=c("non-significant","down-regulated","up-regulated",input$lab2)) +
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme_bw(base_size = 18)+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        geom_vline(xintercept=pos, linetype="dashed", color = input$sig_lines) +
        geom_vline(xintercept=neg, linetype="dashed", color = input$sig_lines) +
        geom_hline(yintercept=-log10(input$Pvalue), linetype="dashed", color = input$sig_lines) +
        theme(text=element_text(size=20,family=input$font),
              axis.title = element_text(colour="black", size=input$axis,family=input$font),
              axis.text.x = element_text(colour="black",size=input$axis_text,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.text.y = element_text(colour="black",size=input$axis_text,angle=0,hjust=1,vjust=0,face="plain",family=input$font),
              axis.title.x=element_text(colour="black",size=input$axis,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.title.y = element_text(colour="black",size=input$axis,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size),
              legend.position = input$legend_location, 
              legend.box="vertical",
              legend.margin=margin(),
              legend.justification = "top")+
        labs(y=y_lable1,
             x=expression(Log[2]~Fold~Change),
             title=input$title) +
        guides(size=FALSE, col = guide_legend(ncol=input$col))+
        scale_alpha(guide = 'none')+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      vals$ggplot
      
    }
    else if (input$selected=="range (down direction)") {
      
      
      mutateddf.gene2 <- subset(mutateddf.gene,mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue)
      Ordered_df <-  mutateddf.gene2[order(mutateddf.gene2$Pvalue,decreasing = F),]
      
      top <- mutateddf.gene2[(input$min:input$max),]
      gene_list <- top$ID
      sub.mutateddf.gene2 <- mutate(mutateddf.gene,
                                    colour=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, "top_up",
                                                  ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, "top_down",                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,"sig_up",
                                                                                                                                                                                                                                                                        ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")))),
                                    alpha=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,
                                                 ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$alpha2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$alpha2,input$alpha3)))),
                                    size=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$size1,
                                                ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$size1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$size2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$size2,input$size3)))),
                                    shape=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$shape1,
                                                 ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$shape1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$shape2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$shape2,input$shape3))))
      )
      
      
      colour_class <- c("NS","sig_down","sig_up","top_down","top_up")
      
      sub.mutateddf.gene2$colour <- factor(sub.mutateddf.gene2$colour, levels = colour_class)
      
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene2$logFC, y=-log10(sub.mutateddf.gene2$Pvalue),
                       col=sub.mutateddf.gene2$colour,shape=sub.mutateddf.gene2$colour),size=sub.mutateddf.gene2$size,alpha=sub.mutateddf.gene2$alpha) +
        geom_text_repel(data=sub.mutateddf.gene2[sub.mutateddf.gene2$ID %in% gene_list,]
                        ,aes(x=sub.mutateddf.gene2$logFC[sub.mutateddf.gene2$ID %in% gene_list], 
                             y=  -log10(sub.mutateddf.gene2$Pvalue)[sub.mutateddf.gene2$ID %in% gene_list],
                             label= sub.mutateddf.gene2$ID[sub.mutateddf.gene2$ID %in% gene_list]),
                        size=input$label,
                        family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines'), 
                        max.overlaps = Inf) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab1),labels=c("non-significant","down-regulated","up-regulated",input$lab1)) +
        scale_shape_manual(values=c(input$shape3,input$shape2,input$shape2,input$shape1),labels=c("non-significant","down-regulated","up-regulated",input$lab1)) +
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme_bw(base_size = 18)+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        geom_vline(xintercept=pos, linetype="dashed", color = input$sig_lines) +
        geom_vline(xintercept=neg, linetype="dashed", color = input$sig_lines) +
        geom_hline(yintercept=-log10(input$Pvalue), linetype="dashed", color = input$sig_lines) +
        theme(text=element_text(size=20,family=input$font),
              axis.title = element_text(colour="black", size=input$axis,family=input$font),
              axis.text.x = element_text(colour="black",size=input$axis_text,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.text.y = element_text(colour="black",size=input$axis_text,angle=0,hjust=1,vjust=0,face="plain",family=input$font),
              axis.title.x=element_text(colour="black",size=input$axis,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.title.y = element_text(colour="black",size=input$axis,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size),
              legend.position = input$legend_location, 
              legend.box="vertical",
              legend.margin=margin(),
              legend.justification = "top")+
        labs(y=y_lable1,
             x=expression(Log[2]~Fold~Change),
             title=input$title) +
        guides(size=FALSE, col = guide_legend(ncol=input$col))+
        scale_alpha(guide = 'none')+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      vals$ggplot
      
    }
    else if (input$selected=="own list") {
      
      
      merged_list <- mutateddf[mutateddf$ID %in% list2,]
      merged_list <- merged_list[order(merged_list$Pvalue),]
      
      ordered_list <- mutate(merged_list,df_order=ifelse(merged_list$ID %in% list2 & abs(merged_list$logFC)>pos & merged_list$Pvalue<input$Pvalue, "1","2"))
      ordered_list <- ordered_list[order(ordered_list$df_order),]
      sig2 <- ordered_list
      list2 <- sig2$ID
      
      sub.mutateddf.gene_list <- mutate(mutateddf.gene,
                                        colour=ifelse(mutateddf.gene$ID %in% list2 & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, "zlist_1",
                                                      ifelse(mutateddf.gene$ID %in% list2 & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, "zlist_2",
                                                             ifelse(mutateddf.gene$ID %in% list2 & mutateddf.gene$logFC>neg & mutateddf.gene$Pvalue>input$Pvalue, "zlist_3",
                                                                    ifelse(mutateddf.gene$ID %in% list2 & mutateddf.gene$logFC<pos & mutateddf.gene$Pvalue>input$Pvalue, "zlist_3",
                                                                           ifelse(mutateddf.gene$ID %in% list2 & mutateddf.gene$logFC>neg & mutateddf.gene$Pvalue<input$Pvalue, "zlist_3",
                                                                                  ifelse(mutateddf.gene$ID %in% list2 & mutateddf.gene$logFC<pos & mutateddf.gene$Pvalue<input$Pvalue,  "zlist_3",
                                                                                         ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,"sig_up",
                                                                                                ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")))))))),
                                        alpha=ifelse(mutateddf.gene$ID %in% list2, input$alpha1, 
                                                     ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC>pos,input$alpha2,
                                                            ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC<neg,input$alpha2,input$alpha3))),
                                        size=ifelse(mutateddf.gene$ID %in% list2, input$size1, 
                                                    ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC>pos,input$size2,
                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC<neg,input$size2,input$size3))),
                                        shape=ifelse(mutateddf.gene$ID %in% list2, input$shape1, 
                                                     ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC>pos,input$shape2,
                                                            ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC<neg,input$shape2,input$shape3)))
                                        
      )
      
      colour_class <- c("NS","sig_down","sig_up","zlist_1","zlist_2","zlist_3")
      
      sub.mutateddf.gene_list$colour <- factor(sub.mutateddf.gene_list$colour, levels = colour_class)
      
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene_list$logFC, y=-log10(sub.mutateddf.gene_list$Pvalue),col=sub.mutateddf.gene_list$colour,shape=sub.mutateddf.gene_list$colour),size=sub.mutateddf.gene_list$size,alpha=sub.mutateddf.gene_list$alpha) +
        geom_text_repel(data=sub.mutateddf.gene_list[sub.mutateddf.gene_list$ID %in% list2[(input$min:input$max)],]
                        ,aes(x=sub.mutateddf.gene_list$logFC[sub.mutateddf.gene_list$ID %in% list2[(input$min:input$max)]], 
                             y= -log10(sub.mutateddf.gene_list$Pvalue)[sub.mutateddf.gene_list$ID %in% list2[(input$min:input$max)]],
                             label= sub.mutateddf.gene_list$ID[sub.mutateddf.gene_list$ID %in% list2[(input$min:input$max)]]),
                        size=input$label,family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines'), 
                        max.overlaps = Inf) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab1,input$col_lab2,input$col_lab3),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2,input$lab3)) +
        scale_shape_manual(values=c(input$shape3,input$shape2,input$shape2,input$shape1,input$shape1,input$shape1),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2,input$lab3)) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme_bw(base_size = 18)+
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        geom_vline(xintercept=pos, linetype="dashed", color = input$sig_lines) +
        geom_vline(xintercept=neg, linetype="dashed", color = input$sig_lines) +
        geom_hline(yintercept=-log10(input$Pvalue), linetype="dashed", color = input$sig_lines) +
        theme(text=element_text(size=20,family=input$font),
              axis.title = element_text(colour="black", size=input$axis,family=input$font),
              axis.text.x = element_text(colour="black",size=input$axis_text,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.text.y = element_text(colour="black",size=input$axis_text,angle=0,hjust=1,vjust=0,face="plain",family=input$font),
              axis.title.x=element_text(colour="black",size=input$axis,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.title.y = element_text(colour="black",size=input$axis,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size),
              legend.position = input$legend_location,
              legend.justification = "top")+
        guides(size=FALSE, col = guide_legend(ncol=input$col))+
        scale_alpha(guide = 'none')+ 
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))+
        labs(y=y_lable1,
             x=expression(Log[2]~Fold~Change),
             title=input$title) 
      
      vals$ggplot
    }
    else  {     
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene$logFC, y=-log10(sub.mutateddf.gene$Pvalue),col=sub.mutateddf.gene$colour,shape=sub.mutateddf.gene$colour),size=sub.mutateddf.gene$size,alpha=sub.mutateddf.gene$alpha) +
        scale_color_manual(values=c(input$NS,input$down,input$up),labels=c("non-significant","down-regulated","up-regulated")) + 
        scale_shape_manual(values=c(input$shape3,input$shape2,input$shape2),labels=c("non-significant","down-regulated","up-regulated"))+
        theme_bw(base_size = 18)+
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        geom_vline(xintercept=pos, linetype="dashed", color = input$sig_lines) +
        geom_vline(xintercept=neg, linetype="dashed", color = input$sig_lines) +
        geom_hline(yintercept=-log10(input$Pvalue), linetype="dashed", color = input$sig_lines) +
        theme(text=element_text(size=20,family=input$font),
              axis.title = element_text(colour="black", size=input$axis,family=input$font),
              axis.text.x = element_text(colour="black",size=input$axis_text,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.text.y = element_text(colour="black",size=input$axis_text,angle=0,hjust=1,vjust=0,face="plain",family=input$font),
              axis.title.x=element_text(colour="black",size=input$axis,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font),
              axis.title.y = element_text(colour="black",size=input$axis,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size),
              legend.position = input$legend_location,
              legend.justification = "top")+
        guides(size=FALSE, col = guide_legend(ncol=input$col))+
        scale_alpha(guide = 'none')+
        labs(y=y_lable1,
             x=expression(Log[2]~Fold~Change),
             title=input$title)+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      
      vals$ggplot
    }
  }
  output$ggplot <- renderPlot({
    withProgress(message = 'Figure is being generated...',
                 detail = '', value = 0, {
                   test_fun()
                 })
    print(plotInput())
  })
  
  output$myoutput <-DT::renderDataTable(escape = FALSE, {
    
    dat <- input.data();
    
    validate(
      need(nrow(dat)>0,
           error_message_1)
    )
    
    dat2 <- input.data2();
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    rownames(dat) <- 1:dim(dat)[1]
    
    
    if (input$selected=="range (both directions)") {
      head(ID.conversion)
      names(ID.conversion) <- c("Ensembl","Uniprot_human","UNIPROT","Chrom","ID","Biotype")
      
      dat <- subset(dat, dat$Pvalue<input$Pvalue & abs(dat$logFC)>input$FC)
      
      dat.top <- merge(dat,ID.conversion,by="ID", all.x=T)
      dat.top[is.na(dat.top)] <- "No_ID"
      dat.top <- dat.top[order(dat.top$Pvalue),]
      
      top <- dat.top[(input$min:input$max),]
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      top$GeneCards <- paste('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      top$Protein_atlas <- paste('<a href=https://www.proteinatlas.org/',top$Ensembl,' target="_blank" class="btn btn-link"','>',top$Ensembl,'</a>',sep="")
      top$Human_Uniprot <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$UNIPROT,' target="_blank" class="btn btn-link"','>',top$UNIPROT,"</a>", sep="")
      top$UniProt_species <- paste('<a href=https://www.uniprot.org/uniprot/?query=',SYMBOL_list$list,' target="_blank" class="btn btn-link"','>',SYMBOL_list$list,"</a>", sep="")
      top$UniProt <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df <- df[-c(4,5,6,7)]
      datatable(df, escape = FALSE,filter = 'top', options=list(extensions="Buttons", scrollX = T), selection = 'none') %>% 
        formatStyle(
          'logFC',
          backgroundColor = styleInterval(c(-input$FC,input$FC), c('#abd7eb', '#D2D2CF',"#ff6961")),
          color = styleInterval(c(-input$FC,input$FC), c('#181A18', '#181A18', '#181A18')),
          fontWeight = styleInterval(c(-input$FC,input$FC), c('bold', 'normal','bold'))) %>% 
        formatStyle(
          'Pvalue',
          backgroundColor = styleInterval(c(input$Pvalue), c('#181A18', '#D2D2CF')),
          color = styleInterval(c(input$Pvalue), c('#d99058',  '#181A18')),
          fontWeight = styleInterval(input$Pvalue, c('bold', 'normal'))) 
      
      
    }
    else if (input$selected=="no labels") {
      
      head(ID.conversion)
      names(ID.conversion) <- c("Ensembl","Uniprot_human","UNIPROT","Chrom","ID","Biotype")
      dat.top <- merge(dat,ID.conversion,by="ID", all.x=T)
      dat.top[is.na(dat.top)] <- "No_ID"
      dat.top <- dat.top[order(dat.top$Pvalue),]
      
      top <- dat.top
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      top$GeneCards <- paste('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      top$Human_Protein_atlas <- paste('<a href=https://www.proteinatlas.org/',top$Ensembl,' target="_blank" class="btn btn-link"','>',top$Ensembl,'</a>',sep="")
      top$Human_Uniprot <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$UNIPROT,' target="_blank" class="btn btn-link"','>',top$UNIPROT,"</a>", sep="")
      top$UniProt_species <- paste('<a href=https://www.uniprot.org/uniprot/?query=',SYMBOL_list$list,' target="_blank" class="btn btn-link"','>',SYMBOL_list$list,"</a>", sep="")
      top$UniProt <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df <- df[-c(4,5,6,7)]
      
      
      datatable(df, escape = FALSE,filter = 'top', options=list(scrollX = T), selection = 'none') %>% 
        formatStyle(
          'logFC',
          backgroundColor = styleInterval(c(-input$FC,input$FC), c('#abd7eb', '#D2D2CF',"#ff6961")),
          color = styleInterval(c(-input$FC,input$FC), c('#181A18', '#181A18', '#181A18')),
          fontWeight = styleInterval(c(-input$FC,input$FC), c('bold', 'normal','bold'))) %>% 
        formatStyle(
          'Pvalue',
          backgroundColor = styleInterval(c(input$Pvalue), c('#181A18', '#D2D2CF')),
          color = styleInterval(c(input$Pvalue), c('#d99058',  '#181A18')),
          fontWeight = styleInterval(input$Pvalue, c('bold', 'normal'))) 
      
    }
    else if (input$selected=="range (up direction)") {
      head(ID.conversion)
      names(ID.conversion) <- c("Ensembl","Uniprot_human","UNIPROT","Chrom","ID","Biotype")
      
      dat <- subset(dat, dat$Pvalue<input$Pvalue & dat$logFC>input$FC)
      
      dat.top <- merge(dat,ID.conversion,by="ID", all.x=T)
      dat.top[is.na(dat.top)] <- "No_ID"
      dat.top <- dat.top[order(dat.top$Pvalue),]
      
      top <- dat.top[(input$min:input$max),]
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      top$GeneCards <- paste('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      top$Protein_atlas <- paste('<a href=https://www.proteinatlas.org/',top$Ensembl,' target="_blank" class="btn btn-link"','>',top$Ensembl,'</a>',sep="")
      top$Human_Uniprot <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$UNIPROT,' target="_blank" class="btn btn-link"','>',top$UNIPROT,"</a>", sep="")
      top$UniProt_species <- paste('<a href=https://www.uniprot.org/uniprot/?query=',SYMBOL_list$list,' target="_blank" class="btn btn-link"','>',SYMBOL_list$list,"</a>", sep="")
      top$UniProt <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df <- df[-c(4,5,6,7)]
      datatable(df, escape = FALSE,filter = 'top', options=list(extensions="Buttons", scrollX = T), selection = 'none') %>% 
        formatStyle(
          'logFC',
          backgroundColor = styleInterval(c(-input$FC,input$FC), c('#abd7eb', '#D2D2CF',"#ff6961")),
          color = styleInterval(c(-input$FC,input$FC), c('#181A18', '#181A18', '#181A18')),
          fontWeight = styleInterval(c(-input$FC,input$FC), c('bold', 'normal','bold'))) %>% 
        formatStyle(
          'Pvalue',
          backgroundColor = styleInterval(c(input$Pvalue), c('#181A18', '#D2D2CF')),
          color = styleInterval(c(input$Pvalue), c('#d99058',  '#181A18')),
          fontWeight = styleInterval(input$Pvalue, c('bold', 'normal'))) 
      
      
    }
    
    else if (input$selected=="range (down direction)") {
      head(ID.conversion)
      names(ID.conversion) <- c("Ensembl","Uniprot_human","UNIPROT","Chrom","ID","Biotype")
      neg <- -1*input$FC
      dat <- subset(dat, dat$Pvalue<input$Pvalue & dat$logFC<neg)
      
      dat.top <- merge(dat,ID.conversion,by="ID", all.x=T)
      dat.top[is.na(dat.top)] <- "No_ID"
      dat.top <- dat.top[order(dat.top$Pvalue),]
      
      top <- dat.top[(input$min:input$max),]
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      top$GeneCards <- paste('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      top$Protein_atlas <- paste('<a href=https://www.proteinatlas.org/',top$Ensembl,' target="_blank" class="btn btn-link"','>',top$Ensembl,'</a>',sep="")
      top$Human_Uniprot <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$UNIPROT,' target="_blank" class="btn btn-link"','>',top$UNIPROT,"</a>", sep="")
      top$UniProt_species <- paste('<a href=https://www.uniprot.org/uniprot/?query=',SYMBOL_list$list,' target="_blank" class="btn btn-link"','>',SYMBOL_list$list,"</a>", sep="")
      top$UniProt <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df <- df[-c(4,5,6,7)]
      datatable(df, escape = FALSE,filter = 'top', options=list(extensions="Buttons", scrollX = T), selection = 'none') %>% 
        formatStyle(
          'logFC',
          backgroundColor = styleInterval(c(-input$FC,input$FC), c('#abd7eb', '#D2D2CF',"#ff6961")),
          color = styleInterval(c(-input$FC,input$FC), c('#181A18', '#181A18', '#181A18')),
          fontWeight = styleInterval(c(-input$FC,input$FC), c('bold', 'normal','bold'))) %>% 
        formatStyle(
          'Pvalue',
          backgroundColor = styleInterval(c(input$Pvalue), c('#181A18', '#D2D2CF')),
          color = styleInterval(c(input$Pvalue), c('#d99058',  '#181A18')),
          fontWeight = styleInterval(input$Pvalue, c('bold', 'normal'))) 
      
      
    }
    else {
      
      
      list <- dat2$ID
      head(ID.conversion)
      names(ID.conversion) <- c("Ensembl","Uniprot_human","UNIPROT","Chrom","ID","Biotype")
      
      dat <- dat[dat$ID %in% list,]
      
      dat.top <- merge(dat,ID.conversion,by.x="ID", by.y="ID", all.x=T)
      dat.top[is.na(dat.top)] <- "No_ID"
      dat.top <- dat.top[order(dat.top$Pvalue),]
      
      top <- dat.top
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      top$GeneCards <- paste('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      top$Protein_atlas <- paste('<a href=https://www.proteinatlas.org/',top$Ensembl,' target="_blank" class="btn btn-link"','>',top$Ensembl,'</a>',sep="")
      top$Human_Uniprot <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$UNIPROT,' target="_blank" class="btn btn-link"','>',top$UNIPROT,"</a>", sep="")
      top$UniProt_species <- paste('<a href=https://www.uniprot.org/uniprot/?query=',SYMBOL_list$list,' target="_blank" class="btn btn-link"','>',SYMBOL_list$list,"</a>", sep="")
      top$UniProt <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      
      df <- top
      
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df <- df[-c(4,5,6,7)]
      
      datatable(df, escape = FALSE,filter = 'top', options=list(extensions="Buttons", scrollX = T), selection = 'none') %>% 
        formatStyle(
          'logFC',
          backgroundColor = styleInterval(c(-input$FC,input$FC), c('#abd7eb', '#D2D2CF',"#ff6961")),
          color = styleInterval(c(-input$FC,input$FC), c('#181A18', '#181A18', '#181A18')),
          fontWeight = styleInterval(c(-input$FC,input$FC), c('bold', 'normal','bold'))) %>% 
        formatStyle(
          'Pvalue',
          backgroundColor = styleInterval(c(input$Pvalue), c('#181A18', '#D2D2CF')),
          color = styleInterval(c(input$Pvalue), c('#d99058',  '#181A18')),
          fontWeight = styleInterval(input$Pvalue, c('bold', 'normal'))) 
      
    }
    
  })
  dataExpTable <- reactive({
    dat <- input.data();
    dat2 <- input.data2();
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    list <- dat2$ID
    neg <- -1*input$FC
    pos <- input$FC
    
    if (input$export=="upregulated") {
      positive <- subset(dat, dat$Pvalue<input$Pvalue & dat$logFC>pos)
      positive
    }
    else if (input$export=="downregulated") {
      negative <- subset(dat, dat$Pvalue<input$Pvalue & dat$logFC<neg)
      negative
    }
    else if (input$export=="own list") {
      
      ownlist <- dat[dat$ID %in% list & dat$Pvalue<input$Pvalue & abs(dat$logFC)>input$FC,]
    }
    else { 
      both <- subset(dat, dat$Pvalue<input$Pvalue & dat$logFC<neg | dat$logFC>pos)
      both }
  })
  
  output$number_of_points <- renderPrint({
    
    dat <- input.data();
    
    validate(
      need(nrow(dat)>0,
           error_message_val1)
    )
    
    
    dat2 <- input.data2();
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    list <- dat2$ID
    list2 <- dat2$ID
    dat$logP <- -log10(dat$Pvalue)
    total <- as.numeric(dim(dat)[1])
    subsetted <- subset(dat, dat$logP<input$yhigh)
    subsetted_2 <- subset(subsetted, subsetted$logFC>input$xlow)
    subsetted_3 <- subset(subsetted_2, subsetted_2$logFC<input$xhigh)
    
    
    points_displayed <- as.numeric(dim(subsetted_3)[1])
    cat(noquote(paste("There are ", points_displayed," points displayed out of ", total, " which represents ",round(points_displayed/total*100,2),"% of the data",sep="")))
    
  })
  output$sig_values_test <- renderPrint({
    
    dat <- input.data();
    
    validate(
      need(nrow(dat)>0,
           " ")
    )
    
    
    dat2 <- input.data2();
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    
    sig_value <- min(dat$Pvalue)
    x.min <- min(dat$logFC)
    x.max <- max(dat$logFC)
    cat(noquote(paste("To display all points: y-axis upper range = ", round(-log10(sig_value),0),", x-axis lower range = ", round(x.min,0), " and x-axis upper range = ", round(x.max,0),sep="")))
    
  })
  
  # downloading PDF -----
  output$downloadPlot <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("ggVolcanoR_",input$title, gsub("/", "-", x), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width=input$width,height=input$height, onefile = FALSE) # open the pdf device
      grid.arrange(vals$ggplot)
      dev.off()},
    
    contentType = "application/pdf"
    
  )
  
  output$downloadPlotPNG <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("ggVolcanoR_",input$title, gsub("/", "-", x), ".png", sep = "")
    },
    content = function(file) {
      
      png(file, width = input$width_png, height = input$height_png, res = input$resolution_PNG)
      grid.arrange(vals$ggplot)
      dev.off()},
    
    contentType = "application/png" # MIME type of the image
    
  )
  
  output$downloadTABLE <- downloadHandler(
    filename = function(){
      paste("ggVolcanoR_",gsub("-", ".", Sys.Date()), " - ","Filtered ",input$FC," ",input$Pvalue," ",input$export,".csv", sep = "")
    },
    content = function(file){
      write.csv(dataExpTable(),file, row.names = FALSE)
    }
  )
  
  
  # correlation graph server ----
  input.data3 <- reactive({switch(input$dataset2,"test-data" = test.data3(),"own" = own.data3())})
  test.data3 <- reactive({
    dataframe = read.csv("test-data/Proteomics data.csv") })
  own.data3 <- reactive({
    inFile3 <- input$file3 
    if (is.null(inFile3)) return(NULL)
    
    else {
      dataframe <- read.csv(
        inFile3$datapath,
        header=TRUE,
        sep=input$sep3,
        quote=input$quote4)}
    
  })
  input.data4 <- reactive({switch(input$dataset2,"test-data" = test.data4(),"own" = own.data4())})
  test.data4 <- reactive({
    dataframe = read.csv("test-data/Transcriptomics data.csv") })
  own.data4 <- reactive({
    inFile4 <- input$file4 
    if (is.null(inFile4)) return(NULL)
    
    else {
      dataframe <- read.csv(
        inFile4$datapath,
        header=TRUE,
        sep=input$sep4,
        quote=input$quote4)}
    
  })
  # Merging the two plots ----
  plotInput2 <- function() {
    dat4 <- input.data4();
    dat3 <- input.data3();
    
    neg1 <- -1*input$FC1
    pos1 <- input$FC1
    
    neg2 <- -1*input$FC2
    pos2 <- input$FC2
    
    validate(
      need(nrow(dat3)>0,
           error_message_val3)
    ) 
    
    validate(
      need(nrow(dat4)>0,
           error_message_val3)
    ) 
    
    dat5 <- merge(dat3,dat4,by="ID")
    dat_all <- mutate(dat5,
                      colour=ifelse(dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_up",
                                    ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_down",
                                           ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 &  dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, "opposite",
                                                  ifelse( dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, "opposite",
                                                          "other")))),
                      alpha=ifelse(dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, input$cor_alpha1,
                                   ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, input$cor_alpha2, 
                                          ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 &  dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, input$cor_alpha3,
                                                 ifelse( dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, input$cor_alpha3,input$cor_alpha4)))),
                      size_of_point=ifelse(dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, input$cor_size1,
                                           ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, input$cor_size2, 
                                                  ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 &  dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, input$cor_size3,
                                                         ifelse(dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, input$cor_size3,input$cor_size4))))
    )
    
    
    colour_class <- c("both_sig_up","both_sig_down","opposite","other")
    
    dat_all$colour <- factor(dat_all$colour, levels = colour_class)
    x_lable1 <- bquote(Log[2]~Fold~Change~(.(input$expression_x)))
    y_lable1 <- bquote(Log[2]~Fold~Change~(.(input$expression_y)))
    
    dat_all <- dat_all[order(dat_all[,input$sort_by],decreasing = input$sort_direction),]
    
    ID_sig <- subset(dat_all,dat_all$colour=="both_sig_up" | dat_all$colour=="both_sig_down")[input$min2:input$max2,]
    
    
    
    if (input$label3 == TRUE && input$reg.line == FALSE) {
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=c(input$col1,input$col2,input$col3,input$col4), labels = c("overlapping up","overlapping down","opposite","other")) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = c("overlapping up","overlapping down","opposite","other")) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        theme(axis.title = element_text(colour="black", size=input$axis2,family=input$font2),
              axis.text.x = element_text(colour="black",size=input$axis_text2,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2),
              axis.text.y = element_text(colour="black",size=input$axis_text2,angle=0,hjust=1,vjust=0,face="plain",family=input$font2),
              axis.title.x=element_text(colour="black",size=input$axis2,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2),
              axis.title.y = element_text(colour="black",size=input$axis2,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font2),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size2,face="plain",family=input$font2),
              legend.position = input$legend_location2,
              legend.justification = "top")+
        scale_y_continuous(breaks = seq(-1e6, 1e6, by = input$cor_ybreaks))+
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$cor_xbreaks)) +
        geom_text_repel(data=dat_all[dat_all$ID %in% ID_sig$ID,],aes(x=dat_all$logFC.x[dat_all$ID %in% ID_sig$ID], 
                                                                     y= dat_all$logFC.y[dat_all$ID %in% ID_sig$ID],
                                                                     label= dat_all$ID[dat_all$ID %in% ID_sig$ID]),
                        size=input$label2,
                        family=input$font2, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist2, 'lines'), 
                        max.overlaps = Inf) 
      vals2$cor_graph}
    else if (input$label3 == TRUE && input$reg.line == TRUE) {
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=c(input$col1,input$col2,input$col3,input$col4), labels = c("overlapping up","overlapping down","opposite","other")) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = c("overlapping up","overlapping down","opposite","other")) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        theme(axis.title = element_text(colour="black", size=input$axis2,family=input$font2),
              axis.text.x = element_text(colour="black",size=input$axis_text2,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2),
              axis.text.y = element_text(colour="black",size=input$axis_text2,angle=0,hjust=1,vjust=0,face="plain",family=input$font2),
              axis.title.x=element_text(colour="black",size=input$axis2,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2),
              axis.title.y = element_text(colour="black",size=input$axis2,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font2),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size2,face="plain",family=input$font2),
              legend.position = input$legend_location2,
              legend.justification = "top")+
        scale_y_continuous(breaks = seq(-1e6, 1e6, by = input$cor_ybreaks))+
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$cor_xbreaks)) + 
        geom_smooth(aes(x=dat_all$logFC.x,y=dat_all$logFC.y),method="lm", se=T, fullrange=T, level=0.95,color=input$linecolour, fill=input$CI95_fill)  +
        geom_text_repel(data=dat_all[dat_all$ID %in% ID_sig$ID,],aes(x=dat_all$logFC.x[dat_all$ID %in% ID_sig$ID], 
                                                                     y= dat_all$logFC.y[dat_all$ID %in% ID_sig$ID],
                                                                     label= dat_all$ID[dat_all$ID %in% ID_sig$ID]),
                        size=input$label2,
                        family=input$font2, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist2, 'lines'), 
                        max.overlaps = Inf) 
      vals2$cor_graph}
    else if (input$label3 == FALSE && input$reg.line == TRUE) {
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=c(input$col1,input$col2,input$col3,input$col4), labels = c("overlapping up","overlapping down","opposite","other")) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = c("overlapping up","overlapping down","opposite","other")) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        theme(axis.title = element_text(colour="black", size=input$axis2,family=input$font2),
              axis.text.x = element_text(colour="black",size=input$axis_text2,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2),
              axis.text.y = element_text(colour="black",size=input$axis_text2,angle=0,hjust=1,vjust=0,face="plain",family=input$font2),
              axis.title.x=element_text(colour="black",size=input$axis2,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2),
              axis.title.y = element_text(colour="black",size=input$axis2,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font2),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size2,face="plain",family=input$font2),
              legend.position = input$legend_location2,
              legend.justification = "top")+
        scale_y_continuous(breaks = seq(-1e6, 1e6, by = input$cor_ybreaks))+
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$cor_xbreaks)) + 
        geom_smooth(aes(x=dat_all$logFC.x,y=dat_all$logFC.y),method="lm", se=T, fullrange=T, level=0.95,color=input$linecolour, fill=input$CI95_fill)  
      vals2$cor_graph}
    else {
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=c(input$col1,input$col2,input$col3,input$col4), labels = c("overlapping up","overlapping down","opposite","other")) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = c("overlapping up","overlapping down","opposite","other")) +
        guides(shape = guide_legend(override.aes = list(size = 5))) +
        theme(axis.title = element_text(colour="black", size=input$axis2,family=input$font2),
              axis.text.x = element_text(colour="black",size=input$axis_text2,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2),
              axis.text.y = element_text(colour="black",size=input$axis_text2,angle=0,hjust=1,vjust=0,face="plain",family=input$font2),
              axis.title.x=element_text(colour="black",size=input$axis2,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2),
              axis.title.y = element_text(colour="black",size=input$axis2,angle=90,hjust=.5,vjust=.5,face="plain",family=input$font2),
              legend.title  =element_blank(),
              legend.text = element_text(size=input$legend_size2,face="plain",family=input$font2),
              legend.position = input$legend_location2,
              legend.justification = "top")+
        scale_y_continuous(breaks = seq(-1e6, 1e6, by = input$cor_ybreaks))+
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$cor_xbreaks))
      vals2$cor_graph
      
    }
  }
  
  output$cor_graph <- renderPlot({
    withProgress(message = 'Figure is being generated...',
                 detail = '', value = 0, {
                   test_fun2()
                 })
    
    print(plotInput2())
  })
  output$Table5 <-DT::renderDataTable(escape = FALSE,filter = 'top', options=list(extensions="Buttons", scrollX = T), {
    neg1 <- -1*input$FC1
    pos1 <- input$FC1
    neg2 <- -1*input$FC2
    pos2 <- input$FC2
    dat4 <- input.data4();
    dat3 <- input.data3();
    
    validate(
      need(nrow(dat3)>0,
           error_message_val1)
    ) 
    
    validate(
      need(nrow(dat4)>0,
           error_message_val1)
    ) 
    
    
    dat5 <- merge(dat3,dat4,by="ID")
    dat_all <- mutate(dat5,
                      significance=ifelse(dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_up",
                                          ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_down", "other")))
    
    dat_all$Pvalue.x <- signif(dat_all$Pvalue.x,3)
    dat_all$logFC.x <- signif(dat_all$logFC.x,3)
    dat_all$Pvalue.y <- signif(dat_all$Pvalue.y,3)
    dat_all$logFC.y <- signif(dat_all$logFC.y,3)
    dat_all <- dat_all[order(dat_all$Pvalue.x),]
    
    names(dat_all) <- gsub('.x','',names(dat_all))
    names(dat_all)[2:3] <- paste(names(dat_all)[2:3],input$expression_x,sep="_")
    names(dat_all) <- gsub('.y','',names(dat_all))
    names(dat_all)[4:5] <- paste(names(dat_all)[4:5],input$expression_y,sep="_")
    
    datatable(dat_all, escape = FALSE,filter = 'top', options=list(extensions="Buttons", scrollX = T), selection = 'none') %>% 
      formatStyle(
        paste("logFC",input$expression_x,sep="_"),
        backgroundColor = styleInterval(c(-input$FC,input$FC), c('#abd7eb', '#D2D2CF',"#ff6961")),
        color = styleInterval(c(-input$FC,input$FC), c('#181A18', '#181A18', '#181A18')),
        fontWeight = styleInterval(c(-input$FC,input$FC), c('bold', 'normal','bold'))) %>% 
      formatStyle(
        paste("Pvalue",input$expression_x,sep="_"),
        backgroundColor = styleInterval(c(input$Pvalue), c('#181A18', '#D2D2CF')),
        color = styleInterval(c(input$Pvalue), c('#d99058',  '#181A18')),
        fontWeight = styleInterval(input$Pvalue, c('bold', 'normal'))) %>% 
      formatStyle(
        paste("logFC",input$expression_y,sep="_"),
        backgroundColor = styleInterval(c(-input$FC,input$FC), c('#abd7eb', '#D2D2CF',"#ff6961")),
        color = styleInterval(c(-input$FC,input$FC), c('#181A18', '#181A18', '#181A18')),
        fontWeight = styleInterval(c(-input$FC,input$FC), c('bold', 'normal','bold')))%>% 
      formatStyle(
        paste("Pvalue",input$expression_y,sep="_"),
        backgroundColor = styleInterval(c(input$Pvalue), c('#181A18', '#D2D2CF')),
        color = styleInterval(c(input$Pvalue), c('#d99058',  '#181A18')),
        fontWeight = styleInterval(input$Pvalue, c('bold', 'normal'))) 
    
    
    
  }) 
  output$text1 <- renderPrint({
    dat4 <- input.data4();
    dat3 <- input.data3();
    validate(
      need(nrow(dat3)>0,
           error_message_val1)
    ) 
    
    validate(
      need(nrow(dat4)>0,
           error_message_val1)
    ) 
    
    data3 <- as.numeric(dim(dat3)[1])
    data4 <- as.numeric(dim(dat4)[1]) 
    dat5 <- merge(dat3,dat4,by="ID")
    data5 <- as.numeric(dim(dat5)[1]) 
    cat(noquote(paste("There are ", data3," from ", input$expression_x, " and ", data4," from ", input$expression_y, "with", data5, "common to both")))
  })
  dataExpTable2 <- reactive({
    neg1 <- -1*input$FC1
    pos1 <- input$FC1
    neg2 <- -1*input$FC2
    pos2 <- input$FC2
    dat4 <- input.data4();
    dat3 <- input.data3();
    
    validate(
      need(nrow(dat3)>0,
           error_message_val1)
    ) 
    
    validate(
      need(nrow(dat4)>0,
           error_message_val1)
    ) 
    
    
    
    
    dat5 <- merge(dat3,dat4,by="ID")
    dat_all <- mutate(dat5,
                      significance=ifelse(dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_up",
                                          ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_down", "other")))
    dat.sig <- subset(dat_all, dat_all$significance== "both_sig_up" | dat_all$significance== "both_sig_down")
    
    
    names(dat_all) <- gsub('.x','',names(dat_all))
    names(dat_all)[2:3] <- paste(names(dat_all)[2:3],input$expression_x,sep="_")
    names(dat_all) <- gsub('.y','',names(dat_all))
    names(dat_all)[4:5] <- paste(names(dat_all)[4:5],input$expression_y,sep="_")
    dat.sig <- subset(dat_all, dat_all$significance== "both_sig_up" | dat_all$significance== "both_sig_down")
    dat.sig
    
  })
  output$cor_test <- renderPrint({
    
    neg1 <- -1*input$FC1
    pos1 <- input$FC1
    neg2 <- -1*input$FC2
    pos2 <- input$FC2
    dat4 <- input.data4();
    dat3 <- input.data3();
    
    validate(
      need(nrow(dat3)>0,
           error_message_val1)
    ) 
    
    validate(
      need(nrow(dat4)>0,
           error_message_val1)
    ) 
    
    
    
    
    dat5 <- merge(dat3,dat4,by="ID")
    dat_all <- mutate(dat5,
                      significance=ifelse(dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_up",
                                          ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_down", "other")))
    pval <- as.list(cor.test(dat_all$logFC.x,dat_all$logFC.y)$p.value)
    
    R_value <- as.list(cor.test(dat_all$logFC.x,dat_all$logFC.y)$estimate)
    CI.95 <- as.list(cor.test(dat_all$logFC.x,dat_all$logFC.y)$conf.int)
    round(CI.95[[1]],3)
    round(CI.95[[2]],3)
    cat(noquote(paste("The overall Pearsons correlation (R) is ", round(R_value[[1]],3)," with a 95% confidence interval from ", round(CI.95[[1]],3), " to ",round(CI.95[[2]],3)," and a p-value of ", signif(pval[[1]],3),sep="")))
    
  })
  
  output$cor_test_sig <- renderPrint({
    
    neg1 <- -1*input$FC1
    pos1 <- input$FC1
    neg2 <- -1*input$FC2
    pos2 <- input$FC2
    dat4 <- input.data4();
    dat3 <- input.data3();
    
    validate(
      need(nrow(dat3)>0,
           error_message_val1)
    ) 
    
    validate(
      need(nrow(dat4)>0,
           error_message_val1)
    ) 
    
    dat5 <- merge(dat3,dat4,by="ID")
    dat_all <- mutate(dat5,
                      significance=ifelse(dat5$logFC.x>pos1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y>pos2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_up",
                                          ifelse(dat5$logFC.x<neg1 & dat5$Pvalue.x<input$Pvalue1 & dat5$logFC.y<neg2 & dat5$Pvalue.x<input$Pvalue2, "both_sig_down", "other")))
    
    dat.sig <- subset(dat_all, dat_all$significance== "both_sig_up" | dat_all$significance== "both_sig_down")
    
    pval <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$p.value)
    
    R_value <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$estimate)
    CI.95 <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$conf.int)
    round(CI.95[[1]],3)
    round(CI.95[[2]],3)
    cat(noquote(paste("The overlap of significant values Pearsons correlation (R) is ", round(R_value[[1]],3)," with a 95% confidence interval from ", round(CI.95[[1]],3), " to ",round(CI.95[[2]],3)," and a p-value of ", signif(pval[[1]],3),sep="")))
    
    
    
  })
  plotInput3 <- function() {
    
    dat3 <- input.data3();
    dat4 <- input.data4();
    dat3 <- as.data.frame(dat3)
    dat4 <- as.data.frame(dat4)
    
    neg1 <- -1*input$FC1
    pos1 <- input$FC1
    
    neg2 <- -1*input$FC2
    pos2 <- input$FC2
    
    validate(
      need(nrow(dat3)>0,
           error_message_val3)
    ) 
    
    validate(
      need(nrow(dat4)>0,
           error_message_val3)
    ) 
    
    
    
    one <- merge(dat3,dat4,by="ID")
    a2 <- subset(one,one$Pvalue.x<input$Pvalue1 & one$Pvalue.y<input$Pvalue2)
    a <- subset(a2, abs(a2$logFC.x)>pos1 & abs(a2$logFC.y)>pos2)
    a$direction <- ifelse(a$logFC.x>pos1 & a$logFC.y>pos2,"1_Sig_up_both",
                          ifelse(a$logFC.x<neg1 & a$logFC.y<neg2,"2_sig_down_both","3_sig_opposite"))
    
    a$direction2 <- ifelse(a$logFC.x>pos1 & a$logFC.y>pos2 | a$logFC.x<neg1 & a$logFC.y<neg2,"1_sig_same","2_sig_opposite")
    
    a<- a[order(a$direction, decreasing = F),]
    a$V1 <- 1:dim(a)[1]
    
    df_logFC <- melt(a[,c("ID","V1","direction","logFC.x","logFC.y")],id.vars = c(1:3))
    
    if (input$direction == "all" ) {
      vals3$logFC_direction <- ggplot(df_logFC,aes(y =reorder(ID,-V1), x = value, fill=variable)) +
        geom_bar(stat="identity", position = "dodge") +
        
        # set overall appearance of the plot
        theme_bw()  +
        theme(legend.title = element_blank(),
              legend.position = "bottom") +
        scale_fill_manual(values=c(input$col5,input$col6),labels=c(input$expression_x,input$expression_y))+
        labs(y="",
             x=expression(Log[2]~"FC"),
             title="") +
        # Add a line showing the alpha = 0.01 level
        theme(
          axis.text.y = element_text(colour="black",size=input$bar_text_y,family=input$font2),
          axis.text.x = element_text(colour="black",size=input$bar_text_x,family=input$font2),
          axis.title.x = element_text(colour="black",size=input$axis3,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2)
        ) +
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$bar_xbreaks))
      
      
      vals3$logFC_direction
      
    }
    else if (input$direction == "up") {
      
      df_logFC_up <- subset(df_logFC,df_logFC$direction=="1_Sig_up_both")
      
      vals3$logFC_direction <- ggplot(df_logFC_up,aes(y =reorder(ID,-V1), x = value, fill=variable)) +
        geom_bar(stat="identity", position = "dodge") +
        
        # set overall appearance of the plot
        theme_bw()  +
        theme(legend.title = element_blank(),
              legend.position = "bottom") +
        scale_fill_manual(values=c(input$col5,input$col6),labels=c(input$expression_x,input$expression_y))+
        labs(y="",
             x=expression(Log[2]~"FC"),
             title="") +
        # Add a line showing the alpha = 0.01 level
        theme(
          axis.text.y = element_text(colour="black",size=input$bar_text_y,family=input$font2),
          axis.text.x = element_text(colour="black",size=input$bar_text_x,family=input$font2),
          axis.title.x = element_text(colour="black",size=input$axis3,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2)
        ) +
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$bar_xbreaks))
      
      vals3$logFC_direction
      
      
    }
    else if (input$direction == "down") {
      
      df_logFC_down <- subset(df_logFC,df_logFC$direction=="2_sig_down_both")
      
      vals3$logFC_direction <- ggplot(df_logFC_down,aes(y =reorder(ID,-V1), x = value, fill=variable)) +
        geom_bar(stat="identity", position = "dodge") +
        
        # set overall appearance of the plot
        theme_bw()  +
        theme(legend.title = element_blank(),
              legend.position = "bottom") +
        scale_fill_manual(values=c(input$col5,input$col6),labels=c(input$expression_x,input$expression_y))+
        labs(y="",
             x=expression(Log[2]~"FC"),
             title="") +
        # Add a line showing the alpha = 0.01 level
        theme(
          axis.text.y = element_text(colour="black",size=input$bar_text_y,family=input$font2),
          axis.text.x = element_text(colour="black",size=input$bar_text_x,family=input$font2),
          axis.title.x = element_text(colour="black",size=input$axis3,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2)
        ) +
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$bar_xbreaks))
      
      vals3$logFC_direction
      
      
    }
    else if (input$direction == "same") {
      
      df_logFC2 <- melt(a[,c("ID","V1","direction2","logFC.x","logFC.y")],id.vars = c(1:3))
      
      df_logFC_same <- subset(df_logFC2,df_logFC2$direction2=="1_sig_same")
      
      vals3$logFC_direction <- ggplot(df_logFC_same,aes(y =reorder(ID,-V1), x = value, fill=variable)) +
        geom_bar(stat="identity", position = "dodge") +
        
        # set overall appearance of the plot
        theme_bw()  +
        theme(legend.title = element_blank(),
              legend.position = "bottom") +
        scale_fill_manual(values=c(input$col5,input$col6),labels=c(input$expression_x,input$expression_y))+
        labs(y="",
             x=expression(Log[2]~"FC"),
             title="") +
        # Add a line showing the alpha = 0.01 level
        theme(
          axis.text.y = element_text(colour="black",size=input$bar_text_y,family=input$font2),
          axis.text.x = element_text(colour="black",size=input$bar_text_x,family=input$font2),
          axis.title.x = element_text(colour="black",size=input$axis3,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2)
        ) +
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$bar_xbreaks))
      
      vals3$logFC_direction
      
    }
    else {
      df_logFC_opposite <- subset(df_logFC,df_logFC$direction=="3_sig_opposite")
      vals3$logFC_direction <- ggplot(df_logFC_opposite,aes(y =reorder(ID,-V1), x = value, fill=variable)) +
        geom_bar(stat="identity") +
        # set overall appearance of the plot
        theme_bw()  +
        theme(legend.title = element_blank(),
              legend.position = "bottom") +
        scale_fill_manual(values=c(input$col5,input$col6),labels=c(input$expression_x,input$expression_y))+
        labs(y="",
             x=expression(Log[2]~"FC"),
             title="") +
        # Add a line showing the alpha = 0.01 level
        theme(
          axis.text.y = element_text(colour="black",size=input$bar_text_y,family=input$font2),
          axis.text.x = element_text(colour="black",size=input$bar_text_x,family=input$font2),
          axis.title.x = element_text(colour="black",size=input$axis3,angle=0,hjust=.5,vjust=.5,face="plain",family=input$font2)
        ) +
        scale_x_continuous(breaks = seq(-1e6, 1e6, by = input$bar_xbreaks))
      vals3$logFC_direction
    }
    
  }
  output$logFC_direction <- renderPlot({
    withProgress(message = 'Figure is being generated...',
                 detail = '', value = 0, {
                   test_fun2()
                 })
    
    
    print(plotInput3())
  })
  
  
  output$downloadTABLE2 <- downloadHandler(
    filename = function(){
      paste("correlation_",gsub("-", ".", Sys.Date()), " - ","Filtered ",".csv", sep = "")
    },
    content = function(file){
      write.csv(dataExpTable2(),file, row.names = FALSE)
    })
  
  
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("correlation_",input$title2, gsub("/", "-", x), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width=input$cor_width,height=input$cor_height, onefile = FALSE) # open the pdf device
      grid.arrange(vals2$cor_graph)
      dev.off()},
    
    contentType = "application/pdf"
    
  )
  
  output$downloadPlotPNG2 <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("correlation_",input$title2, gsub("/", "-", x), ".png", sep = "")
    },
    content = function(file) {
      
      png(file, width = input$cor_width_png, height = input$cor_height_png, res = input$cor_resolution_PNG)
      grid.arrange(vals2$cor_graph)
      dev.off()},
    
    contentType = "application/png" # MIME type of the image
    
  )
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("bar_", gsub("/", "-", x), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width=input$bar_width,height=input$bar_height, onefile = FALSE) # open the pdf device
      grid.arrange(vals3$logFC_direction)
      dev.off()},
    
    contentType = "application/pdf"
    
  )
  
  output$downloadPlotPNG3 <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("bar_", gsub("/", "-", x), ".png", sep = "")
    },
    content = function(file) {
      
      png(file, width = input$bar_width_png, height = input$bar_height_png, res = input$bar_resolution_PNG)
      grid.arrange(vals3$logFC_direction)
      dev.off()},
    
    contentType = "application/png" # MIME type of the image
    
  )
  
}

shinyApp(ui, server)

