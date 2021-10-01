
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
require("colourpicker", lib.loc = "local.lib/")
require("circlize")
require("ComplexHeatmap")
require("circlize", lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/4.1")
require("ComplexHeatmap", lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/4.1")

# install.packages("circlize",lib = "../ggVolcanoR/local.lib/", dependencies = T)
# install.packages("ComplexHeatmap",lib = "../ggVolcanoR/local.lib/", dependencies = T)



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



gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

error_message_1 <- c("No data found"," ","types of errors","    (1) Symbol doesn't match; in test-data MAR10 has been converted to Mar-10 in excel","    (2) Not using a human database","    (3) Not a characterised protein","    (4) Was added to the database after June 2021")

ID.conversion <- read.csv("ID/uniprot.d.anno.210611.csv",row.names = 1)
head(ID.conversion)
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
species <- c("BOVIN","CHICK","ECOLI","HORSE","HUMAN","MAIZE","MOUSE","PEA", "PIG","RABIT","RAT","SHEEP","SOYBN","TOBAC","WHEAT","YEAST","Other")
lab <- c("significant up","significant down","non-significant")
names.conversion <- names(ID.conversion)[c(1:3,5)]
name.conversion.required <- c(FALSE,TRUE) 
label3 <- c(TRUE,FALSE)
direction <- c("all.sig","up","down","same","opposite","own list")
sort_direction <- c(TRUE,FALSE)

error_message_val1 <- "No data found"
error_message_val2 <- "Suggest uploading file\nheaders=ID, logFC, Pvalue"
error_message_val3 <- "No data found\n \nSuggest uploading file\nheaders=ID, logFC, Pvalue"
error_message_val4 <- "no own list found\n \nSuggest uploading file\nheaders=ID"
# user interface  ----
style.volcano.type <- c("default","all.datapoints","up.ID","down.ID","selected.ID")

style.cor.type <- c("default","Labelled","Regression.line","labelled.Regression.line")

ui <- navbarPage("ggVolcanoR", position = "fixed-top",collapsible = TRUE,
                 # UI Volcano plot ----
                 
                 tabPanel("Volcano plot (Single-group)",
                          sidebarLayout(
                            sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 900px; position:relative;", width=3,
                                         tags$style(type="text/css", "body {padding-top: 70px; padding-left: 10px;}"),
                                         tags$head(tags$style(HTML(".shiny-notification {position:fixed;top: 50%;left: 30%;right: 30%;}"))),
                                         tags$head(tags$style(HTML('.progress-bar {background-color: blue;}'))),
                                         selectInput("dataset_parameters","Select preset or user uploaded parameters",choices = c("preset","user-uploaded")),
                                         downloadButton("downloadTABLE.parameters","download parameters guide"),
                                         fileInput('file.style', 'Upload parameters',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         fluidRow(
                                           column(6,radioButtons('sep.style', 'Separator', c( Tab='\t', Comma=','), ',')),
                                           column(6,radioButtons('quote.style', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'))
                                         ),
                                         selectInput("user.defined","Types of preset parameters",choices = style.volcano.type),
                                         selectInput("dataset", "Choose a dataset:", choices = c("test-data", "own")),
                                         fileInput('file1', 'ID, logFC, Pvalue',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         fluidRow(
                                           column(6,radioButtons('sep', 'Separator', c( Tab='\t', Comma=','), ',')),
                                           column(6,radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'))
                                         ),
                                         
                                         tags$hr(),
                                         h4("Type of graph"),
                                         p("There are 5 labelling options: none, both, up, down or own list"),
                                         uiOutput("label.graph.type"),
                                         fileInput('file2', 'Choose selected gene file (.csv)',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         h4("Select font for graph"),
                                         uiOutput("font.type"),
                                         h4("Cut-offs"),
                                         uiOutput("cut.offs"),
                                         h4("Axis parameters"),
                                         uiOutput("axis.parameters"),
                                         uiOutput("axis.parameters2"),
                                         h4("Point colour, size, shape and transparancy"),
                                         uiOutput("up.parameters"),
                                         uiOutput("down.parameters"),
                                         uiOutput("transparancy1"),
                                         uiOutput("NS.parameters"),
                                         uiOutput("transparancy2"),
                                         p(" "),
                                         h4("Selected points labels, colour and shape"),
                                         uiOutput("shape.size.selected"),
                                         uiOutput("transparancy3"),
                                         uiOutput("labelled.parameters"),
                                         h4("Label parameters"),
                                         uiOutput("label.range"),
                                         uiOutput("dist.size.label"),
                                         
                                         
                                         h4("Legend parameters"),
                                         uiOutput("legend.parameters"),
                            ),
                            mainPanel(tabsetPanel(
                              tabPanel("Volcano plot", 
                                       uiOutput("title.volc"),
                                       textOutput("number_of_points"),
                                       textOutput("sig_values_test"),
                                       plotOutput("ggplot",height = "600px"),
                                       h4("Exporting the ggVolcanoR plot"),
                                       fluidRow(
                                         
                                         column(3,numericInput("width", "Width of PDF", value=10)),
                                         column(3,numericInput("height", "Height of PDF", value=8)),
                                         column(3),
                                         column(3,style = "margin-top: 25px;",downloadButton('downloadPlot','Download PDF'))
                                       ),
                                       
                                       fluidRow(
                                         column(3,numericInput("width_png","Width of PNG", value = 1600)),
                                         column(3,numericInput("height_png","Height of PNG", value = 1200)),
                                         column(3,numericInput("resolution_PNG","Resolution of PNG", value = 144)),
                                         column(3,style = "margin-top: 25px;",downloadButton('downloadPlotPNG','Download PNG'))
                                       ),
                                      
                                       
                              ),
                              tabPanel("Volcano plot (selected colours)",
                                       fluidRow(column(12, textInput("string.data3","list of selected points","CD74, TAP2, HLA-E, STAT1, WARS, ICAM1, TAP1", width = "1200px") )),
                                       textInput(inputId = "title3", 
                                                 label = "",
                                                 value = "Volcano plot: selected colour of points"),
                                       fluidRow(column(2,selectInput( "select.ggVolc_colour.choise",label = h5("colour"),choices = c("default","grey")))),
                                       fluidRow(column(3,
                                                       wellPanel(id = "tPanel222",style = "overflow-y:scroll; max-height: 600px",
                                                                 uiOutput('myPanel.ggVolCol'))),
                                                column(9, plotOutput("col.ggplot",height = "600px"))),
                                       fluidRow(
                                         
                                         column(3,numericInput("width_col", "Width of PDF", value=10)),
                                         column(3,numericInput("height_col", "Height of PDF", value=8)),
                                         column(3),
                                         column(3,style = "margin-top: 25px;",downloadButton('downloadPlot_col','Download PDF'))
                                       ),
                                       
                                       fluidRow(
                                         column(3,numericInput("width_png_col","Width of PNG", value = 1600)),
                                         column(3,numericInput("height_png_col","Height of PNG", value = 1200)),
                                         column(3,numericInput("resolution_PNG_col","Resolution of PNG", value = 144)),
                                         column(3,style = "margin-top: 25px;",downloadButton('downloadPlotPNG_col','Download PNG'))
                                       ),
                                       
                                       
                                       ),
                              tabPanel("Table with links", 
                                       selectInput("data.name.type","select initial ID type",choices = c("Symbol","Ensembl","human-uniprot","non-human")),
                                       selectInput("species", label = "Select relevant species",species,selected = "HUMAN"),
                                       
                                       div(DT::dataTableOutput("myoutput",height="600px"))),
                              
                              tabPanel("Summary table",DT::dataTableOutput("summary_table",height="300px"),
                                       textInput("group.name","name of group",value = "Proteomics"),
                                       
                                       selectInput(inputId = "export", 
                                                   label = "Filtered lists for download", 
                                                   choices = filtered_table, 
                                                   selected = filtered_table[1]),
                                       downloadButton("downloadTABLE", "Download filtered Table"))
                              
                            ))
                          )
                 ),
                 # correlation graphs -----
                 tabPanel("Correlation plot (Two-group)",
                          sidebarLayout(
                            sidebarPanel(id = "tPanel2",style = "overflow-y:scroll; max-height: 1000px; position:relative;", width=3,
                                         h4("Correlation plot parameters"),
                                         selectInput("dataset_parameters.cor","Select preset or user uploaded parameters",choices = c("preset","user-uploaded")),
                                         downloadButton("downloadTABLE.parameters.cor","Download parameter guide"),
                                         fileInput('file.style.cor', 'Upload parameters',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         selectInput("user.defined.cor","Types of parameters",choices = style.cor.type),
                                         
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
                                         fileInput('file6', 'Choose selected gene file (.csv)',
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                         h4("font type"),
                                        uiOutput("font_cor"),
                                         h4("Cut-offs"),
                                        uiOutput("cut.off.cor"),
                                         h4("Axis text size"),
                                        uiOutput("axis.text.size.cor"),
                                         h4("Point parameters"),
                                      uiOutput("point.parameter.cor1"),
                                      uiOutput("point.parameter.cor2"),
                                      uiOutput("point.parameter.cor3"),
                                      uiOutput("point.parameter.cor4"),
                                         h4("Axis tick marks"),
                                        uiOutput("axis.tick.marks"),
                                      h4("Legend parameters"),
                                      uiOutput("legend.par.cor")
                                        

                            ),
                            # main panl correlation -----
                            mainPanel(tabsetPanel(
                              tabPanel("Correlation graph", 
                                       
                                       uiOutput("axis.label.cor"),
                                       h5("Correlation line parameters"),
                                       
                                       uiOutput("correlation.line"),
                                       h5("Label options"),
                                       uiOutput("labels.cor"),
                                       uiOutput("labels.cor2"),
                                       p("If order of file is ID, logFC, P-value sort column to sort by logFC by 2 and 4 or p-value by 3 and 5"),
                                       
                                       plotOutput("cor_graph",height = "600px"),
                                       p(" "),
  
                                       fluidRow(
                                         column(3,numericInput("cor_width", "Width of PDF", value=10)),
                                         column(3,numericInput("cor_height", "Height of PDF", value=8)),
                                         column(3),
                                         column(3,style = "margin-top: 25px;",downloadButton('downloadPlot2','Download PDF'))
                                         
                                       ),
                                       
                                       fluidRow(
                                         column(3,numericInput("cor_width_png","Width of PNG", value = 1600)),
                                         column(3,numericInput("cor_height_png","Height of PNG", value = 1200)),
                                         column(3,numericInput("cor_resolution_PNG","Resolution of PNG", value = 144)),
                                         column(3,style = "margin-top: 25px;",downloadButton('downloadPlotPNG2','Download PNG'))
                                       ),
                                       
                                       
                                       
                            ## other cor -----           
                                       
                              ),
                              tabPanel("Correlation pearson statistics",
                                       h5("Correlation of all data points"),
                                       textOutput("cor_test"),
                                       h5("Correlation of all in positive direction"),
                                       textOutput("cor_test_sig"),
                                       h5("Correlation of all in negative direction"),
                                       textOutput("cor_test_sig_neg")
                                       ),
                              tabPanel("Correlation table",DT::dataTableOutput("Table5"),
                                       p(" "),
                                       h5("Download the data that has significant overlap"),
                                       downloadButton("downloadTABLE2", "Filtered Table")
                              ),
                              tabPanel("Bar graph", 
                                       
                                       fluidRow(h4("Bar graph parameters"),
                                                
                                                column(3, selectInput("direction","Select which direction to show",
                                                                      choices = direction,
                                                                      selected = "all")),
                                                column(3,numericInput("bar_xbreaks","x-axis tick marks",value = 1)),
                                                column(3,textInput(inputId = "col5", label = "Dataset 1 (x-axis)",value = "lightgreen")),
                                                column(3,textInput(inputId = "col6", label = "Dataset 2 (y-axis)",value = "grey"))
                                                
                                       ),
                                       fluidRow(
                                         column(4,numericInput("axis3", "x-axis text size",value=30,)),
                                         column(4,numericInput("bar_text_y", "Size of ID in bar graph",value=5)),
                                         column(4,numericInput("bar_text_x", "Size of number in bar graph", value=12))
                                       ),
                                       
                                       
                                       plotOutput("logFC_direction", height = "600px"),
                                       
                                       fluidRow(
                                         column(3,numericInput("bar_width", "Width of PDF", value=10)),
                                         column(3,numericInput("bar_height", "Height of PDF", value=8)),
                                         column(3),
                                         column(3,style = "margin-top: 25px;",downloadButton('downloadPlot3','Download PDF'))
                                       ),
                                       
                                       fluidRow(
                                         column(3,numericInput("bar_width_png","Width of PND", value = 1600)),
                                         column(3,numericInput("bar_height_png","Height of PNG", value = 1200)),
                                         column(3,numericInput("bar_resolution_PNG","Resolution of PNG", value = 144)),
                                         column(3,style = "margin-top: 25px;",downloadButton('downloadPlotPNG3','Download PNG'))
                                       ),

                                       
                              )
                              
                            )
                            )
                          )
                 ),
                 

# heatmap and upset -------------------------------------------------------

                 
                tabPanel("Heatmap & Upset plot (Multi-group)",
                         sidebarLayout(
                          sidebarPanel(id = "tPanel4",style = "overflow-y:scroll; max-height: 800px; position:relative;", width=3,
                                      h4("Heatmap & Upset plot"),
                                      selectInput("dataset.upset.heatmap", "Choose a dataset:", choices = c("test-data", "own")),
                                      fileInput('file.hm', 'ID, logFC, Pvalue, group, group.direction (.csv)',
                                                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                      selectInput('font.hm','Font type',choices = fonts, selected = fonts[2]),
                                      downloadButton('downloadTABLE.hm','Download Heatmap table'),
                                      p(" "),
                                      downloadButton('downloadTABLE.upset','Download Upset table')


                       ),
                      mainPanel(
                        tabsetPanel(
                                   tabPanel("Heatmap",
                                            fluidRow(column(3, selectInput("hm.type","select type of filter",choices = c("all",">0 in all"))),
                                                     column(3, numericInput("min.hm","range (min)",value = 1)),
                                                     column(3, numericInput("max.hm","range (max)",value = 20)),
                                                     column(3, numericInput("heatmap.font.size","ID size",value = 12))),
                                            
                                            plotOutput("heatmap.plot", height = "1200px"),
                                            
                                            h4(" "),
                                            h4("Download Heatmap plot"),
                                            
                                            fluidRow(
                                              
                                              column(3,numericInput("width_heatmap", "Width of PDF", value=10)),
                                              column(3,numericInput("height_heatmap", "Height of PDF", value=16)),
                                              column(3,),
                                              column(3,style = "margin-top: 25px;",downloadButton('downloadPlot_heatmap','Download PDF'))
                                            ),
  
                                            fluidRow(
                                              column(3,numericInput("width_png_heatmap","Width of PNG", value = 1600)),
                                              column(3,numericInput("height_png_heatmap","Height of PNG", value = 2400)),
                                              column(3,numericInput("resolution_PNG_heatmap","Resolution of PNG", value = 144)),
                                              column(3,style = "margin-top: 25px;",downloadButton('downloadPlotPNG_heatmap','Download PNG'))
                                            ),
                                            
                                            
                                            ),
                                   tabPanel("Upset plot",
                                            selectInput("upset.group.select",label = h5("Select group column (max 31 groups)"), choices = "",selected= ""),
                                            numericInput("font.size.anno.upset","Size of numeric annotation",value=12),
                                            plotOutput("upset.plot", height = "600px"),
                                            
                                            h4(" "),
                                            h4("Download Upset plot"),
                                            fluidRow(
                                              
                                              column(3,numericInput("width_upset", "Width of PDF", value=12)),
                                              column(3,numericInput("height_upset", "Height of PDF", value=8)),
                                              column(3,),
                                              column(3,style = "margin-top: 25px;",downloadButton('downloadPlot_upset','Download PDF'))
                                            ),
                                            
                                            fluidRow(
                                              column(3,numericInput("width_png_upset","Width of PNG", value = 1920)),
                                              column(3,numericInput("height_png_upset","Height of PNG", value = 1200)),
                                              column(3,numericInput("resolution_PNG_upset","Resolution of PNG", value = 144)),
                                              column(3,style = "margin-top: 25px;",downloadButton('downloadPlotPNG_upset','Download PNG'))
                                            ),
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
                            
                            tabPanel("Correlation graph Read Me file",
                                     fluidRow(includeMarkdown("README_Correlationplot.md")
                                              
                                     )
                            ),
                            
                            tabPanel("Session info", 
                                     tabPanel("Session info", verbatimTextOutput("sessionInfo"))
                            )
                 ))
# sever -----
# preselected styles 
server  <- function(input, output, session) {
  # style parameters -----
  input.data_parameters <- reactive({switch(input$dataset_parameters,"preset" = test.data_parameters(),"user-uploaded" = own.data_parameters())})
  test.data_parameters <- reactive({
    dataframe = read.csv("test-data/test-parameters.csv") })
  own.data_parameters <- reactive({
    inFile.style <- input$file.style 
    if (is.null(inFile.style)) return(NULL)
    
    else {
      dataframe <- read.csv(
        inFile.style$datapath,
        header=TRUE,
        sep=input$sep.style,
        quote=input$quote.style)}
  })
  
  table.parameters <- function (){
    df <- input.data_parameters()
    df
  }
  
  output$downloadTABLE.parameters <- downloadHandler(
    filename = function(){
      paste("preset-style",".csv", sep = "")
    },
    content = function(file){
      write.csv(test.data_parameters(),file, row.names = FALSE)
    }
  )
  
  values.cut.off <- function(){
    
    df <- input.data_parameters()
    
    if (input$user.defined == "all.datapoints") {
      
      subset(df,df$style.type=="all.datapoints")
      
    }
    else if (input$user.defined == "up.ID") {
      subset(df,df$style.type=="up.ID")
      
      
    }
    else if (input$user.defined == "down.ID") {
      subset(df,df$style.type=="down.ID")
      
      
    }
    else if (input$user.defined == "selected.ID") {
      subset(df,df$style.type=="selected.ID")
    }
    else {
      subset(df,df$style.type=="default")
    }
  }

  
  #  output$x <- renderUI({
  # df <- values.cut.off()
  #})
  output$font.type <- renderUI({
    df <- values.cut.off()
    selectInput('font','Font type',choices = fonts, selected = fonts[df$font.type])
    
  })
  output$label.graph.type <- renderUI({
    
    df <- values.cut.off()
    
    selectInput('selected', 'Type of output', 
                choices = selected_present, 
                selected= selected_present[df$label.type])
    
  })
  output$cut.offs <- renderUI({
    
    df <- values.cut.off()
    
    fluidRow(
      column(6,numericInput("Pvalue", "p-value", value=df$Pvalue)),
      column(6,numericInput("FC", "Absolute logFC", value=df$FC))
    )
    
  })
  output$axis.parameters <- renderUI({
    df <- values.cut.off()
    fluidRow(
      column(12, textInput(inputId = "sig_lines", label = "Significance lines",value = df$sig.col.line)),
      column(12,  textInput(inputId = "expression_y2", 
                            label = "Y-axis label",
                            value = df$x.axis.lab)),
      column(12, numericInput("axis", "Axis label text size", min=0, value=df$axis.text.size)),
      column(12,  numericInput("axis_text", "Axis numeric text size", min=0, value=df$axis.numeric.size)),
     
      
     
      column(4,numericInput("xlow","x-axis lower range",value = df$x.min)),
      column(4,numericInput("xhigh","x-axis upper range",value = df$x.max)),
      column(4,numericInput("xbreaks","x-axis tick marks",value = df$x.tick.marks)),
      
    )
    
    
    
    
  })
  
  output$axis.parameters2 <- renderUI({
    df <- values.cut.off()
    fluidRow(
      column(4,numericInput("yhigh","y-axis upper range",value = df$y.max)),
      column(4,numericInput("ybreaks","y-axis tick marks",value = df$y.tick.marks)),
      
    )
    
    
    
    
  })
  
  
  output$up.parameters <- renderUI({
    df <- values.cut.off()
    
    fluidRow(
      column(4,textInput(inputId = "up", label = "Colour up",value = df$up.colour)),
      column(4,numericInput("shape1.1","Shape of up",value = df$up.symbol)),
      column(4, numericInput("size1.1","Size of up",value = df$up.size))
    )
    
  })
  output$down.parameters <- renderUI({
    
    df <- values.cut.off()
    
    fluidRow(
      column(4,textInput(inputId = "down",  label = "Colour down", value = df$down.colour)),
      column(4,numericInput("shape2","Shape of down",value = df$down.symbol)),
      column(4,numericInput("size2","Size of down",value = df$down.size)),
      
    )
    
  })
  output$transparancy1 <- renderUI({
    df <- values.cut.off()
    
    sliderInput("alpha2", "Transparency of up and down", min=0, max=1, value=df$up.down.transparancy,step = 0.01)
  })
  output$NS.parameters <- renderUI({
    
    df <- values.cut.off()
    fluidRow(
      column(4,textInput(inputId = "NS", label = "Colour of non-significant",value = df$NS.colour)),
      column(4,numericInput("shape3","Shape of non-significant",value = df$NS.shape)),
      column(4,numericInput("size3","Size of non-significant",value = df$NS.size))
    )
  })
  output$transparancy2 <- renderUI({
    df <- values.cut.off()
    sliderInput("alpha3", "Transparency of non-significant", min=0, max=1, value=df$NS.transparancy,step = 0.01)
  })
  output$shape.size.selected <- renderUI({
    df <- values.cut.off()
    
    fluidRow(
      column(4,numericInput("shape1","Shape of selected",value = df$shape.labelled)),
      column(4, numericInput("size1","Size of selected",value = df$size.labelled))
      
    )
  })
  output$transparancy3 <- renderUI({
    df <- values.cut.off()
    sliderInput("alpha1", "Transparency of selected", min=0.00, max=1, value=df$transparancy.labelled,step = 0.01)
  })
  output$labelled.parameters <- renderUI({
    df <- values.cut.off()
    fluidRow(
      column(6,textInput(inputId = "col_lab1", label = "Colour of label 1",value = df$lab1.colour)),
      column(6,selectInput(inputId = "lab1", 
                           label = "label 1", 
                           choices = lab, 
                           selected = lab[df$lab1])),
      column(6,textInput(inputId = "col_lab2", label = "Colour of label 2",value = df$lab2.colour)),
      column(6,selectInput(inputId = "lab2", 
                           label = "label 2", 
                           choices = lab, 
                           selected = lab[df$lab2])),
      column(6,textInput(inputId = "col_lab3", label = "Colour of label 3",value = df$lab3.colour)),
      column(6,selectInput(inputId = "lab3", 
                           label = "label 3", 
                           choices = lab, 
                           selected = lab[df$lab3]))
    )
    
  })
  output$label.range <- renderUI({
    df <- values.cut.off()
    
    fluidRow(
      column(6,numericInput("min", "Label range (min)", value=df$label.range.min)),
      column(6, numericInput("max", "Label range (max)", value=df$label.range.max))
    )
  })
  output$dist.size.label <- renderUI({
    df <- values.cut.off()
    fluidRow(
      column(6,numericInput("dist", "Distance of label", min=0, value=df$dist.label)), 
      column(6,numericInput("label", "Size of labels", min=0, value=df$size.label),
      ))
  })
  output$legend.parameters <- renderUI({
    df <- values.cut.off()
    
    fluidRow(
      column(4,selectInput('legend_location', 'Legend location', choices=legend_location, selected = legend_location[df$legend.location])),
      column(4,numericInput("col", "# of legend columns", value=df$legend.col )),
      column(4, numericInput("legend_size", "Legend text size", value=df$legend.size))
    )
  })
  output$title.volc <- renderUI({
    df <- values.cut.off()
    textInput(inputId = "title", 
              label = "",
              value = df$title.volcano.plot)
  })
  
  # reactive values -----
  vals <- reactiveValues(ggplot=NULL)
  vals2 <- reactiveValues(cor_graph=NULL)
  vals3 <- reactiveValues(logFC_direction=NULL)
  vals4 <- reactiveValues(savedInputs=NULL)
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
                                 size=ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$size1.1,
                                             ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$size2,input$size3)))
    # range of genes
    sub.mutateddf.gene2 <- mutate(mutateddf.gene,
                                  colour=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, "top_up",
                                                ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, "top_down",                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,"sig_up",
                                                                                                                                                                                                                                                                      ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")))),
                                  alpha=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,
                                               ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$alpha2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$alpha2,input$alpha3)))),
                                  size=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$size1,
                                              ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$size1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$size1.1,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$size2,input$size3)))),
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
        scale_shape_manual(values=c(input$shape3,input$shape2,input$shape1.1,input$shape1,input$shape1),
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
                                                ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$size1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$size1.1,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$size2,input$size3)))),
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
        scale_shape_manual(name= "legend",values=c(input$shape3,input$shape2,input$shape1.1,input$shape1),labels=c("non-significant","down-regulated","up-regulated",input$lab2)) +
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
                                                ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$size1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$size1.1,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$size2,input$size3)))),
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
        scale_shape_manual(values=c(input$shape3,input$shape2,input$shape1.1,input$shape1),labels=c("non-significant","down-regulated","up-regulated",input$lab1)) +
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
                                                    ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC>pos,input$size1.1,
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
        scale_shape_manual(values=c(input$shape3,input$shape2,input$shape1.1,input$shape1,input$shape1,input$shape1),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2,input$lab3)) +
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
        scale_shape_manual(values=c(input$shape3,input$shape2,input$shape1.1),labels=c("non-significant","down-regulated","up-regulated"))+
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
  type.of.data <- function () {
    
    dat <- input.data();
    
    
    dat <- dat[, c("ID","logFC","Pvalue")]
    
    ID.conversion <- read.csv("ID/uniprot.d.anno.210905.csv")
    head(ID.conversion)
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    rownames(dat) <- 1:dim(dat)[1]
    names(ID.conversion) <- c("Ensembl","Uniprot_human","UNIPROT","Chrom","Gene.Name","Biotype")
    head(ID.conversion)
    head(dat)
    
    
    if (input$data.name.type == "Symbol") {
      
      dat.top <- merge(dat,ID.conversion,by.x="ID",by.y="Gene.Name", all.x=T)
      dat.top[is.na(dat.top)] <- "No_ID"
      head(dat.top)
      names(dat.top) <- c("ID","logFC","Pvalue","protein_atlas","UniProt_ID","UniProt_human","chrom","Biotype")
      dat.top
      
      
    }
    
    else if (input$data.name.type == "Ensembl") {
      ID.conversion$E2 <- ID.conversion$Ensembl
      head(ID.conversion)
      dat.top <- merge(dat,ID.conversion,by.x="ID",by.y="E2", all.x=T)
      head(dat.top)
      ID.conversion <- ID.conversion[-c(7)]
      dat.top[is.na(dat.top)] <- "No_ID"
      names(dat.top) <- c("ID","logFC","Pvalue","protein_atlas","UniProt_ID","UniProt_human","chrom","Gene.Name","Biotype")
      dat.top
      
      
    }
    
    else if (input$data.name.type == "human-uniprot") {
      ID.conversion$P2 <- ID.conversion$Uniprot_human
      head(ID.conversion)
      dat.top <- merge(dat,ID.conversion,by.x="ID",by.y="P2", all.x=T)
      #dat.top <- dat.top[-c(9)]    ID.conversion <- ID.conversion[-c(7)]
      dat.top[is.na(dat.top)] <- "No_ID"
      names(dat.top) <- c("ID","logFC","Pvalue","protein_atlas","UniProt_ID","UniProt_human","chrom","Gene.Name","Biotype")
      head(dat.top)
      dat.top
      
    }
    
    
    else {
      dat <- input.data();
      dat
      
    }
    
  }
  type.of.filter <- function (){
    dat <- type.of.data()
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    rownames(dat) <- 1:dim(dat)[1]
    
    if (input$selected=="range (both directions)") {
      dat <- subset(dat, dat$Pvalue<input$Pvalue & abs(dat$logFC)>input$FC)
      dat <- dat[order(dat$Pvalue),]
      rownames(dat) <- 1:dim(dat)[1]
      top <- dat[(input$min:input$max),]
      top
    }
    
    else if (input$selected=="no labels") {
      dat
      
    }
    
    else if (input$selected=="range (up direction)") {
      dat <- subset(dat, dat$Pvalue<input$Pvalue & dat$logFC>input$FC)
      dat <- dat[order(dat$Pvalue),]
      top <- dat[(input$min:input$max),]
      top
      
    }
    
    else if (input$selected=="range (down direction)") {
      neg <- -1*input$FC
      dat <- subset(dat, dat$Pvalue<input$Pvalue & dat$logFC<neg)
      top <- dat[(input$min:input$max),]
      top
      
    }
    
    else { # selected ID
      dat2 <- input.data2();
      list <- dat2$ID
      dat <- dat[dat$ID %in% list,]
      dat
      
    }
    
  }
  output$myoutput <-DT::renderDataTable(escape = FALSE, {
    
    dat_req <- input.data();
    
    validate(
      need(nrow(dat_req)>0,
           error_message_1)
    )
    
    if (input$species == "HUMAN") {
      top <- type.of.filter()
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      top$GeneCards <- paste('<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      top$Protein_atlas <- paste('<a href=https://www.proteinatlas.org/',top$protein_atlas,' target="_blank" class="btn btn-link"','>',top$protein_atlas,'</a>',sep="")
      top$Human_Uniprot <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$UniProt_human,' target="_blank" class="btn btn-link"','>',top$UniProt_human,"</a>", sep="")
      top$UniProt <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$UniProt_ID,' target="_blank" class="btn btn-link"','>',top$UniProt_ID,'</a>',sep="")
      
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df <- df[,!names(df) %in% c("protein_atlas","UniProt_ID","UniProt_human","Gene.Name","chrom","Biotype")]
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
    
    
    else if (input$species == "Other") {
      top <- type.of.filter()
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      top$UniProt <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      
      df <- df[,!names(df) %in% c("protein_atlas","UniProt_ID","UniProt_human","Gene.Name","chrom","Biotype")]
      
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
      top <- type.of.filter()
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      top$UniProt_species <- paste('<a href=https://www.uniprot.org/uniprot/?query=',SYMBOL_list$list,' target="_blank" class="btn btn-link"','>',SYMBOL_list$list,"</a>", sep="")
      top$UniProt <- paste('<a href=https://www.uniprot.org/uniprot/?query=',top$ID,' target="_blank" class="btn btn-link"','>',top$ID,'</a>',sep="")
      
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df <- df[,!names(df) %in% c("protein_atlas","UniProt_ID","UniProt_human","Gene.Name","chrom","Biotype")]
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
    dat$group <- input$group.name
    dat$direction <- ifelse(dat$logFC>0,'up','down')
    dat$group.direction <- paste(dat$group,dat$direction,sep=".")
    
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
      both <- subset(dat, dat$Pvalue<input$Pvalue & abs(dat$logFC)>pos)
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
  
  


# selected points ---------------------------------------------------------

  col.ggVolc.data <- reactive({
    df <- input.data();
    df <- as.data.frame(df)
    your_list <- c(input$string.data3)
    your_list_df <- as.data.frame((unlist(strsplit(your_list, ', '))))
    names(your_list_df) <- "ID"
    head(your_list_df)
    your_list_df$selected <- your_list_df$ID

    dat <-  merge(df,your_list_df,by="ID",all.x=T)
    head(dat)
    dat[is.na(dat)] <- "not_selected"

    num <- unique(as.data.frame(dat$selected))
    x = dim(num)[1]-1
    col.gg <- c("lightgrey",gg_fill_hue(x))

    if (input$select.ggVolc_colour.choise == "default") {
      lapply(1:dim(num)[1], function(i) {
        colourInput(paste("col.ggVolc", i, sep="_"), paste(num[i,]), col.gg[i])
      })
    }

    else {
      lapply(1:dim(num)[1], function(i) {
        colourInput(paste("col.ggVolc", i, sep="_"), paste(num[i,]), "grey")
      })


    }



  })

  output$myPanel.ggVolCol <- renderUI({col.ggVolc.data()}) 
  
  colors.ggvolc.plot2 <- reactive({
    df <- input.data();
    df <- as.data.frame(df)
    your_list <- c(input$string.data3)
    your_list_df <- as.data.frame((unlist(strsplit(your_list, ', '))))
    names(your_list_df) <- "ID"
    head(your_list_df)
    your_list_df$selected <- your_list_df$ID
    
    dat <-  merge(df,your_list_df,by="ID",all.x=T)
    head(dat)
    dat[is.na(dat)] <- "not_selected"
    
    num <- unique(as.data.frame(dat$selected))
    
    lapply(1:dim(num)[1], function(i) {
      input[[paste("col.ggVolc", i, sep="_")]]
    })
  })
  
  vals6 <- reactiveValues(volc.plot2=NULL)
  
  plot.col.ggplot <- function () {
    
    df <- input.data();
    df <- as.data.frame(df)
    your_list <- c(input$string.data3)
    your_list_df <- as.data.frame((unlist(strsplit(your_list, ', '))))
    names(your_list_df) <- "ID"
    head(your_list_df)
    your_list_df$selected <- your_list_df$ID
    neg <- -1*input$FC
    pos <- input$FC
    
    x_lable1 <- bquote(Log[2]~Fold~Change~(.(input$expression_x)))
    y_lable1 <- bquote("-"~Log[10]~(.(input$expression_y2)))
    
    dat <-  merge(df,your_list_df,by="ID",all.x=T)
    head(dat)
    dat[is.na(dat)] <- "not_selected"
    
    num <- unique(as.data.frame(dat$selected))
    names(num) <- "V1"
    dat2 <-  merge(df,your_list_df,by="ID")
    dat$selected <- factor(dat$selected, levels = num$V1,labels = num$V1)
    dat$selected
    dat$alpha.test <- ifelse(dat$selected=="not_selected",0.25,1)
    palette.complex <- unlist(colors.ggvolc.plot2())

    vals6$volc.plot2 <- ggplot(dat,aes(x=logFC,y=-log10(Pvalue),colour = selected)) + 
      geom_point(alpha = dat$alpha.test) +
      scale_color_manual(values=palette.complex) +
      geom_text_repel(data=dat2,aes(x=logFC, 
                                    y=  -log10(Pvalue),
                                    label= ID), colour="black",
                      size=input$label,family=input$font, 
                      segment.alpha = 0.5, 
                      show.legend = F,box.padding = unit(input$dist, 'lines'), 
                      max.overlaps = Inf)+

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
           title=input$title3) 
    
      vals6$volc.plot2

  }
  
  output$col.ggplot <- renderPlot({
    withProgress(message = 'Figure is being generated...',
                 detail = '', value = 0, {
                   test_fun()
                 })
    print(plot.col.ggplot())
  })
  
  
  
  output$downloadPlot_col <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("ggVolcanoR_",input$title3, gsub("/", "-", x), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width=input$width_col,height=input$heigh_colt, onefile = FALSE) # open the pdf device
      print(plot.col.ggplot())
      dev.off()},
    
    contentType = "application/pdf"
    
  )
  
  output$downloadPlotPNG_col <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("ggVolcanoR_",input$title3, gsub("/", "-", x), ".png", sep = "")
    },
    content = function(file) {
      
      png(file, width = input$width_png_col, height = input$height_png_col, res = input$resolution_PNG_col)
      print(plot.col.ggplot())
      dev.off()},
    
    contentType = "application/png" # MIME type of the image
    
  )
  
  

  # reactive UI cor plots ---------------------------------------------------
  
  input.data_parameters.cor <- reactive({switch(input$dataset_parameters.cor,"preset" = test.data_parameters.cor(),"user-uploaded" = own.data_parameters.cor())})
  test.data_parameters.cor <- reactive({
    dataframe = read.csv("test-data/test-parameters.cor.csv") })
  own.data_parameters <- reactive({
    inFile.style.cor <- input$file.style.cor 
    if (is.null(inFile.style.cor)) return(NULL)
    
    else {
      dataframe <- read.csv(
        inFile.style.cor$datapath,
        header=TRUE)}
  })
  
  table.parameters <- function (){
    df <- input.data_parameters.cor()
    df
  }
  
  output$downloadTABLE.parameters.cor <- downloadHandler(
    filename = function(){
      paste("preset-style",".csv", sep = "")
    },
    content = function(file){
      write.csv(test.data_parameters.cor(),file, row.names = FALSE)
    }
  )
  
  values.cut.off.cor <- function(){
    
    df <- input.data_parameters.cor()
    df <- as.data.frame(df)
    
    if (input$user.defined.cor == "Labelled") {
      
      subset(df,df$style.type=="Labelled")
      
    }
    
    else if (input$user.defined.cor == "Regression.line") {
      
      subset(df,df$style.type=="Regression.line")
      
    }
    
    
    else if (input$user.defined.cor == "labelled.Regression.line") {
      
      subset(df,df$style.type=="labelled.Regression.line")
      
    }

    else {
      subset(df,df$style.type=="default")
    }
  }
  output$font_cor <- renderUI({
    df <- values.cut.off.cor()
    selectInput('font2','Font type',choices = fonts, selected = fonts[df$font.type])
    
  })
  output$cut.off.cor <- renderUI({
    
    df <- values.cut.off.cor()
    fluidRow(
      column(6,numericInput("Pvalue1", "p-value (x-axis)", value=df$Pvalue1)),
      column(6,numericInput("FC1", "Absolute logFC (x-axis)", value = df$FC1)),
      column(6,numericInput("Pvalue2", "p-value (y-axis)", value=df$Pvalue2)),
      column(6,numericInput("FC2", "Absolute logFC (y-axis)", value=df$FC1))
    )
  })
  output$axis.label.cor <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(
      
      column(3,textInput(inputId = "title2", 
                label = "Title of graph",
                value = df$title.cor)),
      column(3,textInput(inputId = "expression_x", 
                                label = "x-axis label",
                                value = df$expression_x)),
             column(3,textInput(inputId = "expression_y", 
                                label = "y-axis label",
                                value = df$expression_y))
             
    )
    
  })
  output$axis.text.size.cor <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(
      column(6,numericInput("axis2", "Axis label text size", min=0, value=df$axis.label)),
      column(6,numericInput("axis_text2", "Axis numeric text size", min=0, value=df$axis.numeric))
    )
    
  })
  output$point.parameter.cor1 <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(column(3,textInput(inputId = "col1", label = "Colour of up",value = df$colour.up)),
             column(3, numericInput("cor_shape1","Shape of up",value = df$shape.up)),
             column(3,numericInput("cor_size1","Size of up",value = df$size.up)),
             column(3,numericInput("cor_alpha1", "Transparency of up", value = df$alpha.up))
    )
    
  })
  output$point.parameter.cor2 <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(column(3,textInput(inputId = "col2", label = "Colour of down",value = df$colour.down)),
             column(3, numericInput("cor_shape2","Shape of down",value = df$shape.down)),
             column(3,numericInput("cor_size2","Size of down",value = df$size.down)),
             column(3,numericInput("cor_alpha2", "Transparency of down", value = df$alpha.down))
    )
    
  })
  output$point.parameter.cor3 <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(column(3,textInput(inputId = "col3", label = "Colour of opposite",value = df$colour.opposite)),
             column(3, numericInput("cor_shape3","Shape of opposite",value = df$shape.opposite)),
             column(3,numericInput("cor_size3","Size of opposite",value = df$size.opposite)),
             column(3,numericInput("cor_alpha3", "Transparency of opposite", value = df$alpha.opposite))
    )
    
  })
  output$point.parameter.cor4 <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(column(3,textInput(inputId = "col4", label = "Colour of other",value = df$colour.other)),
             column(3, numericInput("cor_shape4","Shape of other",value = df$shape.other)),
             column(3,numericInput("cor_size4","Size of other",value = df$size.other)),
             column(3,numericInput("cor_alpha4", "Transparency of other", value = df$alpha.other))
    )
    
  })
  output$axis.tick.marks <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(
      column(3,numericInput("cor_xbreaks","x-axis tick marks",value = df$x.tick)),
      column(3,numericInput("cor_ybreaks","y-axis tick marks",value = df$y.tick)),
      column(3,textInput(inputId = "cor_sig_lines", label = "x=0,y=0 line colour",value = df$dotted.line)),
    )
    
  })
  output$correlation.line <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(
     
      column(3,checkboxInput("reg.line", label = "Display regression line", value = df$Regression.line)),
      column(3,textInput(inputId = "linecolour", label = "Correlation line colour",value = df$colour.regression.line)),
      column(3,textInput(inputId = "CI95_fill", label = "95% CI colour",value = df$CI95.col))
    )
    
  })
  
  
  output$legend.par.cor <- renderUI({
    df <- values.cut.off.cor()
    fluidRow(
      column(4,selectInput('legend_location2', 'Legend location', choices=legend_location, selected = legend_location[df$legend.location])),
      column(4,numericInput("col.cor", "# of legend columns", value=1, step = df$no.legend.col)),
      column(4,numericInput("legend_size2", "Legend text size", min=1, max=60, value=df$legend.text.size))
    )
    
  })
  
  output$labels.cor <- renderUI({
    df <- values.cut.off.cor()
    
    fluidRow(
      column(2,checkboxInput("label3", label = "Display labels", value = df$Add.labels)),
      column(2, checkboxInput("ownlist.cor", "Own list",value = df$display.ownlist)),
      column(2,sliderInput("sort_by","Column to sort by",min=2,max=5,value=df$Sort.by,step=1)),
      column(2,checkboxInput("sort_direction", label = "reorder LogFC or P-value by smallest to largest", value = df$Reverse.label.order)),
      column(2,numericInput("label2", "Size of labels",value=df$size.label)),
      column(2,numericInput("dist2", "Distance of label", value=df$dist.label)),

    )
    
  })
  
  output$labels.cor2 <- renderUI({
    df <- values.cut.off.cor()
    
    fluidRow(
  
      column(2,numericInput("min2", "Label range (min)", value=df$min.range)),
      column(2,numericInput("max2", "Label range (max)", value=df$max.range))
    )
    
  })
  


  
  # correlation graph server ------------------
  
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
        quote=input$quote3)}
    
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
  
  input.data6 <- reactive({switch(input$dataset2,"test-data" = test.data6(),"own" = own.data6())})
  test.data6 <- reactive({
                dataframe = read.csv("test-data/Refined list.csv") })
  own.data6 <- reactive({
    inFile6 <- input$file6 
    if (is.null(inFile6)) return(NULL)
    
    else {
      dataframe <- read.csv(
        inFile6$datapath,
        header=TRUE)}
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
                      sig=ifelse(dat5$Pvalue.x<input$Pvalue1 & dat5$Pvalue.y<input$Pvalue2,"sig","NS"),
                      direction=ifelse(dat5$logFC.x>pos1 & dat5$logFC.y>pos2,"both_up",
                                       ifelse(dat5$logFC.x<neg1 & dat5$logFC.y<neg2,"both_down",
                                              ifelse(dat5$logFC.x<neg1 &  dat5$logFC.y>pos2, "opposite",
                                                     ifelse(dat5$logFC.x>pos1 & dat5$logFC.y<neg2, "opposite",
                                                            "other")))))
    
    dat_all$colour <- paste(dat_all$sig,dat_all$direction,sep="_")
    
    unique(dat_all$colour)
    
    dat_all$colour <-gsub("NS_other","other",dat_all$colour)
    dat_all$colour <-gsub("NS_opposite","other",dat_all$colour)  
    dat_all$colour <-gsub("NS_both_up","other",dat_all$colour)  
    dat_all$colour <-gsub("NS_both_down","other",dat_all$colour)  
    dat_all$colour <-gsub("sig_other","other",dat_all$colour)  
    
    
    dat_all <- mutate(dat_all, alpha = ifelse(dat_all$colour=="sig_both_up",input$cor_alpha1,
                                           ifelse(dat_all$colour=="sig_both_down",input$cor_alpha2,
                                                  ifelse(dat_all$colour=="sig_opposite",input$cor_alpha3,input$cor_alpha4))),
                      size_of_point = ifelse(dat_all$colour=="sig_both_up",input$cor_size1,
                                             ifelse(dat_all$colour=="sig_both_down",input$cor_size2,
                                                    ifelse(dat_all$colour=="sig_opposite",input$cor_size3,input$cor_size4))),
                              )
    
    
    
    colour_class <- c("sig_both_up","sig_both_down","sig_opposite","other")
    colour.df <- as.data.frame(c("sig_both_up","sig_both_down","sig_opposite","other"))
    names(colour.df) <- "label"
    colour.df$V1 <- c(input$col1,input$col2,input$col3,input$col4)
    
    colour.class1 <- colour.df[colour.df$label %in% unique(dat_all$colour),]
    
    dat_all$colour <- factor(dat_all$colour, levels = colour.class1$label)
    x_lable1 <- bquote(Log[2]~Fold~Change~(.(input$expression_x)))
    y_lable1 <- bquote(Log[2]~Fold~Change~(.(input$expression_y)))

    ID_sig <- subset(dat_all,dat_all$colour=="sig_both_up" | dat_all$colour=="sig_both_down" | dat_all$colour=="sig_opposite")
    ID_sig <-  ID_sig[order(ID_sig[,input$sort_by],decreasing = input$sort_direction),] 
    ID_sig <- ID_sig[input$min2:input$max2,]
    
    if      (input$ownlist.cor == FALSE && input$label3 == TRUE    && input$reg.line == FALSE) {
      
      
      
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        guides(col = guide_legend(ncol=input$col.cor)) +
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=colour.class1$V1, labels = colour.class1$label) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = colour.class1$label) +
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
    else if (input$ownlist.cor == FALSE && input$label3 == TRUE    && input$reg.line == TRUE) {
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        guides(col = guide_legend(ncol=input$col.cor)) +
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=colour.class1$V1, labels = colour.class1$label) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = colour.class1$label) +
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
    else if (input$ownlist.cor == FALSE && input$label3 == FALSE   && input$reg.line == TRUE) {
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        guides(col = guide_legend(ncol=input$col.cor)) +
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=colour.class1$V1, labels = colour.class1$label) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = colour.class1$label) +
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
    else if (input$ownlist.cor == TRUE  && input$reg.line == FALSE &&  input$label3 == TRUE) {
      dat6 <- input.data6();
      
      validate(
        need(nrow(dat6)>0,
             error_message_val4)
      ) 
      
      dat6 <- as.data.frame(dat6)

      ID_sig <- dat_all[dat_all$ID %in% dat6$ID,]
      ID_sig <-  ID_sig[order(ID_sig[,input$sort_by],decreasing = input$sort_direction),] 
      ID_sig <- ID_sig[input$min2:input$max2,]
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        guides(col = guide_legend(ncol=input$col.cor)) +
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=colour.class1$V1, labels = colour.class1$label) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = colour.class1$label) +
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
      vals2$cor_graph
      
      
        
    }
    else if (input$ownlist.cor == TRUE  && input$reg.line == TRUE  &&  input$label3 == TRUE) {
      dat6 <- input.data6();
      
      validate(
        need(nrow(dat6)>0,
             error_message_val4)
      ) 
      
      dat6 <- as.data.frame(dat6)
      
      ID_sig <- dat_all[dat_all$ID %in% dat6$ID,]
      ID_sig <-  ID_sig[order(ID_sig[,input$sort_by],decreasing = input$sort_direction),] 
      ID_sig <- ID_sig[input$min2:input$max2,]
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        guides(col = guide_legend(ncol=input$col.cor)) +
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=colour.class1$V1, labels = colour.class1$label) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = colour.class1$label) +
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
      vals2$cor_graph
      
      
    }
    else if (input$ownlist.cor == TRUE  && input$reg.line == FALSE &&  input$label3 == FALSE) {
      dat6 <- input.data6();
      
      validate(
        need(nrow(dat6)>0,
             error_message_val4)
      ) 
      
      dat6 <- as.data.frame(dat6)
      
      
      ID_sig <- dat_all[dat_all$ID %in% dat6$ID,]
      ID_sig <-  ID_sig[order(ID_sig[,input$sort_by],decreasing = input$sort_direction),] 
      ID_sig <- ID_sig[input$min2:input$max2,]
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        guides(col = guide_legend(ncol=input$col.cor)) +
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=colour.class1$V1, labels = colour.class1$label) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = colour.class1$label) +
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
      vals2$cor_graph
      
      
      
    }
    else if (input$ownlist.cor == TRUE  && input$reg.line == TRUE  &&  input$label3 == FALSE) {
      dat6 <- input.data6();
      
      validate(
        need(nrow(dat6)>0,
             error_message_val4)
      ) 
      
      
      ID_sig <- dat_all[dat_all$ID %in% dat6$ID,]
      ID_sig <-  ID_sig[order(ID_sig[,input$sort_by],decreasing = input$sort_direction),] 
      ID_sig <- ID_sig[input$min2:input$max2,]
      
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        guides(col = guide_legend(ncol=input$col.cor)) +
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=colour.class1$V1, labels = colour.class1$label) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = colour.class1$label) +
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
      vals2$cor_graph
      
      
    }
    else {
      vals2$cor_graph <- ggplot() +
        geom_point(aes(x=dat_all$logFC.x,y=dat_all$logFC.y, col=dat_all$colour,shape=dat_all$colour),alpha=dat_all$alpha,size=dat_all$size_of_point) +
        theme_bw()+
        guides(fill = guide_legend(override.aes = list(shape = NA))) +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position="bottom", legend.justification = "top",legend.title = element_blank())+
        guides(col = guide_legend(ncol=input$col.cor)) +
        geom_vline(xintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        geom_hline(yintercept=0, linetype="dashed", color = input$cor_sig_lines) +
        labs(y=y_lable1,
             x=x_lable1,
             title=input$title2) +
        scale_color_manual(name="legend",values=colour.class1$V1, labels = colour.class1$label) +
        scale_shape_manual(name="legend",values=c(input$cor_shape1,input$cor_shape2,input$cor_shape3,input$cor_shape4), labels = colour.class1$label) +
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
  
  # correlation table displayed -----
  output$Table5 <-DT::renderDataTable( {
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
                      sig=ifelse(dat5$Pvalue.x<input$Pvalue1 & dat5$Pvalue.y<input$Pvalue2,"sig","NS"),
                      direction=ifelse(dat5$logFC.x>pos1 & dat5$logFC.y>pos2,"both_up",
                                       ifelse(dat5$logFC.x<neg1 & dat5$logFC.y<neg2,"both_down",
                                              ifelse(dat5$logFC.x<neg1 &  dat5$logFC.y>pos2, "opposite",
                                                     ifelse(dat5$logFC.x>pos1 & dat5$logFC.y<neg2, "opposite",
                                                            "other")))))
    
    dat_all$colour <- paste(dat_all$sig,dat_all$direction,sep="_")
    
    unique(dat_all$colour)
    
    dat_all$colour <-gsub("NS_other","other",dat_all$colour)
    dat_all$colour <-gsub("NS_opposite","other",dat_all$colour)  
    dat_all$colour <-gsub("NS_both_up","other",dat_all$colour)  
    dat_all$colour <-gsub("NS_both_down","other",dat_all$colour)  
    dat_all$colour <-gsub("sig_other","other",dat_all$colour)  
    
    
    
    
    
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
        backgroundColor = styleInterval(c(-input$FC1,input$FC1), c('#abd7eb', '#D2D2CF',"#ff6961")),
        color = styleInterval(c(-input$FC1,input$FC1), c('#181A18', '#181A18', '#181A18')),
        fontWeight = styleInterval(c(-input$FC1,input$FC1), c('bold', 'normal','bold'))) %>% 
      formatStyle(
        paste("Pvalue",input$expression_x,sep="_"),
        backgroundColor = styleInterval(c(input$Pvalue1), c('#181A18', '#D2D2CF')),
        color = styleInterval(c(input$Pvalue1), c('#d99058',  '#181A18')),
        fontWeight = styleInterval(input$Pvalue1, c('bold', 'normal'))) %>% 
      formatStyle(
        paste("logFC",input$expression_y,sep="_"),
        backgroundColor = styleInterval(c(-input$FC2,input$FC2), c('#abd7eb', '#D2D2CF',"#ff6961")),
        color = styleInterval(c(-input$FC,input$FC2), c('#181A18', '#181A18', '#181A18')),
        fontWeight = styleInterval(c(-input$FC2,input$FC2), c('bold', 'normal','bold')))%>% 
      formatStyle(
        paste("Pvalue",input$expression_y,sep="_"),
        backgroundColor = styleInterval(c(input$Pvalue2), c('#181A18', '#D2D2CF')),
        color = styleInterval(c(input$Pvalue2), c('#d99058',  '#181A18')),
        fontWeight = styleInterval(input$Pvalue2, c('bold', 'normal'))) 
  }) 
  
  
  # test that isnt used? -----
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
                      sig=ifelse(dat5$Pvalue.x<input$Pvalue1 & dat5$Pvalue.y<input$Pvalue2,"sig","NS"),
                      direction=ifelse(dat5$logFC.x>pos1 & dat5$logFC.y>pos2,"both_up",
                                       ifelse(dat5$logFC.x<neg1 & dat5$logFC.y<neg2,"both_down",
                                              ifelse(dat5$logFC.x<neg1 &  dat5$logFC.y>pos2, "opposite",
                                                     ifelse(dat5$logFC.x>pos1 & dat5$logFC.y<neg2, "opposite",
                                                            "other")))))
    
    dat_all$signficance <- paste(dat_all$sig,dat_all$direction,sep="_")
    
    unique(dat_all$colour)
    
    dat_all$signficance <-gsub("NS_other","other",dat_all$signficance)
    dat_all$signficance <-gsub("NS_opposite","other",dat_all$signficance)  
    dat_all$signficance <-gsub("NS_both_up","other",dat_all$signficance)  
    dat_all$signficance <-gsub("NS_both_down","other",dat_all$signficance)  
    dat_all$signficance <-gsub("sig_other","other",dat_all$signficance)
    
    
    dat.sig <- subset(dat_all, dat_all$Pvalue.x<input$Pvalue1 & dat5$Pvalue.x<input$Pvalue2)
    names(dat_all) <- gsub('.x','',names(dat_all))
    names(dat_all)[2:3] <- paste(names(dat_all)[2:3],input$expression_x,sep="_")
    names(dat_all) <- gsub('.y','',names(dat_all))
    names(dat_all)[4:5] <- paste(names(dat_all)[4:5],input$expression_y,sep="_")
    
    dat.sig <- subset(dat_all, dat_all$signficance== "sig_both_up" | dat_all$signficance== "sig_both_down" | dat_all$signficance== "sig_opposite")
    dat.sig
    
  })

  
  


  

  # overall correlation test -----
  
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
    cat(noquote(paste("The overall Pearson's correlation (R) is ", round(R_value[[1]],3)," with a 95% confidence interval from ", round(CI.95[[1]],3), " to ",round(CI.95[[2]],3)," and a p-value of ", signif(pval[[1]],3),sep="")))
    
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
                      sig=ifelse(dat5$Pvalue.x<input$Pvalue1 & dat5$Pvalue.y<input$Pvalue2,"sig","NS"),
                      direction=ifelse(dat5$logFC.x>pos1 & dat5$logFC.y>pos2,"both_up",
                                       ifelse(dat5$logFC.x<neg1 & dat5$logFC.y<neg2,"both_down",
                                              ifelse(dat5$logFC.x<neg1 &  dat5$logFC.y>pos2, "opposite",
                                                     ifelse(dat5$logFC.x>pos1 & dat5$logFC.y<neg2, "opposite",
                                                            "other")))))
    
    dat_all$significance <- paste(dat_all$sig,dat_all$direction,sep="_")
    
    unique(dat_all$colour)
    
    dat_all$significance <-gsub("NS_other","other",dat_all$significance)
    dat_all$significance <-gsub("NS_opposite","other",dat_all$significance)  
    dat_all$significance <-gsub("NS_both_up","other",dat_all$significance)  
    dat_all$significance <-gsub("NS_both_down","other",dat_all$significance)  
    dat_all$significance <-gsub("sig_other","other",dat_all$significance)  
    
    dat.sig <- subset(dat_all, dat_all$significance== "sig_both_up" | dat_all$significance== "sig_both_down")
    
    
    if (dim(dat.sig)[1] < 4) {
      cat("There are", noquote(paste(dim(dat.sig)[1]," observations and cannot calculate correlation",sep="")))
      
    }
    else {
    
    
    pval <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$p.value)
    
    R_value <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$estimate)
    CI.95 <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$conf.int)
    round(CI.95[[1]],3)
    round(CI.95[[2]],3)
    cat(noquote(paste("The overlap of significant values for the positive Pearson's correlation (R) is ", round(R_value[[1]],3)," with a 95% confidence interval from ", round(CI.95[[1]],3), " to ",round(CI.95[[2]],3)," and a p-value of ", signif(pval[[1]],3),sep="")))
    
    }
      })
  output$cor_test_sig_neg <- renderPrint({
    
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
                      sig=ifelse(dat5$Pvalue.x<input$Pvalue1 & dat5$Pvalue.y<input$Pvalue2,"sig","NS"),
                      direction=ifelse(dat5$logFC.x>pos1 & dat5$logFC.y>pos2,"both_up",
                                       ifelse(dat5$logFC.x<neg1 & dat5$logFC.y<neg2,"both_down",
                                              ifelse(dat5$logFC.x<neg1 &  dat5$logFC.y>pos2, "opposite",
                                                     ifelse(dat5$logFC.x>pos1 & dat5$logFC.y<neg2, "opposite",
                                                            "other")))))
    
    dat_all$significance <- paste(dat_all$sig,dat_all$direction,sep="_")
    
    unique(dat_all$colour)
    
    dat_all$significance <-gsub("NS_other","other",dat_all$significance)
    dat_all$significance <-gsub("NS_opposite","other",dat_all$significance)  
    dat_all$significance <-gsub("NS_both_up","other",dat_all$significance)  
    dat_all$significance <-gsub("NS_both_down","other",dat_all$significance)  
    dat_all$significance <-gsub("sig_other","other",dat_all$significance)  
    
    dat.sig <- subset(dat_all, dat_all$significance== "sig_opposite" )
    
    if (dim(dat.sig)[1] < 4) {
      cat("There are", noquote(paste(dim(dat.sig)[1]," observations and cannot calculate correlation <4",sep="")))
    
    }
    else {
      pval <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$p.value)
      
      R_value <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$estimate)
      CI.95 <- as.list(cor.test(dat.sig$logFC.x,dat.sig$logFC.y)$conf.int)
      round(CI.95[[1]],3)
      round(CI.95[[2]],3)
      cat(noquote(paste("The overlap of significant negative correlation Pearson's correlation (R) is ", round(R_value[[1]],3)," with a 95% confidence interval from ", round(CI.95[[1]],3), " to ",round(CI.95[[2]],3)," and a p-value of ", signif(pval[[1]],3),sep="")))
      
    }
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
    # sasdfdas
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
    
    if (input$direction == "all.sig" ) {
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
    else if (input$direction == "own list") {
    
      dat6 <- input.data6();
      
      validate(
        need(nrow(dat6)>0,
             error_message_val4)
      ) 
      
      two <- one
      two<- two[order(two$ID, decreasing = F),]
      two$V1 <-  1:dim(two)[1]
      df_logFC2 <- melt(two[,c("ID","V1","logFC.x","logFC.y")],id.vars = c(1:2))
      df_logFC_ownlist <- df_logFC2[df_logFC2$ID %in% dat6$ID,]
      
      vals3$logFC_direction <- ggplot(df_logFC_ownlist,aes(y =reorder(ID,-V1), x = value, fill=variable)) +
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
  
  savedInputs <- reactive({
   
    vals4$savedInputs <-  c("user inputs included",
                            paste("The graph output selected: ",input$selected,sep=""),
                            paste("The following parameters were used for the graph titled: ",input$title),
                            paste("The font used: ",input$font,sep=""),
                            paste("The p-value=",input$Pvalue," and the logFC=",input$FC,sep=""),
                            paste("The user used the ",input$expression_y2," of p-value",sep=""),
                            paste("The y-axis ranged from 0 to ", input$yhigh,sep=""),
                            paste("The x-axis ranged from ",input$xlow," to ",input$xhigh,sep=""),
                            paste("The axix text size was ",input$axis," and the numeric size was ",input$axis_text,sep=""),
                            paste("The sig. upregulated IDs were coloured: ",input$up," with the size of ",input$size1.1," and shape ",input$shape1.1,sep=""),
                            paste("The sig. downregulated IDs were coloured: ",input$down," with the size of ",input$size2," and shape ",input$shape2,sep=""),
                            paste("The non-significant IDs were coloured: ",input$NS," with the size of ",input$size3," and shape ",input$shape3,sep=""),
                            " ",
                            paste("The follow parameters used with the labelled graph",sep=""),
                            paste("    label 1 (",input$lab1,") was coloured ",input$col_lab1," with the size of ",input$size1," and shape ",input$shape1,sep=""),
                            paste("    label 2 (",input$lab2,") was coloured ",input$col_lab2," with the size of ",input$size1," and shape ",input$shape1,sep=""),
                            paste("    label 3 (",input$lab3,") was coloured ",input$col_lab3," with the size of ",input$size1," and shape ",input$shape1,sep=""),
                            paste("    The range of labels on the graph was from ",input$min, " to ", input$max," at a distance of ",input$dist," with the text size ",input$label,sep=""),
                            paste(" ",sep=""),
                            paste("The legend was located to the ", input$legend_location ," with the text size: ",input$legend_size,sep="")
                            
                            )
    vals4$savedInputs
  })
  

  
  
  output$downloadTABLE4 <- downloadHandler(
    filename = function(){
      paste("User defined Volcano features_",gsub("-", ".", Sys.Date()),".csv", sep = "")
    },
    content = function(file){
      write.table(savedInputs(),file, row.names = FALSE, quote = F, sep=",",  col.names=FALSE)
    })
  
  
  
  
  # heatmap and upset  ------------------------------------------------------
  
  input.data.upset.heatmap <- reactive({switch(input$dataset.upset.heatmap,"test-data" = test.data.hm(),"own" = own.data.hm())})
  
  test.data.hm <- reactive({
    dataframe = read.csv("test-data/Heatmap.upset.csv") })
  own.data.hm <- reactive({
    inFile.hm <- input$file.hm
    if (is.null(inFile.hm)) return(NULL)
    
    else {
      dataframe <- read.csv(
        inFile.hm$datapath,
        header=TRUE)}
    
  })
  
  
  file.heatmap  <- function () {
    
    
    file <- input.data.upset.heatmap();
    min.FC <- min(file$logFC)
    max.FC <- max(file$logFC)
    
    df.1 <- acast(file, ID~group, value.var="logFC")
    if (input$hm.type == "all") {
      
      head(df.1)
      df.1[is.na(df.1)] <- 0
      dim(df.1)
      ?Heatmap
     # ha = HeatmapAnnotation(text = anno_text(df.1), which = "row", gp = gpar(fontfamily = "serif", fontface = "bold"))
      Heatmap(df.1[input$min.hm:input$max.hm,], 
              column_names_gp = gpar(fontfamily = input$font.hm),
              row_names_gp = gpar(fontsize = input$heatmap.font.size, fontfamily = input$font.hm),
              heatmap_legend_param = list(title = "logFC"),
              col = colorRamp2(c(min.FC, 0, max.FC), c("blue", "white", "red")))
    }
    
    else {
      df.1 <- acast(file, ID~group, value.var="logFC")
      df2 <- as.data.frame(df.1)
      in.both <- df2[complete.cases(df2),]
      Heatmap(as.matrix(in.both), 
              column_names_gp = gpar(fontfamily = input$font.hm),
              row_names_gp = gpar(fontsize = input$heatmap.font.size, fontfamily = input$font.hm),
              heatmap_legend_param = list(title = "logFC"),
              col = colorRamp2(c(min.FC, 0, max.FC), c("blue", "white", "red"))
      )
      
    }
    
  }
  
  file.heatmap.table  <- function () {
    
    
    file <- input.data.upset.heatmap();
    min.FC <- min(file$logFC)
    max.FC <- max(file$logFC)
    
    df.1 <- acast(file, ID~group, value.var="logFC")
    if (input$hm.type == "all") {
      
      head(df.1)
      df.1[is.na(df.1)] <- 0
      
      as.data.frame(df.1)

    }
    
    else {
      df.1 <- acast(file, ID~group, value.var="logFC")
      df2 <- as.data.frame(df.1)
      in.both <- df2[complete.cases(df2),]
      as.data.frame(in.both)
      
    }
    
  }
  
  
  output$heatmap.plot <- renderPlot({
    withProgress(message = 'Figure is being generated...',
                 detail = '', value = 0, {
                   test_fun2()
                 })
    par(family = "serif")
    print(file.heatmap())
  })
  
  # downloading PDF heatmap -----
  
  output$downloadTABLE.hm <- downloadHandler(
    filename = function(){
      paste("heatmap file",".csv", sep = "")
    },
    content = function(file){
      write.csv(file.heatmap.table(),file)
    }
  )
  
  output$downloadPlot_heatmap <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("ggVolcanoR_heatmap",gsub("/", "-", x), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width=input$width_heatmap,height=input$height_heatmap, onefile = FALSE) # open the pdf device
      print(file.heatmap())
      dev.off()},    contentType = "application/pdf"
  )
  
  output$downloadPlotPNG_heatmap <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("ggVolcanoR_", gsub("/", "-", x), ".png", sep = "")
    },
    content = function(file) {
      
      png(file, width = input$width_png_heatmap, height = input$height_png_heatmap, res = input$resolution_PNG_heatmap)
      print(file.heatmap())
      dev.off()},contentType = "application/png" # MIME type of the image
  )
  
  # upset plots ----
  
  observe({
    updateSelectInput(
      session,
      "upset.group.select",
      choices=names(input.data.upset.heatmap()),
      selected = "group")
    
  })
  
  file.upset  <- function () {
    file <- input.data.upset.heatmap();
    file$upset.present <- 1
    df.upset <- acast(file, ID~get(input$upset.group.select), value.var="upset.present")
    head(df.upset)
    df.upset[is.na(df.upset)] <- 0
    df.x <- make_comb_mat(as.matrix(df.upset))
    head(df.x)
    
    ht = draw(UpSet(df.x,
                    row_names_gp =  gpar(fontfamily = input$font.hm),
                    column_names_gp = gpar(fontfamily = input$font.hm),
                    top_annotation = upset_top_annotation(df.x,
                                                          annotation_name_gp = gpar(fontfamily = input$font.hm)
                                                          ),
                    right_annotation = upset_right_annotation(df.x,
                                                              annotation_name_gp = gpar(fontfamily = input$font.hm))
                    ))
    od = column_order(ht)
    cs = comb_size(df.x)
    decorate_annotation("intersection_size", {
      grid.text(cs[od], x = seq_along(cs), y = unit(cs[od], "native") + unit(2, "pt"), 
                default.units = "native", just = "bottom", gp = gpar(fontsize = input$font.size.anno.upset, fontfamily = input$font.hm)
                )
    })
    
  }
  
  
  
  output$upset.plot <- renderPlot({
    withProgress(message = 'Figure is being generated...',
                 detail = '', value = 0, {
                   test_fun2()
                 })
    
    
    print(file.upset())
  })
  
  file.upset.table  <- function () {
    file <- input.data.upset.heatmap();
    file$upset.present <- 1
    
    df.upset <- acast(file, ID~get(input$upset.group.select), value.var="upset.present")
    head(df.upset)
    df.upset[is.na(df.upset)] <- 0
    df.upset
    as.data.frame(df.upset)
  }
  
  output$downloadTABLE.upset <- downloadHandler(
    filename = function(){
      paste("upset file",".csv", sep = "")
    },
    content = function(file){
      write.csv(file.upset.table(),file)
    }
  )
  
  # downloading PDF upset -----
  output$downloadPlot_upset <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("ggVolcanoR_heatmap",gsub("/", "-", x), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width=input$width_upset,height=input$height_upset, onefile = FALSE) # open the pdf device
      print(file.upset())
      dev.off()},
    
    contentType = "application/pdf"
    
  )
  
  output$downloadPlotPNG_upset <- downloadHandler(
    filename = function() {
      x <- gsub(":", ".", Sys.time())
      paste("ggVolcanoR_", gsub("/", "-", x), ".png", sep = "")
    },
    content = function(file) {
      
      png(file, width = input$width_png_upset, height = input$height_png_upset, res = input$resolution_PNG_upset)
      print(file.upset())
      dev.off()},
    
    contentType = "application/png" # MIME type of the image
    
  )
  
  
  
  
}

shinyApp(ui, server)

