
# Allow files up to 10 Mb
options(shiny.maxRequestSize=10*1024^2)

## volcano plots
require("tidyverse")
require("ggplot2") #Best plots
require("ggrepel") #Avoid overlapping labels
require("shiny")
library("shinyBS")
require("gridExtra")
require("DT")
require("plyr")
require("dplyr")
require("reshape2")

filtered_table <- c("pos","neg","both","own list")

fonts <- c("Arial" = "sans", 
           "Times New Roman" = "serif", 
           "Courier" = "mono")

selected_present <- c("no labels","range of genes","own list")
y_options <- c("-Log10(p-value)","FDR", "adjusted")
legend_location <- c("right","bottom","left","top","none")
species <- c("BOVIN","CHICK","ECOLI","HORSE","HUMAN","MAIZE","MOUSE","PEA", "PIG","RABIT","RAT","SHEEP","SOYBN","TOBAC","WHEAT","YEAST")
lab <- c("list_up","list_down")

#####

ui <- navbarPage(title = "ggVolcanoR Shiny App", id="main",
                 tabPanel("Volcano plot",sidebarLayout(sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 700px; position:relative;", width=3,
                                                                    popify(actionButton("btn3", "Uploading the file"), "R is a case sensitive language. Please ensure that the upper and lower cases are matched when uploading the file. ID and id are different", placement="bottom", trigger = "hover"),
                                                                    fileInput('file1', 'ID, logFC, Pvalue',
                                                                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                                                    radioButtons('sep', 'Separator', c( Tab='\t', Comma=','), ','),
                                                                    radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
                                                                    tags$hr(),
                                                                    popify(actionButton("btn4", "Types of graphs"), "Three distinct graphs for labelling purposes: (1) no label, (2) sequential (3) user uploaded labels (can be non-sequential)", 
                                                                           placement="bottom", trigger = "hover"),
                                                                    selectInput('selected', 'type of output: header=ID', selected_present),
                                                                    
                                                                    fileInput('file2', 'Choose selected gene file (.csv)',
                                                                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                                                    p("select font for graph"),
                                                                    selectInput('font',
                                                                                'font type',
                                                                                choices = fonts, 
                                                                                selected = fonts[1]),
                                                                    p("Cut-offs"),
                                                                    numericInput("Pvalue", "p-value cut-off", value=0.05),
                                                                    numericInput("FC", "absolute log2 fold change", value=0.58),
                                                                    textInput(inputId = "sig_lines", label = "significance lines",value = "grey"),
                                                                    p("Axis parameters"),
                                                                    selectInput(inputId = "y", 
                                                                                label = "Y-axis label", 
                                                                                choices = y_options, 
                                                                                selected = "-Log10(p-value)"),
                                                                    numericInput("yhigh","y axis upper range",value = 100),
                                                                    numericInput("ybreaks","y axis breaks",value = 10),
                                                                    numericInput("xlow","x axis lower range",value = -10),
                                                                    numericInput("xhigh","x axis upper range",value = 10),
                                                                    numericInput("xbreaks","x axis upper breaks",value = 1),
                                                                    sliderInput("axis", "axis text size", min=0, max=100, value=30, step=0.1),
                                                                    sliderInput("axis_text", "axis numeric text size", min=0, max=100, value=30, step=0.1),
                                                                    p("point size and transparancy"),
                                                                    numericInput("size", "point size", value=3),
                                                                    sliderInput("alpha1", "transparency of top ID", min=0.01, max=1, value=1,step = 0.01),
                                                                    sliderInput("alpha2", "transparency of sig ID", min=0.01, max=1, value=0.5,step = 0.01),
                                                                    sliderInput("alpha3", "transparency of non-sig ID", min=0.01, max=1, value=0.25,step = 0.01),
                                                                    p("Colour of points"),
                                                                    textInput(inputId = "up", label = "up-regulated",value = "red"),
                                                                    textInput(inputId = "down",  label = "down-regulated", value = "steelblue1"),
                                                                    textInput(inputId = "NS", label = "Non-significant",value = "grey"),
                                                                    textInput(inputId = "col_lab1", label = "label one",value = "orange"),
                                                                    selectInput(inputId = "lab1", 
                                                                                label = "select label 1", 
                                                                                choices = lab, 
                                                                                selected = "list_down"),
                                                                    textInput(inputId = "col_lab2", label = "label two",value = "darkblue"),
                                                                    selectInput(inputId = "lab2", 
                                                                                label = "select label 2", 
                                                                                choices = lab, 
                                                                                selected = "list_up"),
                                                                    p("Label parameters"),
                                                                    numericInput("min", "lable range (min)", value=1),
                                                                    numericInput("max", "lable range (max)", value=20),
                                                                    sliderInput("dist", "distance of label", min=0, max=5, value=1,step = 0.1),
                                                                    sliderInput("label", "size of labels", min=0, max=60, value=8,step =0.1),
                                                                    p("Legend parameters"),
                                                                    selectInput('legend_location', 'Legend location', legend_location),
                                                                    numericInput("col", "# of legend columns", value=1, step = 1),
                                                                    sliderInput("legend_size", "legend text size", min=1, max=60, value=12),
                                                                    
                                                                    p("Exporting the ggVolcanoR plot"),
                                                                    numericInput("width", "width of PDF", value=10),
                                                                    numericInput("height", "height of PDF", value=8),
                                                                    downloadButton('downloadPlot','Download PDF'),
                                                                    numericInput("width_png","width of png", value = 1600),
                                                                    numericInput("height_png","height of png", value = 1200),
                                                                    numericInput("resolution_PNG","resolution of PNG", value = 144),
                                                                    downloadButton('downloadPlotPNG','Download PNG')
                 ),
                 mainPanel(tabsetPanel(tabPanel("Plot", textInput(inputId = "title", 
                                                                  label = "",
                                                                  value = "Volcano plot: x vs y"),
                                                plotOutput("ggplot",height = "600px"),
                                                verbatimTextOutput("value")),
                                       tabPanel("Table with links", 
                                                p("Table rendering parameters"),
                                                popify(actionButton("btn2", "Uniprot search species"), "List of 16 searchable species: https://www.uniprot.org/help/taxonomy", 
                                                       placement="bottom", trigger = "hover"),
                                                selectInput("species", label = "",species,selected = "HUMAN"),
                                                div(DT::dataTableOutput("myoutput",height = "100px"))),
                                       tabPanel("Summary table",DT::dataTableOutput("summary_table"),
                                                selectInput('export', 'Selected Filtered csv file', filtered_table),
                                                downloadButton("downloadTABLE", "Filtered Table"))
                 )))),
                 tabPanel("Read Me",includeMarkdown("README.md")))


# the function of the app ----
server <- function(input, output) {
  
  options(shiny.sanitize.errors = TRUE)
  
  vals <- reactiveValues(ggplot=NULL)
  #summary_table=NULL,myoutput=NULL,file1=NULL
  input.data <- reactive({
    
    
    
    inFile <- input$file1
    if (is.null(inFile)) {  dataframe = data.frame() }
    
    else {
      dataframe <- read.csv(
        inFile$datapath,
        header=TRUE,
        sep=input$sep,
        quote=input$quote
      )}
  })
  input.data2 <- function(){ 
    
    inFile2 <- input$file2
    if (is.null(inFile2)) { dataframe2= NULL}
    
    else {
      dataframe2 <- read.csv(
        inFile2$datapath,
        header=T)}
  }
  output$summary_table <-DT::renderDataTable({
    
    dat <- input.data();
    
    validate(
      need(nrow(dat)>0,
           "Please import the data")
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
                                    significance=ifelse(dat$Pvalue<input$Pvalue & dat$logFC>pos,"up",
                                                        ifelse(dat$Pvalue<input$Pvalue & dat$logFC<neg,"down","non significant")))
      
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
                                    significance=ifelse(dat$Pvalue<input$Pvalue & dat$logFC>pos,"up",
                                                        ifelse(dat$Pvalue<input$Pvalue & dat$logFC<neg,"down","non significant")))
      
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
           "This is the ggVolcanoR Shiny app\nPlease upload file\nheaders=ID, logFC, Pvalue")
    )
    
    
    dat2 <- input.data2();
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    
    list <- dat2$ID
    
    list2 <- dat2$ID
    
    #####
    neg <- -1*input$FC
    pos <- input$FC
    
    maximum <- input$max
    
    mutateddf <- mutate(dat, sig=ifelse(dat$Pvalue<0.05, "Pvalue<0.05", "Not Sig")) 
    sig <- subset(dat, dat$Pvalue<input$Pvalue & abs(dat$logFC)>input$FC)
    top <- sig[(input$min:input$max),]
    
    gene_list <- top$ID
    
    mutateddf.gene <- mutate(mutateddf, top=ifelse(mutateddf$ID %in% gene_list, "top", "other"))
    mutateddf.gene
    sub.mutateddf.gene <- mutate(mutateddf.gene,
                                 colour=ifelse(mutateddf.gene$Pvalue<input$Pvalue & mutateddf.gene$logFC>pos,"sig_up",
                                               ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")),
                                 alpha=ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$alpha2,
                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$alpha2,input$alpha3)))
    # range of genes
    sub.mutateddf.gene2 <- mutate(mutateddf.gene,
                                  colour=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, "top_up",
                                                ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, "top_down",                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,"sig_up",
                                                                                                                                                                                                                                                                      ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")))),
                                  alpha=ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,
                                               ifelse(mutateddf.gene$ID %in% gene_list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,                                                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$alpha2,                                                                                                                              ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$alpha2,input$alpha3)))))
    # own list graph
    sub.mutateddf.gene_list <- mutate(mutateddf.gene,
                                      colour=ifelse(mutateddf.gene$ID %in% list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, "top_up",
                                                    ifelse(mutateddf.gene$ID %in% list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, "top_down",
                                                           ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,"sig_up",
                                                                  ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,"sig_down","NS")))),
                                      alpha=ifelse(mutateddf.gene$ID %in% list & mutateddf.gene$logFC>pos & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,
                                                   ifelse(mutateddf.gene$ID %in% list & mutateddf.gene$logFC<neg & mutateddf.gene$Pvalue<input$Pvalue, input$alpha1,
                                                          ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,input$alpha2,
                                                                 ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC<neg,input$alpha2,input$alpha3)))),
                                      
    )
    
    
    
    
    ##### 
    
    if (input$selected=="range of genes" && input$y=="-Log10(p-value)") {
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene2$logFC, y=-log10(sub.mutateddf.gene2$Pvalue),
                       col=sub.mutateddf.gene2$colour),size=input$size,alpha=sub.mutateddf.gene2$alpha) +
        geom_text_repel(data=sub.mutateddf.gene2[sub.mutateddf.gene2$ID %in% gene_list,]
                        ,aes(x=sub.mutateddf.gene2$logFC[sub.mutateddf.gene2$ID %in% gene_list], 
                             y=  -log10(sub.mutateddf.gene2$Pvalue)[sub.mutateddf.gene2$ID %in% gene_list],
                             label= sub.mutateddf.gene2$ID[sub.mutateddf.gene2$ID %in% gene_list]),
                        size=input$label,
                        family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines')) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab2,input$col_lab1),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2)) +
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
        labs(y=expression("-"~Log[10]~"(p-value)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title) +
        guides(size=FALSE, col = guide_legend(ncol=input$col))+
        scale_alpha(guide = 'none')+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      vals$ggplot
      
    }
    else if (input$selected=="range of genes" && input$y=="FDR"){
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene2$logFC, y=-log10(sub.mutateddf.gene2$Pvalue),
                       col=sub.mutateddf.gene2$colour),size=input$size,alpha=sub.mutateddf.gene2$alpha) +
        geom_text_repel(data=sub.mutateddf.gene2[sub.mutateddf.gene2$ID %in% gene_list,]
                        ,aes(x=sub.mutateddf.gene2$logFC[sub.mutateddf.gene2$ID %in% gene_list], 
                             y=  -log10(sub.mutateddf.gene2$Pvalue)[sub.mutateddf.gene2$ID %in% gene_list],
                             label= sub.mutateddf.gene2$ID[sub.mutateddf.gene2$ID %in% gene_list]),
                        size=input$label,
                        family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines')) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab2,input$col_lab1),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2)) +
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
        labs(y=expression("-"~Log[10]~"(FDR)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title) +
        guides(size=FALSE, col = guide_legend(ncol=input$col))+
        scale_alpha(guide = 'none')+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      vals$ggplot
      
    }
    else if (input$selected=="range of genes" && input$y=="adjusted") {
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene2$logFC, y=-log10(sub.mutateddf.gene2$Pvalue),
                       col=sub.mutateddf.gene2$colour),size=input$size,alpha=sub.mutateddf.gene2$alpha) +
        geom_text_repel(data=sub.mutateddf.gene2[sub.mutateddf.gene2$ID %in% gene_list,]
                        ,aes(x=sub.mutateddf.gene2$logFC[sub.mutateddf.gene2$ID %in% gene_list], 
                             y=  -log10(sub.mutateddf.gene2$Pvalue)[sub.mutateddf.gene2$ID %in% gene_list],
                             label= sub.mutateddf.gene2$ID[sub.mutateddf.gene2$ID %in% gene_list]),
                        size=input$label,
                        family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines')) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab2,input$col_lab1),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2)) +
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
        labs(y=expression("-"~Log[10]~"(adj P-value)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title) +
        guides(size=FALSE, col = guide_legend(ncol=input$col))+
        scale_alpha(guide = 'none')+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      vals$ggplot
      
    }
    else if (input$selected=="own list" && input$y=="-Log10(p-value)") {
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene_list$logFC, y=-log10(sub.mutateddf.gene_list$Pvalue),col=sub.mutateddf.gene_list$colour),size=input$size,alpha=sub.mutateddf.gene_list$alpha) +
        geom_text_repel(data=sub.mutateddf.gene_list[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC,]
                        ,aes(x=sub.mutateddf.gene_list$logFC[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC], 
                             y=  -log10(sub.mutateddf.gene_list$Pvalue)[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC],
                             label= sub.mutateddf.gene_list$ID[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC]),
                        size=input$label,family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines')) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab2,input$col_lab1),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2)) +
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
        labs(y=expression("-"~Log[10]~"(p-value)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title) 
      
      vals$ggplot
    }
    else if (input$selected=="own list" && input$y=="FDR") {
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene_list$logFC, y=-log10(sub.mutateddf.gene_list$Pvalue),col=sub.mutateddf.gene_list$colour),size=input$size,alpha=sub.mutateddf.gene_list$alpha) +
        geom_text_repel(data=sub.mutateddf.gene_list[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC,]
                        ,aes(x=sub.mutateddf.gene_list$logFC[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC], 
                             y=  -log10(sub.mutateddf.gene_list$Pvalue)[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC],
                             label= sub.mutateddf.gene_list$ID[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC]),
                        size=input$label,family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines')) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab2,input$col_lab1),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2)) +
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
        labs(y=expression("-"~Log[10]~"(FDR)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title) 
      
      vals$ggplot
    }
    else if (input$selected=="own list" && input$y=="adjusted") {
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene_list$logFC, y=-log10(sub.mutateddf.gene_list$Pvalue),col=sub.mutateddf.gene_list$colour),size=input$size,alpha=sub.mutateddf.gene_list$alpha) +
        geom_text_repel(data=sub.mutateddf.gene_list[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC,]
                        ,aes(x=sub.mutateddf.gene_list$logFC[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC], 
                             y=  -log10(sub.mutateddf.gene_list$Pvalue)[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC],
                             label= sub.mutateddf.gene_list$ID[sub.mutateddf.gene_list$ID %in% list2 & sub.mutateddf.gene_list$Pvalue<input$Pvalue & abs(sub.mutateddf.gene_list$logFC)>input$FC]),
                        size=input$label,family=input$font, 
                        segment.alpha = 0.5, 
                        show.legend = F,box.padding = unit(input$dist, 'lines')) +
        scale_color_manual(values=c(input$NS,input$down,input$up,input$col_lab2,input$col_lab1),labels=c("non-significant","down-regulated","up-regulated",input$lab1,input$lab2)) + 
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
        labs(y=expression("-"~Log[10]~"(adj P-value)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title) +
        
        vals$ggplot
    }
    else if(input$selected=="no labels" && input$y=="-Log10(p-value)") {     
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene$logFC, y=-log10(sub.mutateddf.gene$Pvalue),col=sub.mutateddf.gene$colour),size=input$size,alpha=sub.mutateddf.gene$alpha) +
        scale_color_manual(values=c(input$NS,input$down,input$up),labels=c("non-significant","down-regulated","up-regulated")) + 
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
        labs(y=expression("-"~Log[10]~"(p-value)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title)+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      
      vals$ggplot
    }
    else if(input$selected=="no labels" && input$y=="FDR") {     
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene$logFC, y=-log10(sub.mutateddf.gene$Pvalue),col=sub.mutateddf.gene$colour),size=input$size,alpha=sub.mutateddf.gene$alpha) +
        scale_color_manual(values=c(input$NS,input$down,input$up),labels=c("non-significant","down-regulated","up-regulated")) + 
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
        labs(y=expression("-"~Log[10]~"(FDR)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title)+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      
      vals$ggplot
    }
    else if(input$selected=="no labels" && input$y=="adjusted") {     
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene$logFC, y=-log10(sub.mutateddf.gene$Pvalue),col=sub.mutateddf.gene$colour),size=input$size,alpha=sub.mutateddf.gene$alpha) +
        scale_color_manual(values=c(input$NS,input$down,input$up),labels=c("non-significant","down-regulated","up-regulated")) + 
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
        labs(y=expression("-"~Log[10]~"(adj P-value)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title)+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      
      vals$ggplot
    }
    else {     
      vals$ggplot <- ggplot() + 
        geom_point(aes(x=sub.mutateddf.gene$logFC, y=-log10(sub.mutateddf.gene$Pvalue),col=sub.mutateddf.gene$colour),size=input$size,alpha=sub.mutateddf.gene$alpha) +
        scale_color_manual(values=c(input$NS,input$down,input$up),labels=c("non-significant","down-regulated","up-regulated")) + 
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
        labs(y=expression("-"~Log[10]~"(adj P-value)"),
             x=expression(Log[2]~Fold~Change),
             title=input$title)+
        scale_y_continuous(limits = c(0, input$yhigh) ,breaks = seq(0, input$yhigh, by = input$ybreaks))+
        scale_x_continuous(limits = c(input$xlow, input$xhigh), breaks = seq(input$xlow, input$xhigh, by = input$xbreaks))
      
      vals$ggplot
    }
  }
  output$ggplot <- renderPlot({
    if (input$selected == "")
      return(NULL)
    print(plotInput())
  })
  
  output$myoutput <-DT::renderDataTable(escape = FALSE,filter = 'top', options=list(extensions="Buttons", scrollX = T), {
    
    dat <- input.data();
    
    validate(
      need(nrow(dat)>0,
           "")
    )
    
    dat2 <- input.data2();
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$Pvalue),]
    rownames(dat) <- 1:dim(dat)[1]
    
    if (input$selected=="range of genes") {
      sig <- subset(dat, dat$Pvalue<input$Pvalue & abs(dat$logFC)>input$FC)
      top <- sig[(input$min:input$max),]
      
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      top$GeneCards <- paste("<a href='","https://www.genecards.org/cgi-bin/carddisp.pl?gene=",top$ID,"'>",top$ID,"</a>", sep="")
      top$Protein_atlas <- paste("<a href='","https://www.proteinatlas.org/search/",top$ID,"'>",top$ID,"</a>", sep="")
      top$UniProt_species <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",SYMBOL_list$list,"'>",SYMBOL_list$list,"</a>", sep="")
      top$UniProt_other <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",top$ID,"'>",top$ID,"</a>", sep="")
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
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
      top <- dat
      
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      top$GeneCards <- paste("<a href='","https://www.genecards.org/cgi-bin/carddisp.pl?gene=",top$ID,"'>",top$ID,"</a>", sep="")
      top$Protein_atlas <- paste("<a href='","https://www.proteinatlas.org/search/",top$ID,"'>",top$ID,"</a>", sep="")
      
      top$UniProt_species <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",SYMBOL_list$list,"'>",SYMBOL_list$list,"</a>", sep="")
      top$UniProt_other <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",top$ID,"'>",top$ID,"</a>", sep="")
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      
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
    else {
      
      
      list <- dat2$ID
      
      selected_genes <- dat[dat$ID %in% list,]
      datatable(selected_genes) %>% formatSignif('Pvalue',3) %>% formatRound('logFC',2)
      SYMBOL_list <- as.data.frame(paste(selected_genes$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      selected_genes$genecardslink <- paste("<a href='","https://www.genecards.org/cgi-bin/carddisp.pl?gene=",selected_genes$ID,"'>",selected_genes$ID,"</a>", sep="")
      selected_genes$proteinatlaslink <- paste("<a href='","https://www.proteinatlas.org/search/",selected_genes$ID,"'>",selected_genes$ID,"</a>", sep="")
      selected_genes$UniProtlink_16_species <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",SYMBOL_list$list,"'>",SYMBOL_list$list,"</a>", sep="")
      selected_genes$UniProtlink_other_species <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",selected_genes$ID,"'>",selected_genes$ID,"</a>", sep="")
      selected_genes$Pvalue <- signif(selected_genes$Pvalue,3)
      selected_genes$logFC <- signif(selected_genes$logFC,3)
      selected_genes 
      
      datatable(selected_genes, escape = FALSE,filter = 'top', options=list(extensions="Buttons", scrollX = T), selection = 'none') %>% 
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
    
    if (input$export=="pos") {
      positive <- subset(dat, dat$Pvalue<input$Pvalue & dat$logFC>pos)
      positive
    }
    else if (input$export=="neg") {
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
  
  
  # downloading PDF -----
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("ggVolcanoR_", Sys.time(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width=input$width,height=input$height, onefile = FALSE) # open the pdf device
      grid.arrange(vals$ggplot)
      dev.off()}
    
  )
  
  output$downloadPlotPNG <- downloadHandler(
    filename = function() {
      paste("ggVolcanoR_", Sys.time(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$width_png, height = input$height_png, res = input$resolution_PNG)
      grid.arrange(vals$ggplot)
      dev.off()}
  )
  
  output$downloadTABLE <- downloadHandler(
    
    filename = function(){
      paste(gsub("-", ".", Sys.Date()), " - ","Filtered",input$export," data",".csv", sep = "")
    },
    
    content = function(file){
      write.csv(dataExpTable(),file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui, server)
