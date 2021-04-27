## volcano plots
require("tidyverse")
require("ggplot2") #Best plots
require("ggrepel") #Avoid overlapping labels
require("shiny")
library(shinyBS)
require("gridExtra")
require("DT")
require(plyr)
require(dplyr)
require(reshape2)

fonts <- c("sans","serif","mono")
selected_present <- c("no labels","range of genes","own list")
y_options <- c("-Log10(p-value)","FDR", "adjusted")
legend_location <- c("right","bottom","left","top","none")
species <- c("BOVIN","CHICK","ECOLI","HORSE","HUMAN","MAIZE","MOUSE","PEA", "PIG","RABIT","RAT","SHEEP","SOYBN","TOBAC","WHEAT","YEAST")

#####

ui <- fluidPage(tabsetPanel(              
  tabPanel(title = "Volcano plot",sidebarLayout(sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",
                                          popify(actionButton("btn1", "Click Me!"), "I can give you a hint", 
                                                              placement="bottom", trigger = "click"),
                                          popify(actionButton("btn3", "HINT: uploading the file"), "R is a case sensitive language. Please ensure that the upper and lower cases are matched when uploading the file. ID and id are different", placement="bottom", trigger = "click"),
                                          fileInput('file1', 'Choose File: ID, logFC, Pvalue',
                                                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                          checkboxInput('header', 'Header', TRUE),
                                          radioButtons('sep', 'Separator', c( Tab='\t', Comma=','), ','),
                                          radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
                                          tags$hr(),
                                          popify(actionButton("btn4", "Types of graphs"), "Three distinct graphs for labelling purposes: (1) no label, (2) sequential (3) user uploaded labels (can be non-sequential)", 
                                                 placement="bottom", trigger = "click"),
                                          selectInput('selected', 'type of output: header=ID', selected_present),
                                          
                                          fileInput('file2', 'Choose selected gene file (.csv)',
                                                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                          
                                          selectInput('font', 'font type', fonts),
                                          selectInput(inputId = "y", 
                                                      label = "Indicator to display on Y-axis", 
                                                      choices = y_options, 
                                                      selected = "-Log10(p-value)"),
                                          popify(actionButton("btn2", "Uniprot search species"), "List of 16 searchable species: https://www.uniprot.org/help/taxonomy", 
                                                                placement="bottom", trigger = "click"),
                                          selectInput("species", label = "",species,selected = "HUMAN"),
                                          numericInput("Pvalue", "p-value cut-off", value=0.05),
                                          numericInput("FC", "absolute log2 fold change", value=0.58),
                                          numericInput("yhigh","y axis upper range",value = 100),
                                          numericInput("ybreaks","y axis breaks",value = 10),
                                          numericInput("xlow","x axis lower range",value = -10),
                                          numericInput("xhigh","x axis upper range",value = 10),
                                          numericInput("xbreaks","x axis upper breaks",value = 1),
                                          numericInput("size", "point size", value=3),
                                          sliderInput("axis", "axis text size", min=1, max=60, value=30),
                                          sliderInput("axis_text", "axis numeric text size", min=1, max=60, value=30),
                                          selectInput('legend_location', 'Legend location', legend_location),
                                          numericInput("col", "# of legend columns", value=1, step = 1),
                                          sliderInput("legend_size", "legend text size", min=1, max=60, value=12),
                                          sliderInput("dist", "distance of label", min=0, max=2, value=1,step = 0.1),
                                          sliderInput("label", "size of labels", min=0, max=30, value=8,step =0.1),
                                          numericInput("min", "lable range (min)", value=1),
                                          numericInput("max", "lable range (max)", value=20),
                                          sliderInput("alpha1", "transparency of top ID", min=0.01, max=1, value=1,step = 0.01),
                                          sliderInput("alpha2", "transparency of sig ID", min=0.01, max=1, value=0.5,step = 0.01),
                                          sliderInput("alpha3", "transparency of non-sig ID", min=0.01, max=1, value=0.25,step = 0.01),
                                          textInput(inputId = "up", label = "up-regulated",value = "red"),
                                          textInput(inputId = "down",  label = "down-regulated", value = "steelblue1"),
                                          textInput(inputId = "NS", label = "Non-significant",value = "grey"),
                                          textInput(inputId = "lab_up", label = "labelled up",value = "orange"),
                                          textInput(inputId = "lab_down", label = "labelled down",value = "darkblue"),
                                          textInput(inputId = "sig_lines", label = "significance lines",value = "grey"),
                                          numericInput("width", "width of PDF", value=10),
                                          numericInput("height", "height of PDF", value=8),
                                          downloadButton('downloadPlot','Download PDF'),
                                          numericInput("width_png","width of png", value = 1600),
                                          numericInput("height_png","height of png", value = 1200),
                                          numericInput("resolution_PNG","resolution of PNG", value = 144),
                                          downloadButton('downloadPlotPNG','Download PNG')
                                          ),
                              mainPanel(textInput(inputId = "title", 
                                                  label = "",
                                                   value = "Volcano plot: x vs y"),
                                        plotOutput("ggplot",height = "600px"),
                                                                                verbatimTextOutput("value"),
                                        DT::dataTableOutput("myoutput",height = "100px"),
                                        DT::dataTableOutput("summary_table",height = "100px"))))))

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
        header=input$header,
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
           "")
    )
    
    dat2 <- input.data2();
    list <- dat2$ID
        dat <- as.data.frame(dat)
    neg <- -1*input$FC
    pos <- input$FC
    
    if (input$selected=="own list") {
      sub.mutateddf.gene3 <- mutate(dat,
                                    significance=ifelse(dat$ID %in% list & dat$logFC>pos,"list_Up",
                                                        ifelse(dat$ID %in% list & dat$logFC<neg,"List_down","not in list")))
      sub.mutateddf.gene3$count <- 1
      summary <- as.data.frame(ddply(sub.mutateddf.gene3,c("significance"),numcolwise(sum)))[c(1,4)]
      summary
    }
    
    else if (input$selected=="no labels") {
    sub.mutateddf.gene3 <- mutate(dat,
                                    significance=ifelse(dat$Pvalue<input$Pvalue & dat$logFC>pos,"Up",
                                                        ifelse(dat$Pvalue<input$Pvalue & dat$logFC<neg,"down","NS")))
      
      sub.mutateddf.gene3$count <- 1
      summary <- as.data.frame(ddply(sub.mutateddf.gene3,c("significance"),numcolwise(sum)))[c(1,4)]
      summary}
    
   else {
    sub.mutateddf.gene3 <- mutate(dat,
                                  significance=ifelse(dat$Pvalue<input$Pvalue & dat$logFC>pos,"Up",
                                               ifelse(dat$Pvalue<input$Pvalue & dat$logFC<neg,"Down","NS")))
    
    sub.mutateddf.gene3$count <- 1
        summary <- as.data.frame(ddply(sub.mutateddf.gene3,c("significance"),numcolwise(sum)))[c(1,4)]
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
      list <- dat2$ID
      list2 <- dat2$ID[(input$min:input$max)]
      #####
      neg <- -1*input$FC
      pos <- input$FC
      
      maximum <- input$max
      
      mutateddf <- mutate(dat, sig=ifelse(dat$Pvalue<0.05, "Pvalue<0.05", "Not Sig")) 
      top <- dat[(input$min:input$max),]
      
      gene_list <- top$ID
      
      mutateddf.gene <- mutate(mutateddf, top=ifelse(mutateddf$ID %in% gene_list, "top", "other"))
      mutateddf.gene
      sub.mutateddf.gene <- mutate(mutateddf.gene,
                                   colour=ifelse(mutateddf.gene$Pvalue<input$Pvalue& mutateddf.gene$logFC>pos,"sig_up",
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
          scale_color_manual(values=c(input$NS,input$down,input$up,input$lab_down,input$lab_up),labels=c("non-significant","down-regulated","up-regulated","list_down","list_up")) +
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
          scale_color_manual(values=c(input$NS,input$down,input$up,input$lab_down,input$lab_up),labels=c("non-significant","down-regulated","up-regulated","list_down","list_up")) +
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
          scale_color_manual(values=c(input$NS,input$down,input$up,input$lab_down,input$lab_up),labels=c("non-significant","down-regulated","up-regulated","list_down","list_up")) +
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
          geom_text_repel(data=sub.mutateddf.gene_list[sub.mutateddf.gene_list$ID %in% list2 ,]
                          ,aes(x=sub.mutateddf.gene_list$logFC[sub.mutateddf.gene_list$ID %in% list2 ], 
                               y=  -log10(sub.mutateddf.gene_list$Pvalue)[sub.mutateddf.gene_list$ID %in% list2 ],
                               label= sub.mutateddf.gene_list$ID[sub.mutateddf.gene_list$ID %in% list2 ]),
                          size=input$label,family=input$font, 
                          segment.alpha = 0.5, 
                          show.legend = F,box.padding = unit(input$dist, 'lines')) +
          scale_color_manual(values=c(input$NS,input$down,input$up,input$lab_down,input$lab_up),labels=c("non-significant","down-regulated","up-regulated","list_down","list_up")) + 
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
          geom_text_repel(data=sub.mutateddf.gene_list[sub.mutateddf.gene_list$ID %in% list2,]
                          ,aes(x=sub.mutateddf.gene_list$logFC[sub.mutateddf.gene_list$ID %in% list2], 
                               y=  -log10(sub.mutateddf.gene_list$Pvalue)[sub.mutateddf.gene_list$ID %in% list2],
                               label= sub.mutateddf.gene_list$ID[sub.mutateddf.gene_list$ID %in% list2]),
                          size=input$label,family=input$font, 
                          segment.alpha = 0.5, 
                          show.legend = F,box.padding = unit(input$dist, 'lines')) +
          scale_color_manual(values=c(input$NS,input$down,input$up,input$lab_down,input$lab_up),labels=c("non-significant","down-regulated","up-regulated","list_down","list_up")) + 
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
          geom_text_repel(data=sub.mutateddf.gene_list[sub.mutateddf.gene_list$ID %in% list2,]
                          ,aes(x=sub.mutateddf.gene_list$logFC[sub.mutateddf.gene_list$ID %in% list2], 
                               y=  -log10(sub.mutateddf.gene_list$Pvalue)[sub.mutateddf.gene_list$ID %in% list2],
                               label= sub.mutateddf.gene_list$ID[sub.mutateddf.gene_list$ID %in% list2]),
                          size=input$label,family=input$font, 
                          segment.alpha = 0.5, 
                          show.legend = F,box.padding = unit(input$dist, 'lines')) +
          scale_color_manual(values=c(input$NS,input$down,input$up,input$lab_down,input$lab_up),labels=c("non-significant","down-regulated","up-regulated","list_down","list_up")) + 
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
    
    if (input$selected=="range of genes") {
      top <- dat[(input$min:input$max),]
      
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      top$genecards_link <- paste("<a href='","https://www.genecards.org/cgi-bin/carddisp.pl?gene=",top$ID,"'>",top$ID,"</a>", sep="")
      top$proteinatlas_link <- paste("<a href='","https://www.proteinatlas.org/search/",top$ID,"'>",top$ID,"</a>", sep="")
      top$UniProt_link <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",top$ID,"'>",top$ID,"</a>", sep="")
      top$UniProt_species_link <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",SYMBOL_list$list,"'>",SYMBOL_list$list,"</a>", sep="")
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df
      
    }
    else if (input$selected=="no labels") {
      top <- dat[(input$min:input$max),]
      
      SYMBOL_list <- as.data.frame(paste(top$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      top$genecardslink <- paste("<a href='","https://www.genecards.org/cgi-bin/carddisp.pl?gene=",top$ID,"'>",top$ID,"</a>", sep="")
      top$proteinatlaslink <- paste("<a href='","https://www.proteinatlas.org/search/",top$ID,"'>",top$ID,"</a>", sep="")
      top$UniProtlink <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",top$ID,"'>",top$ID,"</a>", sep="")
      top$UniProtlink_human <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",SYMBOL_list$list,"'>",SYMBOL_list$list,"</a>", sep="")
      df <- top
      df$Pvalue <- signif(df$Pvalue,3)
      df$logFC <- signif(df$logFC,3)
      df
      
    }
    else {
      
      
        list <- dat2$ID
      
      selected_genes <- dat[dat$ID %in% list & dat$Pvalue<input$Pvalue,]
      datatable(selected_genes) %>% formatSignif('Pvalue',3) %>% formatRound('logFC',2)
      SYMBOL_list <- as.data.frame(paste(selected_genes$ID,"_",input$species,sep=""))
      names(SYMBOL_list) <- "list"
      
      selected_genes$genecardslink <- paste("<a href='","https://www.genecards.org/cgi-bin/carddisp.pl?gene=",selected_genes$ID,"'>",selected_genes$ID,"</a>", sep="")
      selected_genes$proteinatlaslink <- paste("<a href='","https://www.proteinatlas.org/search/",selected_genes$ID,"'>",selected_genes$ID,"</a>", sep="")
      selected_genes$UniProtlink<- paste("<a href='","https://www.uniprot.org/uniprot/?query=",selected_genes$ID,"'>",selected_genes$ID,"</a>", sep="")
      selected_genes$UniProtlink_human <- paste("<a href='","https://www.uniprot.org/uniprot/?query=",SYMBOL_list$list,"'>",SYMBOL_list$list,"</a>", sep="")
      selected_genes$Pvalue <- signif(selected_genes$Pvalue,3)
      selected_genes$logFC <- signif(selected_genes$logFC,3)
      selected_genes 
      
    }
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
      dev.off()
    })
}

shinyApp(ui, server)

