---
title: "ggVolcanoR READ ME file" 
author: "Kerry A Mullan" 
output: html_document 
---

# ggVolcanoR

This is the README.md file containing information on the features of the application
Please contact x for error reporting. 

## file types accepted
The file must contain headers: ID, logFC and Pvalue. 
Unique ID names are preferred especially when labelling the graph
This needs to be in the form of either a **.csv** or **.txt** file

## uploading the file
![](IMAGES/1.uploading.png)

## types of graphs
There are **three** different types of graphs that differ on how the graph is labeled. 
![range of graph types](MAGES/2.types.png)
* **Unlabeled graph**
  - the table will render the first 20 points, but can include the total range of values. 
  
![](IMAGES/2.Nolabs)
  
* **range of genes** 
  - default list the top 20
  - This looks at both up and down directions

![](IMAGES/2.top20.png)
  
* **own list** 
  - will label the first 20
  - colors might be different than expect if only labeling one direction of transcripts
  ![](IMAGES/2.Uploading.png)
  ![](IMAGES/2.Selected.png)
## Types of font
There are three font types available for the plot

This includes **Arial (Default)**, Times New Roman and Courier

## Cut-offs
P-value and log2 fold change can be altered in the plot. These are reactive numerical inputs. They will accept scientific formats i.e. 1e-5. 
inline equation: $A = \pi*r^{2}$
These are represented by the horizontal and vertical dotted lines on the graph (Default=grey)

![](cut-off.png)

## Axis paramaters
Select label
- p-value, FDR (false discovery rate; out-put of EdgeR) or adj p-value (which could include other p-value corrections)

axis 
- y-axis from 0 onward with the default being 100. To identify the top hit, look at the exponent on the P-value (i.e. 1e-50) and then add 5 to cover that point i.e. 55
- x-axis default from -10 to 10. 

axis break
- denotes how often the tick marks occur 
  - y-axis every 10
  - x-axis every 1

font size (range 0 to 100) 
- text size 
- number size 

## point parameters
point size (Default 3)
Coloring the dots
- no label (3 colors)
- Range of ID and Selected ID have up to 5 colors
  - if only one direction is being labeled the legend text can be changed to accommodate 4 parameters

Transparency of points
- There are 3 transparencies that can be altered
  - Non-significant (default = 0.25)
  - significant (default = 0.5)
  - labeled  (default = 1)

## ID Labels
These can either be the range or the own list of ID
the user can chose to alter
- the number of labels (default is 20, but would not recommend more than that due to crowding on the image)
- distance from the labeled point
- text size 

## Legend Parameters
These include:
- size of legend text
- location of legend (default=right)
- # of columns for the legend (default 1); recommend using 2-5 when the legend is below

## Other features
The title of the graph can be changed for export purposes

## explorting the graph
the user defined parameters will be the same as the exported graph. 
There are two download options. 
- PDF 
- png (default assumes legend is present at the 1200 by 1600; recommended to change to 1200 by 1200)
  - if the user wishes to increase the resolution, all point parameters will be affected. 
