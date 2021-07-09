# Corrleation plot

This is the README.md file containing information on the features of the application.

Please contact: Chen.Li@monash.edu or Kerry.Mullan@monash.edu to report errors.

If using the local GitHub, run the following command in R or Rstudio to download and install the required packages. 
```
install.packages(c("tidyverse", "ggplot2", "ggrepel", "shiny", "shinyBS", "gridExtra", "DT", "plyr", "dplyr", "reshape2"))
```
## File types accepted

The file must contain headers: ID, logFC and Pvalue. 

Unique ID names are preferred especially when labelling the graph.

This needs to be in the form of either a **.csv** or **.txt** file.

The test-data includes 'proteomics.csv' and 'Transcriptomics.csv' files from Gonglaves et al. (2021)

<img src="IMAGES/uploading.cor.png" width="400">

## Font Available

This includes **Arial (Default)**, Times New Roman and Courier.

<img src="IMAGES/font.png" width="400">

## Axis labels and cut-offs



## Packages cited 
Auguie, B., A. Antonov and M. B. Auguie (2017). "gridExtra: Miscellaneous Functions for "Grid" Graphics. R package version 2.3. https://CRAN.R-project.org/package=gridExtra."

Bailey, E. (2015). "shinyBS: twitter bootstrap components for Shiny. R package version 0.61. https://CRAN.R-project.org/package=shinyBS."

Chang, W., J. Cheng, J. Allaire, Y. Xie and J. McPherson (2020). "shiny: Web Application Framework for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny."

Slowikowski, K. (2020). ggrepel: Automatically Position Non-Overlapping Text Labels with’ggplot2’.

Villanueva, R. A. M. and Z. J. Chen (2019). ggplot2: Elegant graphics for data analysis, Taylor & Francis.

Wickham, H. (2007). "Reshaping data with the reshape package." Journal of statistical software 21(12): 1-20.

Wickham, H. (2011). "The split-apply-combine strategy for data analysis." Journal of statistical software 40(1): 1-29.

Wickham, H., M. Averick, J. Bryan, W. Chang, L. D. A. McGowan, R. François, G. Grolemund, A. Hayes, L. Henry, J. Hester, M. Kuhn, T. L. Pedersen, E. Miller, S. M. Bache, K. Müller, J. Ooms, D. Robinson, D. P. Seidel, V. Spinu, K. Takahashi, D. Vaughan, C. Wilke, K. Woo and H. Yutan (2019). Welcome to the tidyverse, Welcome to the tidyverse. 4: 1686.

Wickham, H., R. François, L. Henry and K. Müller (2020). dplyr: a grammar of data manipulation.

Xie, Y., J. Cheng and X. Tan (2020). DT: A Wrapper of the JavaScript Library “DataTables”.
