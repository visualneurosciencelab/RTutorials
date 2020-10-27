######################################################################
################# == Load necessary packages == ######################
######################################################################

### Let's start by loading the packages we need...

library(tidyverse) # A suite of packages used for data wrangling + data viz
library(ggforce)   # Creating multi-paged PDFs

#####
#####
#####
######################################################################
################## == Import CSV's == ################################
######################################################################

### Now, let's import the data we're interested in analyzing...
###   We'll start by assigning the file paths of our 2 CSV's
###   to unique objects in R.
###   
### Getting the file path for a CSV is relatively straightforward: 
###
###       1.) Navigate to your CSV of interest.
###
###       2.) Copy the file path to your CSV. 
###
###                   A.) On PC, hold down the 'shift' key and right click on the 
###                          CSV for which you want the file path. Then, click on 
###                          the 'Copy As Path' drop-down option. 
###
###                   B.) On MacOS, do the following; 
###
###                             i.) Click on 'Finder'.
###
###                            ii.) Click 'View' in the upper bar.
###
###                           iii.) Click 'Show the Path Bar'.
###
###                            iv.) Now, 'Control' + Click the file you want the location for.
###
###                             v.) Hold the 'Option' key. 
### 
###                            vi.) Click on 'Copy .. as Pathname' from the drop-down menu. 
###
###       3.) Paste the file path into your R window.  
###        
###       4.) Modify the original file path in the following manner...
###
###             Original path: "your\file\path\to\the.csv" --> R doesn't recognize single back slashes 
###
###             Modified Path: 
###            
###                    A.) "your\\file\\path\\to\\the.csv" --> R recognizes double back slashes
###
###                         OR
###
###                    B.) "your/file/path/to/the.csv"  --> R recognizes single forward slashes
###           
### First, we'll assign the file path to the CSV of the  Kang data set
###   to an object called 'data.path'.

data.path <- 'C:\\Users\\dezia\\Dropbox (Kathy Murphy)\\Lab SARS-COV-2 Brain Development\\Lab SARS-COV-2 Brain Development Excel\\Kang Data\\Kang Data for Our 37 Genes and 16 Regions\\COVID Data - 37 Genes - 1340 Samples - 16 Regions - log2 and z-scored - Long.csv'

### Next, we'll store the file path to a CSV containing the names of 37 genes of interest 
###   to an object called 'gene.path'. 

gene.path <- 'C:\\Users\\dezia\\Dropbox (Kathy Murphy)\\Lab SARS-COV-2 Brain Development\\Lab SARS-COV-2 Brain Development Excel\\Kang Data\\Kang Data for Our 37 Genes and 16 Regions\\Vector of 37 COVID-related Genes.csv'

# The Kang data has 5 columns and 47397 rows
#
#   Columns include:
#       
#         1.) Sample -- Sample IDs
#         2.) Region -- The brain region from which a measurement was taken
#         3.) Years -- The age of the sample
#         4.) Gene -- The name of the gene being measured
#         5.) Expression -- The Z-Scored & log2 transformed gene measurement 

### Now, to import the Kang data into R, we're gunna take 'data.path'  and 
###    pass it through the aptly named 'read_csv()' function. The output of
###    that function is going to be the full Kang data set stored as a tibble
###    which is just a unique data structure in R that's great for working
###    with diverse data sets (ex. variables measured on nominal, 
###    ordinal, interval and ratio scales). Finally, we'll store the Kang data
###    in an object called 'Our.Genes.zscore.Long' so that we don't need to 
###    continuously import the data when we need it. 


Our.Genes.zscore.Long <- data.path %>%  # File path object
  
                          read_csv()    # Import CSV into R


head(Our.Genes.zscore.Long)
tail(Our.Genes.zscore.Long)


### Side note: the %>% symbol is called a 'pipe operator' and it's used to write
###      readable and efficient code in R. It works by taking the object to the 
###      right of the %>% symbol and passing it through the function to its left. 


###  We'll then import the CSV of 37 gene ID's by taking 'gene.path', passing it through
###    'read_csv()', converting it into a vector, and storing it in an object called 'genes'.


genes <- gene.path %>%    # File path object
  
          read_csv() %>%  # Import CSV into R as a tibble
  
          unlist() %>%    # Convert from tibble to a named vector
  
          as.vector()     # Convert from named vector to unnamed vector 

genes


#####
#####
#####
######################################################################
################# == Generate LOESS Curves == ########################
######################################################################

##### Genes (bare plot)
###
### Now that we've imported our data, let's go ahead and make some sample LOESS curves by visualizing 
###   the expression of the ACE2 gene. We'll make 1 figure that displays a curve for ACE2 expression
###   in all 16 brain regions explored in the Kang data set. Therefore, our plot will have 16 curves.
###
### We'll construct LOESS curves using the ggplot2 package in R. This package is one of the 
###   biggest reasons why people use R, as it allows you to construct high quality figures with ease.
###   Ggplot2 is built upon a framework called 'the grammar of graphics'. While this framework
###   could warrant a lecture in and of itself, we'll focus making some plots.
###   Here is the minimum code needed to create a LOESS curve...

head(Our.Genes.zscore.Long)

Our.Genes.zscore.Long %>%       ### Call upon the Kang data
  
  filter(Gene == 'ACE2') %>%    ### Specify that we only want data from the 'ACE2' gene
  
  ggplot(aes(x = Years,         ### Declare that we want to make a ggplot figure with data  
             y = Expression,    ###    from the 'Years' and 'Expression' columns mapping onto the X- and
             color = Region)) + ###    Y-axis, and to colour the curves according to the 'Region' columns
  
  geom_smooth(method = 'loess') ### Specify that we want to visualize our data with a 'smooth curve' by
                                ###    calling upon 'geom_smooth()' function. Geoms are used by ggplot2 
                                ###    to specify the creation of certain graphs (ex. box plots, bar charts,
                                ###    scatter plots, violin plots, etc.)
                                ###
                                ### To get a LOESS curve, specifically, you just need 
                                ###   pass a value of 'loess' to the 'method =' argument
                                ###   of 'geom_smooth()'.


##### Genes (removing confidence intervals)

### We can see that the curves are surrounded by a grey cloud and that these clouds overlap and obscure
###   one another. Those grey clouds are the 95% confidence intervals for a given LOESS curve. Let's go
###   ahead get rid of the confidence intervals so that we have JUST the curves appearing on our plot. 

Our.Genes.zscore.Long %>% 
  
  filter(Gene == 'ACE2') %>% 
  
  ggplot(data = .,
         aes(x = Years,
             y = Expression,
             color = Region)) +
  
  geom_smooth(method = 'loess',
              se = FALSE)       # To remove the confidence intervals,  just 
                                #   pass a value of FALSE through the 'se =' argument
                                #   of 'geom_smooth()'.

##### Genes (storing minimum code)
###
### Let's store the minimum chunk of code in its own object, which we'll call 'p'. Doing this 
###    makes your code more efficient, as you don't need to write out a long script and you 
###    can still build upon this basic plot. 

plot <- Our.Genes.zscore.Long %>% 
  
      filter(Gene == 'ACE2') %>% 
      
      ggplot(data = .,
             aes(x = Years,
                 y = Expression,
                 color = Region)) +
      
      geom_smooth(method = 'loess',
                  se = FALSE)

### Let's see what happens when we execute the code in 'p'

plot

##### == Genes (completed sample plot)
###
### Now, we'll take this bare plot and do 4 things to it. We'll:
###   
###     1.) Log2 transform our X-axis
###     2.) Add some custom tick marks
###     3.) Change the plot background to a black and white grid
###     4.) Rotate the X-axis tick marks 90 DEG clockwise

plot +
  scale_x_continuous(trans = "log2", ### log2 transforms the X-axis.
                     
                     breaks = c(0, 10/52, 20/52, 40/52, 40/52 + 2/12, ### Specifies where along the X-axis 
                                40/52 + 6/12, 92/52, 40/52 + 2,       ###    to add custom tick marks.
                                40/52 + 4, 40/52 + 8, 40/52 + 16,
                                40/52 + 32, 40/52 + 64, 40/52 + 90),
                     
                     labels = c("0 PCW", "10 PCW", "20 PCW", "0",        ### Specifies what to call the custom
                                "2 M", "6 M", "1 Y", "2 Y", "4 Y",       ###    X-axis tick marks. 
                                "8 Y", "16 Y", "32 Y", "64 Y", "90 Y")) +
  
  theme_bw() + ### Changes the plot background to black and white grids
  
  theme(axis.text.x = element_text(angle = 90)) ### Rotate custom X-axis 90 Degrees

##### == Genes (generating PDFs)
###
### After we make figures in R, we can export them as JPEGs, PNGs, TIFFs, and more. To end off, 
###   let's go through a simple example of making a PDF for each of our 37 genes' expression 
###   across all 16 brain regions. We'll do just that by making a single PDF with 37 pages, where 
###   each page will display one of our genes
###
### This task requires us to draw upon some computer programming concepts, specifically, while and for-loops.
###   To help you understand the logic of the code which will generate this PDF, I've provided some pseudo-code
###   that walks you through how the code works.
###
#### Pseudo-code for generating PDFs...
###
### while (TRUE) { 
###
###   - Make a new PDF called 'example_ZScored_Gene_LOESS_Trajectories.pdf'.
###
###         for (1 to the number of genes we're interested in -- 37) {
###
###                 - print the ID of the current gene in the 'genes' vector within
###                         the R console 
###                   
###                 - plot LOESS curves for the current gene across all 16 brain regions on
###                         a new page of the PDF.
###                 } 
###
###   - Download the PDF in its specified location on your computer
###
###   - Break out of the while-loop
###
### } End of code
###
####

head(Our.Genes.zscore.Long)
tail(Our.Genes.zscore.Long)

while (T) {
  
  pdf('C:\\Users\\dezia\\Dropbox (Kathy Murphy)\\Tutorials\\Samples\\example_ZScored_Gene_LOESS_Trajectories.pdf')
    # The pdf() function allows you to specify the file path to, and the name of the PDF you 
    #            want to generate.   
  
  for (i in 1:length(genes)) {
    
    print(genes[i])
    
    print(Our.Genes.zscore.Long %>% 
            
            filter(Gene == genes[i]) %>% ### The 'genes[i]' term iterates across entries in the 'genes' 
                                         ###      vector. 
            
            ggplot(data = .,
                   aes(x = Years,
                       y = Expression,
                       color = Region)) +
            
            geom_smooth(method = 'loess',
                        se=F) +
            
            scale_x_continuous(trans = "log2", 
                               breaks = c(0, 10/52, 20/52, 40/52, 40/52 + 2/12,
                                          40/52 + 6/12, 92/52, 40/52 + 2,
                                          40/52 + 4, 40/52 + 8, 40/52 + 16,
                                          40/52 + 32, 40/52 + 64, 40/52 + 90),
                               labels = c("0 PCW", "10 PCW", "20 PCW", "0",
                                          "2 M", "6 M", "1 Y", "2 Y", "4 Y", 
                                          "8 Y", "16 Y", "32 Y", "64 Y", "90 Y")) +
            theme_bw() +
            
            theme(axis.text.x = element_text(angle = 90)) +
            
            facet_wrap(~Gene))
    
  }
  
  print(
        dev.off() # Renders the PDF after it's been generated.
        )

  break
    
}

### We've successfully generated a PDF document with 37 x 16 = 592 LOESS curves, and because 
###     LOESS curves are a form of polynomial regressions, we've just performed 592
###     regressions with 42 lines of code. This is just an example of how powerful R is and how we
###     can use it to study brain development across the lifespan. Thanks for attending this tutorial!
###     The code used in this will be made freely available on the visual neuroscience lab GitHub
###     account. I encourage you all to contribute to the GitHub as it's optimized for software development,
###     and allows you to host R packages. 
###
### Before we finish up, are there any questions?

######################################################################
################# == End of Tutorial == ##############################
######################################################################