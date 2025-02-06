#! /usr/bin/env Rscript

library(argparse)
library(ggplot2)
library(readxl)
library(tidyverse)


##### SET UP PARSER
#sets argument parser for adding command line arguments
parser<- ArgumentParser(description ='Generate polysome profile plots from raw data')

#limits used to define polysome boundries
parser$add_argument("-l","--limits", action = 'append', default = c(200,550),
                    help = "Sets bounds for x coordinates")

# sets list of polysome profile data in excel format. Default is for debugging
parser$add_argument("-f", "--files", action = 'append', default = c("A/B/C.txt","A/B/D.txt","A/B/E.txt"),
                    help = "List of polysome files in xlsx format. Default is for debugging")


parser$add_argument("-c", "--comparisons", action = "append", default = NULL
                    help = "Polysome profiles to be compared. \n Should be entered as comparison1_comparison2")

parser$add_argument("-a", "--align", default = "no"
                    help = "Should polysomes be aligned? (yes/no). \n Only works if monosomes are the largest peak.")


#turns args into a vector like object (?). Arguments can be referenced with args$ now
args<- parser$parse_args()

##### FUNCTIONS
# extracts sample names from file paths in sample.txt format
extract_sample_names <- function(file){
  result <- strsplit(file, split = "/")
  sample_ext<- sapply(result, function(x) tail(x,1))
  sample_name <- gsub("\\.txt$", "", sample_ext)
  return(sample_name)
}

# creates polysome profiling dataframe
polysome_read<-function(file,limits){
  x<-read_xlsx(file,col_names = c("dist","abs","fract"))
  y<-x%>%filter(between(dist,args$limits[1],args$limits[2]))%>%
    mutate(abs = abs-min(abs))
  return(y)
}

peak_finder<-function(absorbances){
  max_indices<- order(my_vector, decreasing = TRUE)[1:4]
  max_values<-sort(absorbances, decreasing = TRUE)[1:4] 
}

align_monosomes<-function(peaks){
  if (str(args$align) == "yes"):
  else:
    return(0)
}

#creates a polysome plot of two profiles
double_plot<-function(data1,data2){
  ggplot()+
  theme_classic()+
  theme(panel.border = element_rect(color="black",size=2,fill=NA))+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.y = element_text(size=16,color="black"))+
  theme(axis.ticks.y = element_line(size=1))+
  theme(axis.title = element_text(size=16,color="black"))+
  theme(plot.title = element_text(size=14,color="black", hjust = 0.5))+
  geom_line(data=data1,aes(x=dist,y=abs),color="#4682B4",
            linewidth=1)+
  geom_line(data=data2,aes(x=dist,y=abs),color="#B22222",
            linewidth=1)+
  ylab("UV Absorbance")+coord_cartesian(xlim=c(args$limits[1],args$limits[2]),ylim=c(0,0.10))+
  xlab("")
  }


##### RUNTIME
# assigns polysome dataframe to each sample name as a global variable
for (i in args$files){
  name <- extract_sample_names(i)
  polysome<- polysome_read(i, args$limits)
  assign(name, polysome)
}







#DEPRACATED
# for (i in 1:length(args$files)){
#   result<- strsplit(args$files[i], split = "/")
#   last_items <- sapply(result, function(x) tail(x,1))
#   last_names <- gsub("\\.txt$", "", last_items)
#   print(last_names)
# }
