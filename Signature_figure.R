library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(cowplot)

#Input file for this code is:
#ID\tSignature\tMean
#P-00XX		1	0.456834502038147
#P-00XX		3	0.135175099888683
#M-00XX		4	0.111076246238745
#No column header is needed. 

#=================== FUNCTIONS TO MANIPULATE ===================
"Check files for number of columns
df is the data frame
cols_df is the number of expected columns
"
check_files<-function(df, cols_df)
{
  if (ncol(df) == cols_df)
  {
    return(0)
  }else
  {
    return(1)
  }
}


"Create pdf file name"
create_pdf<-function(time_stmp)
{
  file_name = paste0("Signature_", time_stmp,".pdf")
  return(file_name)
}

"Exit the whole code"
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}

exit_if <- function(sig_file){
  #End if file contains less than 3 columns 
  if (check_files(sig_file, 3))
  {
    print ("This is not the right file")  
    exit()
  }else{
    print ("Everything looks good")
  }
  
}

"Name the columns of the data frame"
name_columns<-function(df)
{
  #Name the columns
  colnames(df) =c("id", "signature", "mean")
  return(df)
}

"Read the user input into a data frame"
read_file <- function(file_name)
{
  read.table(file_name , sep="\t", header=FALSE,stringsAsFactors = FALSE)
}

"Find the date"
find_time <-function()
{

  ttime = format(Sys.time(), tz="")
  
  return(ttime)
}

#========================================================================  
#===================== FUNCTIONS FOR DATAFRAME MANIPULATION =============
"Arrange the data frame according to descending order of mean
and  create a new dataframe
"
arrange_df <-function(df,mean,id)
{
  b1<-df %>% arrange(desc(mean)) %>% group_by(id)
  df1 <- b1 %>% arrange(id, mean)
  write.table(df1, file="temp", sep="\t", row.names = FALSE, quote = FALSE)
  df2= read.table("temp", sep="\t",header=TRUE)
  df3 = df2 %>%
    mutate(mean_int2 = case_when(mean >0.01 ~ df1$mean*100,
                                 mean < 0.01 ~ df1$mean *100))
  
  unlink("temp")
  return(df3)
}

"Find the most occuring signature from 1 to 30"
most_frequent_signature<-function(new_df, n=30)
{
  hh1.new =new_df[seq(n, nrow(new_df),n), ]
  frq_cnt_sig = hh1.new %>% count(signature) %>% arrange(desc(n))
  return(frq_cnt_sig)
}

"Select samples  from the ordered list"
select_samples<-function(j1, df)
{
  kk1<-data.frame()
  for (i in 1:nrow(unique(j1))){
    for (j in 1:nrow(df)){
    if (j1[i,][1] == df[j,][1]){
      kk1<-rbind(kk1, (df[j,]))
    }
    }
    }
  return(unique(kk1))
}

#Order samples 
order_samples<-function(new_df,order_sig)
{
  n<-data_frame()
  #Pull all those samples that have signatures starting from the signature order
  #And then order them according to their greatest mean  and put it in a df n 
  for(i in 1:nrow(Signature_order)){
    
    n<- rbind(n,new_df[new_df$signature == order_sig[i],] %>%  arrange(desc(mean_int2)), stringsAsFactors=FALSE)
  }
  return(n)
}

#=========================================================================

#================ FUNCTIONS FOR COLOR ====================================
"Colors for each  signature.
Make more palettes 
#####colourCount = length(unique(x$signature))
#####getPalette = colorRampPalette(brewer.pal(9, \"Spectral\"))
Made a list of colors so that they do not overlap 
"
select_color<-function()
{
  new_colors = c(brewer.pal(n=8, name="Set2"), 
                 brewer.pal(n=8, name="Accent"), 
                 brewer.pal(n=10, name="Spectral"), 
                 brewer.pal(n=4, name="Dark2"))
  return(new_colors)
}

"Make a legend"
make_legend<-function()
{
  #Make legend for signatures separately
  leg <- as.data.frame( seq(1,30, by=1) )
  sig <-as.data.frame(seq(1,30, by=1))
  sig1 <-cbind(sig, leg)
  colnames(sig1)=c("xaxis","Signature")

  #  + 
  legend = ggplot(sig1, aes(x=xaxis, y=1, fill=as.factor(Signature), label = Signature)) + geom_point(shape=22, aes(size=1))+
           scale_fill_manual(values=new) + 
           geom_text(check_overlap = TRUE, vjust=-1, hjust=-1.5)+
           theme(axis.text =  element_blank(), axis.ticks = element_blank(),panel.grid= element_blank(), axis.title = element_blank(),axis.line = element_blank(), legend.position = "None") +
           coord_flip()
  return(legend)
  
}

#========================================================================


sig_file=read_file("test_data_mutational_sig")

exit_if(sig_file)
#Name the columns
sig_file = name_columns(sig_file)
new_df   = arrange_df(sig_file,mean, id)

count_sig = most_frequent_signature(new_df)
Signature_order <- matrix(dplyr::pull(count_sig, 1))
ordered_sample_sig = order_samples(new_df, Signature_order)


#Get all the samples ordered with the right signature order
ordered_samples  = select_samples(ordered_sample_sig, new_df )

#Make new colors and create pdf 
new_colors = select_color()
tday =find_time()
create_pdf(tday)


#Create the PDF
pdf(create_pdf(tday))
#order the samples as is! So convert into factors
ordered_samples$id <- factor((ordered_samples$id), levels = as.character(unique(ordered_samples$id)))

mutational_sig_plot = ggplot(ordered_samples , aes(x=id, y=-mean_int2, fill=signature),colour="black") + 
                      geom_bar(stat="identity",width=0.7, colour="black",aes(colour="black", linetype="dotdash")) +
                      theme(axis.text=element_text(angle=0, hjust=1)) + 
                      scale_fill_gradientn(colours=new_colors)+ 
                      theme(panel.background = element_blank(),  axis.text = element_text(hjust=1, angle=0, size=9), axis.line = element_blank(),axis.ticks = element_blank(), legend.position = "None") + 
                      scale_y_continuous(labels=c("-100" = "0", "-50" = "25", "-75" = "50", "-25"= "75", "0" ="100"))+
                      ylab("Mean of the Signatures") + 
                      guides(fill=guide_colourbar(nbin=30)) +
                      coord_flip()
  legend = make_legend()
  plot_grid(mutational_sig_plot, legend,  rel_widths = c(3, 1))
dev.off()


