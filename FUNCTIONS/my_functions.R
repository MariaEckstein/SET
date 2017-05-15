#######################
#### Load packages ####
#######################

#install.packages('Hmisc'); install.packages('ggplot2'); install.packages('reshape'); install.packages('car'); install.packages('mlogit'); install.packages('pastecs'); install.packages('psych'); install.packages('polycor'); install.packages('boot'); install.packages('ggm'); install.packages('QuantPsyc'); install.packages('WRS'); install.packages("compute.es"); install.packages("effects"); install.packages("lm4"); install.packages("pwr")

#library(WRS)
library(Hmisc); library(ggplot2); library(reshape); library(car); library(mlogit); library(pastecs); library(psych); library(polycor); library(boot); library(ggm); library(QuantPsyc); library(plyr); require(scales); library(ez); library(nlme); library(compute.es); library(effects); library(RColorBrewer); library(lme4); library(pwr); library(caret)
#library(multcomp)
#source('http://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v25.txt')

theme_set(theme_bw())   #make ggplot theme_bw()


##########################
#### Define variables ####
##########################

subjects <- c(3:31, 33:39, 41); bin.time <- 0.05; times <- round(seq(0.05, 9, bin.time), 2); spans <- c('0span', '1span', '2span', '3span'); items <- c('Item 1', 'Item 2', 'Item 3', 'Item 4'); windows <- c('w1', 'w2', 'w3', 'w4', 'after')
#excluded <- c(4, 7, 10, 13, 16, 17, 22, 25, 30, 36)   #as of 14-07-29 (37 subjects)
excluded <- c(4, 7, 10, 16, 22, 25, 30, 36)   #as of 14-07-29 (37 subjects)
excluded <- c(4, 10, 30, 36)   #THAT'S WHAT IT SAYS IN THE PAPER!!!!!
included <- subjects[!subjects %in% excluded]
#linear_group <- c(6, 14, 15, 20, 29, 33); ushape_group <- c(8, 9, 12, 18, 19, 21, 26, 35, 41)   #as of 14-07-29 (37 subjects)
#linear_group1 <- c(6, 33); linear_group2 <- c(3, 6, 9, 20, 23, 28, 29, 33, 37, 39); ushape_group1 <- c(3, 5, 8, 9, 14, 18, 26, 27, 28, 35, 37, 38, 41); ushape_group2 <- c(5, 18, 19, 21, 24, 26, 35, 41)   #as of 14-08-11 (37 subjects; separated into two blocks)
#linear_group1 <- c(6, 7, 17, 25, 33); linear_group2 <- c(3, 4, 6, 9, 17, 20, 23, 28, 29, 30, 33, 37, 39); ushape_group1 <- c(3, 5, 8, 9, 13, 14, 16, 18, 26, 27, 28, 35, 36, 37, 38, 41); ushape_group2 <- c(5, 16, 18, 19, 21, 22, 24, 26, 35, 41)   #as of 14-08-14 (ALL 37 subjects, nobody excluded, separated into two blocks)
linear_group1 <- c(6, 17, 33); linear_group2 <- c(3, 6, 9, 17, 20, 23, 28, 29, 33, 37, 39); ushape_group1 <- c(3, 5, 8, 9, 13, 14, 18, 26, 27, 28, 35, 37, 38, 41); ushape_group2 <- c(5, 18, 19, 21, 24, 26, 35, 41)   #as of 14-08-25 (10 excluded subjects, separated into two blocks)
neither_group1 <- included[!included %in% c(linear_group1, ushape_group1)]; neither_group2 <- included[!included %in% c(linear_group2, ushape_group2) & included != 15]
lin_lin <- c(6, 17, 33); lin_u <- vector(); u_u <- c(5, 18, 26, 35, 41); u_lin <- c(3, 9, 28, 37)   #as of 14-08-11 (37 subjects, separated into two blocks)
#nlinear_group1 <- c(3, 13, 24, 27, 29, 33); nlinear_group2 <- c(3, 9, 20, 31, 33, 35, 37); nushape_group1 <- c(21, 26, 28, 37, 41); nushape_group2 <- c(5, 14, 19, 21, 24, 26, 28, 41)

# Exclude participants with < 60% correct #
qbinom(p=.95, size=60, prob=.5)/60

#########################
#### Load Dataframes ####
#########################

questionnaires <- read.csv('C:/Users/Maria/Documents/MEGAsync/NewData/seqSET/rawdata/filtered_and_interpolated_files/outputfiles2/binned rolling fixation separate (3sec-4.5sec)/R Outputs/14-07-28seqSET_questionnaires.csv')
#all_files <- read.csv('C:/Users/Maria/Documents/MEGAsync/NewData/seqSET/rawdata/filtered_and_interpolated_files/outputfiles2/binned rolling fixation separate (3sec-4.5sec)/R Outputs/allfiles.csv')
all_subj.span.averages <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Spans differ in pupil dilation/new_subj_span_averages.csv')
#block_subj.span.averages <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Spans differ in pupil dilation/block_subj_span_averages.csv')
all_subj.noSET.averages <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Spans differ in pupil dilation/new_subj_noSET_averages.csv')
block_subj.noSET.averages <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Spans differ in pupil dilation/block_subj.noSET.averages.csv')
vals.peaks <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Intake and Lures/trial.intakes.csv')
av.vals.peaks <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Intake and Lures/av.trial.intakes(weighted.new01).csv')
av.vals.peaks_block <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Intake and Lures/av.trial.intakes_block(weighted.new02).csv')
av.intake.diff <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Intake and Lures/av.trial.intakediff.csv')
easy_intakes <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Intake and Lures/easy_intakes.csv')
easy_intakes_blocks <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Intake and Lures/easy_intakes_blocks.csv')
easy_intakes123 <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Intake and Lures/easy_intakes123.csv')
intake.incorr <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Intake and Lures/intake.incorr(new).csv')
summary_file <- read.csv('C:/Users/Maria/Documents/MEGAsync/NewData/seqSET/rawdata/filtered_and_interpolated_files/outputfiles2/binned rolling fixation separate (3sec-4.5sec)/R Outputs/14-07-19summary_file.csv')
pupildiffs <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Hypotheses about processing load during presentation/pupildiffs.csv')
difftests <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Hypotheses about processing load during presentation/difftests.csv')
RT_dprime_etc_block <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Behavioral/RT_dprime_etc_byspans_and_byblocks.csv')
RT_dprime_etc <- read.csv('C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Behavioral/RT_dprime_etc_byspans.csv')
col2 <- colorRampPalette(c('green', 'red'))
rel.pupil.time.span <- read.csv("C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Spans differ in pupil dilation/rel.pupil.time.spanSET.csv")
RT_lines2 = read.csv("C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Hypotheses about processing load during presentation/RT_lines2.csv")

### update excluded column in all the data frames
# all_subj.span.averages$excluded <- 0; all_subj.span.averages$excluded[all_subj.span.averages$Subject %in% excluded] <- 1
# write.csv(all_subj.span.averages, 'C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Spans differ in pupil dilation/new_subj_span_averages.csv')
# block_subj.span.averages$excluded <- 0; block_subj.span.averages$excluded[block_subj.span.averages$Subject %in% excluded] <- 1
# write.csv(block_subj.span.averages, 'C:/Users/Maria/Documents/MEGAsync/Berkeley/R scripts/sequentialset/Spans differ in pupil dilation/block_subj_span_averages.csv')

#### Variables inside my_functions
text.size <- 4   #for the functions that work on plots


####################################
#### dprime(hits, false.alarms) ####
dprime <- function(hits, false.alarms) {
  hits_corrected <- hits; hits_corrected[hits_corrected == 1] <- 0.95; hits_corrected[hits_corrected == 0] <- 0.05  
  false.alarms_corrected <- false.alarms; false.alarms_corrected[false.alarms_corrected == 1] <- 0.95; false.alarms_corrected[false.alarms_corrected == 0] <- 0.05
  qnorm(hits_corrected) - qnorm(false.alarms_corrected)
}


###########################################################
#### Add testwindows & what is on the screen to ggplot ####
ymin <- -.15; ymax <- .25
add.stuff <- function(ggplot.object, yaxmin = ymin, yaxmax = ymax) {
  testwindows <- data.frame(xmin = c(seq(1, 5.5, 1.5), 6), xmax = c(seq(1.5, 6, 1.5), 7), ymin = rep(-Inf, 5), ymax = rep(Inf, 5))
  response_line <- data.frame(x = c(6, 6), y = c(-Inf, Inf))
  ggplot.object +
    geom_rect(data = testwindows, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, inherit.aes = FALSE) +
    geom_line(data = response_line, aes(x = x, y = y), size = 1, inherit.aes = FALSE) +
    annotate('text', x = 0.5, y = yaxmin, label = 'Item1', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 2.0, y = yaxmin, label = 'Item2', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 3.5, y = yaxmin, label = 'Item3', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 5.0, y = yaxmin, label = 'Item4', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 6.75, y = yaxmin, label = 'Resp.', size = text.size, inherit.aes = FALSE) +
    annotate('text', x = 8.0, y = yaxmin, label = 'Fixation', size = text.size, inherit.aes = FALSE) +
    scale_x_continuous(breaks = seq(0, 9, 1.5)) +
    scale_y_continuous(breaks = seq(yaxmin, yaxmax, .05), labels = round(seq(yaxmin, yaxmax, .05), 2))
}



######################################################
#### Add Items and grey shades for encoding phase ####
add.encoding.phase <-
  function(ggplot.object, yaxmin = ymin, yaxmax = ymax, text.size = text.size) {
    encoding_window <- data.frame(xmin = 1.5, xmax = 4.5, ymin = -Inf, ymax = Inf)
    response_line <- data.frame(x = c(6, 6), y = c(-Inf, Inf))
    ggplot.object +
      geom_rect(data = encoding_window, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, inherit.aes = FALSE) +
      geom_line(data = response_line, aes(x = x, y = y), size = 1, color = 'blue', inherit.aes = FALSE) +
      annotate('text', x = 0.5, y = yaxmin, label = 'Item 1', size = text.size, inherit.aes = FALSE) +
      annotate('text', x = 2.0, y = yaxmin, label = 'Item 2', size = text.size, inherit.aes = FALSE) +
      annotate('text', x = 3.5, y = yaxmin, label = 'Item 3', size = text.size, inherit.aes = FALSE) +
      annotate('text', x = 5.0, y = yaxmin, label = 'Item 4', size = text.size, inherit.aes = FALSE) +
      annotate('text', x = 6.75, y = yaxmin, label = 'Response', size = text.size, inherit.aes = FALSE) +
      annotate('text', x = 8.0, y = yaxmin, label = 'Fixation', size = text.size, inherit.aes = FALSE) +
      scale_x_continuous(breaks = seq(0, 9, 1.5), labels = round(seq(0, 9, 1.5), 2)) +
      scale_y_continuous(breaks = seq(yaxmin, yaxmax, .05), labels = round(seq(yaxmin, yaxmax, .05), 2))
  }


    
####################################################
#### Add comparison bars and significance stars ####
add.comparisonbars <-
  function(ggplot_object, height_of_comparison_bars, upperest_comparison_bar, text_x.position_breadth_dataframe) {
    # get parameters for the first comparison bar
    d <- height_of_comparison_bars
    yo <- upperest_comparison_bar; yu <- yo - d; yt <- yo + d/2
    xm <- text_x.position_breadth_dataframe[1,2]
    xl <- xm - (text_x.position_breadth_dataframe[1,3] / 2)
    xr <- xm + (text_x.position_breadth_dataframe[1,3] / 2)
    xy <- data.frame(x.values = c(xl, xl, xr, xr), y.values = c(yu, yo, yo, yu))
    # add first comparison bar to the plot object
    ggplot_object <- ggplot_object +
      annotate("text", x = xm, y = yt, label = text_x.position_breadth_dataframe[1,1], inherit.aes = FALSE) +
      geom_path(data = xy, aes(x = x.values, y = y.values), inherit.aes = FALSE)
    # loop over the remaining comparison bars and do the same thing (get parameters & add to the plot object)
    for (row in 2:dim(text_x.position_breadth_dataframe)[1]) {
      yo <- yu - 2*d; yu <- yo - d; yt <- yo + d/2
      xm <- text_x.position_breadth_dataframe[row,2]
      xl <- xm - (text_x.position_breadth_dataframe[row,3] / 2)
      xr <- xm + (text_x.position_breadth_dataframe[row,3] / 2)
      xy <- data.frame(x.values = c(xl, xl, xr, xr), y.values = c(yu, yo, yo, yu))
      ggplot_object <- ggplot_object +
        annotate("text", x = xm, y = yt, label = text_x.position_breadth_dataframe[row,1], inherit.aes = FALSE) +
        geom_path(data = xy, aes(x = x.values, y = y.values), inherit.aes = FALSE) 
    }
    # return plotobject to plot it
    return(ggplot_object)
  }


########################
#### Add stars only ####
add.significance <-
  function(ggplot_object, text_x.position_y.position) {
    # loop the dataframe to get parameters and add stars to the plot object
    for (row in 1:dim(text_x.position_y.position)[1]) {
      xt <- text_x.position_y.position[row, 2]; yt <- text_x.position_y.position[row, 3]; text <- text_x.position_y.position[row, 1]
      ggplot_object <- ggplot_object +
        annotate("text", x = xt, y = yt, label = text, size = text.size, inherit.aes = FALSE)
    }
    # return plotobject to plot it
    return(ggplot_object)
  }



#################################################
#### Adjust Means for within-subject Designs ####
adjust.means <-
  function(dataframe_wide, colnames, melt = F) {
    # Calculate the mean of all conditions (as indicated in colnames) for each row = subject
    col_sum <- data.frame(rep(0, dim(dataframe_wide)[1])); for (col in colnames) {col_sum <- col_sum + dataframe_wide[, col]}
    condition_mean <- col_sum / length(colnames)
    # Calculate the grand mean
    grand_mean <- mean(condition_mean[[1]], na.rm = T)
    # Calculate the adjustment
    adjustment <- grand_mean - condition_mean
    # Calculate adjusted values for all conditions
    adj_columns <- data.frame(rep(NA, dim(dataframe_wide)[1])); for (col in colnames) {adj_col <- dataframe_wide[, col] + adjustment; adj_columns <- cbind(adj_columns, adj_col)}; adj_columns <- adj_columns[,2:dim(adj_columns)[2]]
    newnames <- vector(); for (col in colnames) {newname <- paste(col, 'adj', sep = '_'); newnames <- c(newnames, newname)}; colnames(adj_columns) <- newnames
    # Attach the adjusted colums to the original dataframe_wide
    new_dat <- cbind(dataframe_wide, adj_columns)
    # If melt is set to TRUE, melt the dataframe so that 
    if (melt == T) {new_dat <- melt(new_dat, id = colnames(dataframe_wide)[!colnames(dataframe_wide) %in% colnames], measured = colnames)}
      
    return(new_dat)
  }


###########################
#### t.test.from.means ####
t.test.from.means <-
  function(m1, m2, sd1, sd2, n1, n2) {
    df <- n1 + n2 - 2
    poolvar <- (((n1 - 1) * sd1^2) + ((n2 - 1) * sd2^2)) / df
    t <- (m1 - m2) / sqrt(poolvar * ((1 / n1) + (1 / n2)))
    sig <- 2 * (1 - (pt(abs(t), df)))
    paste("t(", df, ") = ", t, ", p = ", sig, sep ="")
  }


##############################
#### Effect size r from t ####
r_from_t <-
  function(t, df) {
    r <- sqrt((t^2) / (t^2 + df))
    d <- (t * 2) / sqrt(df)
    return(r)
  }

#################################################################
#### Calculate SE for ggplot stat_summary (fun.data = "cSE") ####
SE = function(x) {
  result = c(mean(x) - sd(x) / length(x), mean(x) + sd(x) / length(x), mean(x))
  names(result) = c("ymin", "ymax", "y")
  result
}
