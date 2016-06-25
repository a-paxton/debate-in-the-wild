#### libraries-and-functions_debate-in-the-wild.r: Part of `debate_in_the_wild.Rmd` ####
#
# This script sets the working directory, loads libraries, creates a number of 
# additional functions to facilitate data prep and analysis.
#
# Written by: A. Paxton (University of California, Berkeley)
#
#####################################################################################

#### Set working directory ####
setwd('/debate-in-the-wild/')

#####################################################################################

#### Load libraries we'll need ####
library(doBy)
library(dplyr)
library(e1071)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(gtools)
library(Hmisc)
library(kernlab)
library(languageR)
library(lme4)
library(pander)
library(piecewiseSEM)
library(purrr)
library(plotrix)
library(plyr)
library(tsne)

#####################################################################################

#### Create functions we'll need ####

# "%notin%": identify values from one list (x) not included in another (y)
'%notin%' <- function(x,y) !(x %in% y) 

# "pander_lme": simplify lme4 printouts (available on GitHub: https://github.com/a-paxton/stats-tools)
pander_lme = function(lme_model_name, stats.caption){
  
  # load in pander
  library(pander)
  
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lme_model_name)$coefficient)
  
  # get it to display p-values and asterisks based on significance
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .1] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  # set a caption that includes R-squared values
  if (stats.caption == TRUE){
    
    # use piecewiseSEM to calculate R-squared
    library(piecewiseSEM)
    model_marginal_r_squared = sem.model.fits(lme_model_name)$Marginal
    model_conditional_r_squared = sem.model.fits(lme_model_name)$Conditional
    neat_caption = paste('**Marginal *R*-squared: ',
                         round(model_marginal_r_squared,2), 
                         "; Conditional *R*-squared: ",
                         round(model_conditional_r_squared,2), ".**",sep="")
    
    # return the table
    return(pander(neat_output, split.table = Inf, caption = neat_caption, format = 'rmarkdown'))
  } else { # or return a table without it
    return(pander(neat_output, style="rmarkdown",split.table = Inf, format = 'rmarkdown'))
  }
}

# "pander_lm": simplify lm printouts and include adjusted R-squared and F-stats
pander_lm = function(lm_model_name, stats.caption){
  
  # load in pander
  library(pander)
  
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lm_model_name)$coefficient)
  
  # get it to display p-values and asterisks based on significance
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .1] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  # set a caption that includes R-squared values
  if (stats.caption==TRUE){
    
    # grab stats F-stats and adjusted R-squared
    model_adj_r_squared = summary(lm_model_name)$adj.r.squared
    model_fstatistics = summary(lm_model_name)$fstatistic
    neat_caption = paste('**Adjusted *R*-squared: ',
                         round(model_adj_r_squared,2), "; *F*(",
                         model_fstatistics[2],",",model_fstatistics[3],
                         ") = ",round(model_fstatistics[1],2),"**",sep="")
    
    # return the table
    return(pander(neat_output, split.table = Inf, caption = neat_caption, format = 'rmarkdown'))
  }else{# or return a table without it
    return(pander(neat_output, style="rmarkdown",split.table = Inf, format = 'rmarkdown'))
  }
}

# "significant.effects": identify significant effects in a model
significant.effects = function(lm_model_name){
  
  # convert the model summary to a dataframe
  model.summary = data.frame(summary(lm_model_name)$coefficient)
  
  # create significance values
  model.summary$p = 2*(1-pnorm(abs(model.summary$t.value)))
  
  # identify only effects below p = .05
  significant.effects = data.frame(model.summary[model.summary$p <= .05,])
  significant.effects$component = rownames(significant.effects)
  
  # grab only the component name and p-value, then remove row names
  significant.effects = significant.effects %>%
    select(component,p)
  rownames(significant.effects) = seq(length=nrow(significant.effects))
  
  # return the names of the significant effects
  return(pander(significant.effects, format = 'rmarkdown'))
}

# "trending.effects": identify trends in a model (.05 < p <= .10)
trending.effects = function(lm_model_name){
  
  # convert the model summary to a dataframe
  model.summary = data.frame(summary(lm_model_name)$coefficient)
  
  # create significance values
  model.summary$p = 2*(1-pnorm(abs(model.summary$t.value)))
  
  # identify only effects between .05 < p <= .10
  trending.effects = data.frame(model.summary[model.summary$p > .05 &
                                                model.summary$p <= .10,])
  trending.effects$component = rownames(trending.effects)
  
  # grab only the component name and p-value, then remove row names
  trending.effects = trending.effects %>%
    select(component,p)
  rownames(trending.effects) = seq(length=nrow(trending.effects))
  
  # return the names of the significant effects
  return(pander(trending.effects, format = 'rmarkdown'))
}


# 'component.factors': a function to show the bottom or top factors in a given component
component.factors = function(original.df, svd.df, factor.list, component, top.bottom){
  
  # subset original dataframe to include only those in our list of factors
  subset.data = original.df[names(original.df) %in% factor.list]
  
  # specify the top/lowest n values
  slice.n = 5
  
  # create name
  component.name = paste('c',component,sep='')
  
  # use the option to decide whether to...
  if (top.bottom == 'top'){ # ... return top slice.n factors...
    sorted.factors.vals = as.numeric(svd.df$v[,component])[sort(as.numeric(svd.df$v[,component]),
                                                                decreasing = TRUE,
                                                                index = TRUE)$ix[1:slice.n]]
    sorted.factors.names = colnames(subset.data)[sort(svd.df$v[,component],
                                                      decreasing = TRUE,
                                                      index = TRUE)$ix[1:slice.n]]
    
  } else { # ... or return bottom slice.n factors
    sorted.factors.vals = as.numeric(svd.df$v[,component])[sort(as.numeric(svd.df$v[,component]),
                                                                decreasing = FALSE,
                                                                index = TRUE)$ix[1:slice.n]]
    sorted.factors.names = colnames(subset.data)[sort(svd.df$v[,component],
                                                      decreasing = FALSE,
                                                      index = TRUE)$ix[1:slice.n]]
  }
  
  # then spit out our list
  sorted.factors = data.frame(component.name,
                              sorted.factors.names,
                              sorted.factors.vals)
  names(sorted.factors) = c('component','name','weight')
  return(sorted.factors)
}


# 'summarySE': Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
# (Thanks to http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#####################################################################################

#### Group variables ####

# "ratings.variables": list of variables derived from headshot ratings
debater.ratings = c('attractive','intelligent','expert','biased','liberal','hostile','trustworthy')
debater.ratings.group = paste("group.", debater.ratings, sep = "")
debater.vars = c(debater.ratings,debater.ratings.group)

# "desc.variables": list of variables about descriptions about who's speaking, etc.
desc.variables = c('debate','speaker','group')

# "outcome.variables": list of variables about vote outcomes
outcome.variables = c('before_for','before_against','before_undecided',
                      'after_for','after_against','after_undecided',
                      'change_for','change_against','change_undecided',
                      'winner','deltaV')

# "detailed.variables": list of variables about more detailed vote metrics
detailed.variables = c('for_for','against_against','undecided_undecided',
                       'for_against','for_undecided',
                       'against_for','against_undecided',
                       'undecided_for','undecided_against')

# "summedLIWC.variables": list of LIWC-related variables that will not be included in the typical LIWC content
summedLIWC.variables = c('eos','WPS','WC','charnum')

# "text.variables": list of string variables to be excluded from numeric operations
text.variables = c('word','Filename','Seg','transcript')

# 'unused.variables': list of variables that are not included in standard LIWC
unused.variables = c('politics','liberal.1','conservative','demo','hedge','repub','turn')
unused.var.prop = paste("prop.", unused.variables, sep = "")
unused.var.group = paste("group.", unused.variables, sep = "")
unused.var.group.prop = paste("group.prop.",unused.variables, sep = "")
unused.variables = c(unused.variables,
                     unused.var.prop,
                     unused.var.group,
                     unused.var.group.prop) 