#### dimensionality_cutoff-pca_etc.r: Part of `pca-etc-markdown.Rmd` ####
#
# This script helps establish the number of components to include in our
# reduced-dimensionality model.
#
# Written by: A. Paxton (University of California, Berkeley)
#
#####################################################################################

# set maximum number of components
max.dim = 50

# create an empty dataframe to contain the information
dim.cutoff.table = data.frame(factor = 1:max.dim,
                              adj.r.squared = 0,
                              adj.r.gain = 0,
                              model = 0)

# cycle through all of the models
prev.model = 'reduced.data$deltaV ~ '
prev.adj.r.squared = 0
for (i in 1:max.dim){
  
  # create new model structure
  next.model = paste(prev.model,'model.data$u[,',i,']', sep = '')
  
  # run new model and grab the difference in adj.r.squared from previous model
  next.adj.r.squared = summary(lm(next.model))$adj.r.squared
  next.adj.r.gain = next.adj.r.squared - prev.adj.r.squared
  
  # store adj.r.squared in dataframe
  dim.cutoff.table$adj.r.squared[dim.cutoff.table$factor == i] = next.adj.r.squared
  dim.cutoff.table$adj.r.gain[dim.cutoff.table$factor == i] = next.adj.r.gain
  dim.cutoff.table[dim.cutoff.table$factor == i,]$model = next.model
  
  # save model and gain to variable for next go-around
  prev.model = paste(next.model,' + ', sep = '')
  prev.adj.r.squared = next.adj.r.squared
}
