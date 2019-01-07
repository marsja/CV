#######################################################################################################
#' Erik Marsja CV - erik at marsja dot se, https://www.marsja.se
#' Script to install the dependencies for the CV
#######################################################################################################


list.of.packages <- c("stargazer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)