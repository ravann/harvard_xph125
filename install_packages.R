system('defaults write org.R-project.R force.LANG en_US.UTF-8')

install.packages('ggplot2', dependencies=TRUE, repos='http://cran.rstudio.com/')

install.packages('tidyverse', dependencies=TRUE, repos='http://cran.rstudio.com/')

install.packages('dslabs', dependencies=TRUE, repos='http://cran.rstudio.com/')

install.packages('readr', dependencies=TRUE, repos='http://cran.rstudio.com/')

install.packages('gapminder', dependencies=TRUE, repos='http://cran.rstudio.com/')

install.packages('stringr', dependencies=TRUE, repos='http://cran.rstudio.com/')

install.packages('Lahman', dependencies=TRUE, repos='http://cran.rstudio.com/')

install.packages('HistData', dependencies=TRUE, repos='http://cran.rstudio.com/')
