# Package installation script for all packages used in this package

#install.packages("devtools")
#install.packages("testthat")
#install.packages("roxygen2")
#install.packages("rriskDistributions")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("R6")
#install.packages("fitdistrplus")
#install.packages("config")
#install.packages("triangle")
#install.packages("rsconnect")
#install.packages("gld")
#remove.packages("R6")
#devtools::install_github('wch/R6', build_vignettes = FALSE)
#remove.packages("R6", lib="~/R/win-library/3.3")
#remove.packages("R6", lib="C:/Program Files/Microsoft/R Client/R_SERVER/library")


#unloadNamespace("R6")
#Error in unloadNamespace("R6") : 
#  l'espace de noms 'R6' est importé par 'mrsdeploy', 'CompatibilityAPI' et ne peut, donc, pas être déchargé
#> unloadNamespace("mrsdeploy")
#> unloadNamespace("CompatibilityAPI")
#Error in unloadNamespace("CompatibilityAPI") : 
#l'espace de noms 'CompatibilityAPI' est importé par 'RevoScaleR' et ne peut, donc, pas être déchargé
#> unloadNamespace("RevoScaleR")
#Error: le package ''RevoScaleR' est nécessaire pour 'MicrosoftML' et n'est donc pas détachable
#> unloadNamespace("MicrosoftML")
#> unloadNamespace("RevoScaleR")
#> unloadNamespace("CompatibilityAPI")
#> unloadNamespace("R6")