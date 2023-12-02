setwd("C:/Users/forhad/Desktop/packg/Np") #set the working directory to the path of the packages
pkgs <- list.files()

install.packages(c(print(as.character(pkgs),
                         collapse = "\",\"")),
                 type = 'source', repos = NULL)
