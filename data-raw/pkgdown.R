library(pkgdown, lib.loc="C:\\users\\saiem\\documents\\r\\win-library\\4.1")
library(callr, lib.loc="C:\\users\\saiem\\documents\\r\\win-library\\4.1")
library(processx, lib.loc="C:\\users\\saiem\\documents\\r\\win-library\\4.1")
library(ps, lib.loc="C:\\users\\saiem\\documents\\r\\win-library\\4.1")


git2r::config(user.name = "saiemgilani", user.email = "saiem.gilani@gmail.com")
pkgdown::deploy_to_branch()
