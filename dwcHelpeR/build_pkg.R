# add required package to DESCRIPTION
required_packages = c("dplyr", "data.table", "uuid", "digest", "httr", "xml2", "stringdist")
lapply(required_packages, devtools::use_package)
