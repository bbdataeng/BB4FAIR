# remotes::install_version("rsconnect", "0.8.29")

library(rsconnect)

rsconnect::setAccountInfo(name='bbdataeng',
                          token='1A6630C592D9A63FD1C8D65C0F08F390',
                          secret='SLd4aCbiTMTzLbzh1QVn2/egA2b0H5JpLay6/iSG')

rsconnect::deployApp('bb4FAIR_app')



