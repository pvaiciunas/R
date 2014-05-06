#Installing SIT package

setInternet2(TRUE)
download.file('http://www.systematicportfolio.com/SIT.tar.gz', 'sit')
install.packages('sit', repos = NULL, type='source')