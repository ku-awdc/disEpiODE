PKG_NAME := "disEpiODE"
default:
    echo 'Hello, world!'

install:
    Rcmd.exe INSTALL --preclean --no-multiarch --with-keep.source .

document:
    Rscript -e "devtools::document()"
check: 
    Rscript -e "devtools::check()"
test:
    Rscript -e "devtools::test()"
