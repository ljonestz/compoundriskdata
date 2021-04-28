if(!require(bookdown)){
    install.packages("bookdown")
    library(bookdown)
}
library(bookdown)
bookdown::render_book('index.Rmd', 'all')