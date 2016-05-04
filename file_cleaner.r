library(tools)
library(tm)

clean_file <- function(file_name) {
    extension = file_ext(file_name)
    #file name without extensions
    base_name <- substr(file_name,1,
        nchar(file_name) - nchar(extension)-1) 
   
    tmp_name <- paste(base_name,"_tmp.",extension,sep="")
    con <- file(file_name, open="r")
    con2 <- file(tmp_name,"w+")
    
    remove_special_chars <- function(text) {
        special_chars_re <- "[½§!\"@#£¤$%&/{([)=?+\\]`´^¨~*'-_–.:;,|><]"  
        return(gsub(special_chars_re,"",text,perl=TRUE))
    }
    
    clean_functions <- c(
        removeNumbers, stripWhitespace, tolower,
        remove_special_chars
    )
    
    #TODO: also remove words whose length is < min_len ?
    
    apply_cleaning_functions <- function(line) {
        ret <-line
        for (f in clean_functions) {
            ret <- f(ret)    
        }
        return(ret)
    }
    
    #http://stackoverflow.com/questions/4106764/what-is-a-good-way-to-read-line-by-line-in-r
    
    while(length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        new_line <-paste(apply_cleaning_functions(line),"\n",sep="")
        cat(new_line,file=con2,append=TRUE)
    }
    
    close(con)
    close(con2)
    
    file.rename(tmp_name,file_name)
}