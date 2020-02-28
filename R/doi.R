#' Fetch Bibliographies via DOI
#' @param path file path
#' @param style citaiton style, default apa
#' @return NULL
#' @export
doiref <- function(path, style = 'apa'){
        mystring <- readLines(path)
        doi <- unlist(stringr::str_extract_all(mystring,"\\[\\@\\b10\\.(\\d+\\.*)+[\\/](([^\\s\\.])+\\.*)+\\b\\]"))
        doi <- unique(doi)
        n <- as.numeric(factor(doi,levels = doi))
        for (i in 1:length(doi)){
                x <- readLines(path)
                x <- gsub(doi[i], paste0('[',n[i],']'),x,fixed = T)
                writeLines(x,path)
                temp <- try(rcrossref::cr_cn(dois = unlist(strsplit(doi[i],split = '\\[@|\\]'))[2], format = "text", style = style), T)
                cat(temp,file=path,sep="\n",append=TRUE)
        }
}
