#'
getpubmed <- function(query, start = 1, end = 100){
        query <- as.character(query)
        query <- gsub(" ", "+", query, fixed = TRUE)
        PID <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?",
                     "db=pubmed&term=",
                     query,
                     "&usehistory=y",
                     sep = "")
        # use ID to get full records
        # tmpConnect <- url(PID, open = "rb")
        xml <- xml2::read_xml(PID)
        # close.connection(tmpConnect)
        list <- xml2::as_list(xml)
        n <- list$Count[[1]]
        warning <- paste(n,"records founds")
        message(warning)
        efetch_url = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?",
                           "db=pubmed&WebEnv=",
                           list$WebEnv[[1]],
                           "&query_key=",
                           list$QueryKey[[1]],
                           "&retstart=1",
                           start,
                           "&retmax=",
                           end,
                           "&retmode=xml",
                           sep = "")
        xml2 <- xml2::read_xml(efetch_url)
        return(xml2)
}

getabs <- function(list){
        if (length(list) > 2){
                abstract <- paste(list[1:length(list)-1], collapse = " ", sep = " ")
        } else if (length(list) < 1) {
                abstract <- NA
        } else {
                abstract <- list[1]
        }
        return(abstract)
}

getpubmeddf <- function(xml2){
        record <- xml2 %>%
                xml2::xml_find_all(".//PubmedArticle//MedlineCitation//Article")
        journal <- record$ %>%
                xml2::xml_find_all("Journal//ISOAbbreviation") %>%
                xml2::xml_contents() %>%
                as.character()
        title <- record %>%
                xml2::xml_find_all("ArticleTitle") %>%
                xml2::xml_contents() %>%
                as.character()
        abstract <- record %>%
                xml2::as_list() %>%
                purrr::map(`[[`,'Abstract') %>%
                purrr::map(getabs) %>%
                purrr::map_chr(unlist) %>%
                stringr::str_replace('list \\(\"',"") %>%
                stringr::str_replace('\"\\)',"")


}
query <- 'janusz pawliszyn[AU]'
z <- getpubmed('janusz pawliszyn[AU]')


