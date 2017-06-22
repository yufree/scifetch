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

getpubmeddf <- function(xml2){
        options(warn = -1)
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
        record <- xml2 %>%
                xml2::xml_find_all(".//PubmedArticle//MedlineCitation//Article")
        journal <- record %>%
                xml2::xml_find_all("Journal//ISOAbbreviation") %>%
                xml2::xml_contents() %>%
                as.character() %>%
                unlist()
        title <- record %>%
                xml2::xml_find_all("ArticleTitle") %>%
                xml2::xml_contents() %>%
                as.character() %>%
                unlist()
        year <- record %>%
                xml2::xml_find_all("Journal//JournalIssue//PubDate") %>%
                xml2::as_list() %>%
                purrr::map(unlist) %>%
                purrr::map(paste0) %>%
                purrr::map(`[`,1) %>%
                unlist()
        month <- record %>%
                xml2::xml_find_all("Journal//JournalIssue//PubDate") %>%
                xml2::as_list() %>%
                purrr::map(unlist) %>%
                purrr::map(paste0) %>%
                purrr::map(`[`,2) %>%
                unlist()
        day <- record %>%
                xml2::xml_find_all("Journal//JournalIssue//PubDate") %>%
                xml2::as_list() %>%
                purrr::map(unlist) %>%
                purrr::map(paste0) %>%
                purrr::map(`[`,3) %>%
                unlist()
        abstract <- record %>%
                xml2::as_list() %>%
                purrr::map(`[[`,'Abstract') %>%
                purrr::map(getabs) %>%
                purrr::map_chr(unlist) %>%
                stringr::str_replace_all('list\\(\"',"") %>%
                stringr::str_replace_all('\"\\)',"") %>%
                unlist()

        paperdf <- as.tibble(cbind(journal,title,year,month,day,abstract)) %>%
                mutate(day = case_when(is.na(day) ~ '01', !is.na(day) ~ day)) %>%
                mutate(month = case_when(!(month %in% month.abb) & is.na(month) ~ 'Jan', !(month %in% month.abb) & !is.na(month) ~ month.abb[as.numeric(month)], month %in% month.abb ~ month)) %>%
                unite(date, year, month, day, sep = '') %>%
                mutate(date = as.Date(date,'%Y%b%d')) %>%
                bind_cols(line = 1:length(title))
        options(warn = 0)
        return(paperdf)
}
query <- 'janusz pawliszyn[AU]'
z <- getpubmed('janusz pawliszyn[AU]')
z2 <- getpubmeddf(z)

