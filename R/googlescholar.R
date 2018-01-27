#' Fetch author's publication from Google Scholar
#' @param id the google scholar ID to be searched
#' @param start begin item
#' @param end finish item
#' @return tibble object
#' @examples \dontrun{
#' library(scifetch)
#' id <- "SPNX8oUAAAAJ"
#' z <- getgsauthor(id)
#' }
#' @export
getgsauthor <- function(id, start = 1, end = 100) {
        url_template <- "http://scholar.google.com/citations?hl=en&user=%s&cstart=%d&pagesize=%d"
        url <- sprintf(url_template, id, start-1, end)

        cites <- url %>%
                xml2::read_html() %>%
                rvest::html_nodes(xpath="//tr[@class='gsc_a_tr']")

        title <- cites %>%
                rvest::html_nodes(".gsc_a_at") %>%
                rvest::html_text()

        pubid <- cites %>%
                rvest::html_nodes(".gsc_a_at") %>%
                rvest::html_attr("href") %>%
                stringr::str_extract(":.*$") %>%
                stringr::str_sub(start=2)

        doc_id <- cites %>%
                rvest::html_nodes(".gsc_a_ac") %>%
                rvest::html_attr("href") %>%
                stringr::str_extract("cites=.*$") %>%
                stringr::str_sub(start=7)

        cited_by <- suppressWarnings(
                cites %>%
                        rvest::html_nodes(".gsc_a_ac") %>%
                        rvest::html_text() %>%
                        as.numeric(.) %>%
                        replace(is.na(.), 0))

        year <- cites %>%
                rvest::html_nodes(".gsc_a_y") %>%
                rvest::html_text() %>%
                as.numeric()

        authors <- cites %>%
                rvest::html_nodes("td .gs_gray") %>%
                rvest::html_text() %>%
                subset(c(TRUE,FALSE))

        details <- cites %>%
                rvest::html_nodes("td .gs_gray") %>%
                rvest::html_text() %>%
                subset(c(FALSE,TRUE))

        first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", details)) - 1
        journal <- stringr::str_trim(stringr::str_sub(details, end=first_digit)) %>%
                stringr::str_replace(",$", "")

        numbers <- stringr::str_sub(details, start=first_digit) %>%
                stringr::str_trim() %>%
                stringr::str_sub(end=-5) %>%
                stringr::str_trim() %>%
                stringr::str_replace(",$", "")

        data <- tibble::as_tibble(cbind(
                title = title,
                author=authors,
                journal=journal,
                number=numbers,
                cites=cited_by,
                year=year,
                cid=doc_id,
                pubid=pubid))
        return(data)
}

#' #' Fetch journal's publication from Google Scholar
#' #' @param id the journal ID to be searched
#' #' @param start begin item
#' #' @param end finish item
#' #' @param years start year
#' #' @param yeare end year
#' #' @return tibble object
#' #' @examples \dontrun{
#' #' library(scifetch)
#' #' id <- "environmental science and technology"
#' #' z <- getgsjournal(id)
#' #' }
#' #' @export
#' getgsjournal <- function(name, start = 1, end = 100, years = 2016, yeare = 2017) {
#'         url_template <- "https://scholar.google.ca/scholar?hl=en&scisbd=1&as_publication=%s&cstart=%d&pagesize=%d&as_ylo=%d&as_yhi=%d"
#'         url <- sprintf(url_template, name,start, end, years, yeare)
#'
#'         cites <- url %>%
#'                 read_html() %>%
#'                 html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "gs_ri", " " ))]')
#'
#'         title <- cites %>%
#'                 html_nodes(".gsc_a_at") %>%
#'                 html_text()
#'
#'         pubid <- cites %>%
#'                 html_nodes(".gsc_a_at") %>%
#'                 html_attr("href") %>%
#'                 str_extract(":.*$") %>%
#'                 str_sub(start=2)
#'
#'         doc_id <- cites %>%
#'                 html_nodes(".gsc_a_ac") %>%
#'                 html_attr("href") %>%
#'                 str_extract("cites=.*$") %>%
#'                 str_sub(start=7)
#'
#'         cited_by <- suppressWarnings(
#'                 cites %>%
#'                         html_nodes(".gsc_a_ac") %>%
#'                         html_text() %>%
#'                         as.numeric(.) %>%
#'                         replace(is.na(.), 0))
#'
#'         year <- cites %>%
#'                 html_nodes(".gsc_a_y") %>%
#'                 html_text() %>%
#'                 as.numeric()
#'
#'         authors <- cites %>%
#'                 html_nodes("td .gs_gray") %>%
#'                 html_text() %>%
#'                 subset(c(TRUE,FALSE))
#'
#'         details <- cites %>%
#'                 html_nodes("td .gs_gray") %>%
#'                 html_text() %>%
#'                 subset(c(FALSE,TRUE))
#'
#'         first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", details)) - 1
#'         journal <- str_trim(str_sub(details, end=first_digit)) %>%
#'                 str_replace(",$", "")
#'
#'         numbers <- str_sub(details, start=first_digit) %>%
#'                 str_trim() %>%
#'                 str_sub(end=-5) %>%
#'                 str_trim() %>%
#'                 str_replace(",$", "")
#'
#'         data <- tibble::as_tibble(cbind(
#'                 title = title,
#'                 author=authors,
#'                 journal=journal,
#'                 number=numbers,
#'                 cites=cited_by,
#'                 year=year,
#'                 cid=doc_id,
#'                 pubid=pubid))
#'         return(data)
#' }
#'
