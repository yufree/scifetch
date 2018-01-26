#' Fetch rss into tibble
#' @param feed feed address
#' @param abslength lenghth of the description, default 500
#' @return tibble object
#' @seealso getpubmed
#' @examples \dontrun{
#' library(scifetch)
#' feed <- 'yihui.name/index.xml'
#' z <- getrss(feed)
#' }
#' @export
getrss <- function(feed, abslength = 500){
        formats <- c("a d b Y H:M:S z", "a, d b Y H:M z",
                     "Y-m-d H:M:S z", "d b Y H:M:S",
                     "d b Y H:M:S z", "a b d H:M:S z Y",
                     "a b dH:M:S Y")
        json_parse <- function(feed){

                res <- jsonlite::fromJSON(feed)

                items <- res$items

                results <- tibble::tibble(
                        title = items$title,
                        date = lubridate::parse_date_time(items$date_published, orders = formats) %>%
                                as.character(),
                        linkTitle = items$url,
                        source = res$title,
                        description = items$content_html
                )

                return(results)
        }

        rss_parse <- function(doc){

                channel <- xml2::xml_find_all(doc, "channel")

                if(identical(length(channel), 0L)){
                        ns <- xml2::xml_ns_rename(xml2::xml_ns(doc), d1 = "rss")
                        channel <- xml2::xml_find_all(doc, "rss:channel", ns = ns)
                        site <- xml2::xml_find_all(doc, "rss:item", ns = ns)

                        res <- suppressWarnings({tibble::tibble(
                                title = xml2::xml_text(xml2::xml_find_all(site, "rss:title", ns = ns)),
                                date = xml2::xml_text(xml2::xml_find_first(site, "rss:pubDate", ns = ns)) %>%
                                        lubridate::parse_date_time(orders = formats) %>%
                                        as.character(),
                                linkTitle = xml2::xml_text(xml2::xml_find_all(site, "rss:link", ns = ns)),
                                source = xml2::xml_text(xml2::xml_find_first(channel, "rss:title", ns = ns)),
                                description = xml2::xml_text(xml2::xml_find_first(site, "rss:description", ns = ns))
                        )})
                } else{

                        site <- xml2::xml_find_all(channel, "item")

                        res <- suppressWarnings({tibble::tibble(
                                title = xml2::xml_text(xml2::xml_find_first(site, "title")),
                                date = xml2::xml_text(xml2::xml_find_first(site, "pubDate")) %>%
                                        lubridate::parse_date_time(orders = formats) %>%
                                        as.character(),
                                linkTitle = xml2::xml_text(xml2::xml_find_first(site, "link")),
                                source = xml2::xml_text(xml2::xml_find_first(channel, "title")),
                                description = xml2::xml_text(xml2::xml_find_first(site, "description"))
                        )})

                        res <- Filter(function(x) !all(is.na(x)), res)

                        return(res)
                }
        }
        atom_parse <- function(doc){
                channel <- xml2::xml_find_all(doc, "channel", ns = xml2::xml_ns(doc))
                site <- xml2::xml_find_all(channel, "item")


                res <- tibble::tibble(
                        title = xml2::xml_text(xml2::xml_find_first(site, ns = xml2::xml_ns(doc), "title")),
                        date = xml2::xml_text(xml2::xml_find_first(site, ns = xml2::xml_ns(doc),
                                                                                "pubDate")) %>%
                                lubridate::parse_date_time(orders = formats) %>%
                                as.character(),
                        linkTitle = xml2::xml_text(xml2::xml_find_first(site, ns = xml2::xml_ns(doc),
                                                                        "link")),
                        source = xml2::xml_text(xml2::xml_find_first(channel, ns = xml2::xml_ns(doc), "title")),
                        description = xml2::xml_text(xml2::xml_find_all(site, ns = xml2::xml_ns(doc), "description")) %>%
                        substr(start=1, stop=abslength))
                return(res)
        }

        invisible({
                suppressWarnings({
                        stopifnot(identical(length(feed), 1L)) # exit if more than 1 feed provided

                        msg <- "Error in feed parse; please check URL."

                        doc <- try(httr::GET(feed), silent = TRUE)

                        if(grepl("json", doc)){
                                result <- json_parse(feed)
                                return(result)
                        } else{
                                doc <- doc %>% xml2::read_xml()
                        }

                        if(unique(grepl('try-error', class(doc)))){
                                stop(msg)
                        }


                        if(grepl("http://www.w3.org/2005/Atom", doc)){

                                result <- atom_parse(doc)

                                return(result)

                        } else{
                                result <- rss_parse(doc)

                                return(result)
                        }
                })
        })
}
