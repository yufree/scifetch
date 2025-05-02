#' Fetch rss into tibble
#' @param feed feed address
#' @return tibble object
#' @seealso getpubmed
#' @examples \dontrun{
#' library(scifetch)
#' feed <- 'yihui.name/index.xml'
#' z <- getrss(feed)
#' }
getrss <- function(feed) {
  # This function is modified from tidyRSS and credits should belong to the author of tidyRSS package
  formats <- c(
    "a d b Y H:M:S z", "a, d b Y H:M z",
    "Y-m-d H:M:S z", "d b Y H:M:S",
    "d b Y H:M:S z", "a b d H:M:S z Y",
    "a b dH:M:S Y", "Y-m-d"
  )
  json_parse <- function(feed) {
    res <- jsonlite::fromJSON(feed)
    items <- res$items
    results <- tibble::tibble(
      title = items$title,
      date = lubridate::parse_date_time(items$date_published, orders = formats) %>%
        as.character() %>% substr(1, 10),
      linkTitle = items$url,
      source = res$title,
      description = items$content_html
    )
    return(results)
  }

  rss_parse <- function(doc) {
    channel <- xml2::xml_find_all(doc, "channel")
    if (identical(length(channel), 0L)) {
      ns <- xml2::xml_ns_rename(xml2::xml_ns(doc), d1 = "rss")
      channel <- xml2::xml_find_all(doc, "rss:channel", ns = ns)
      site <- xml2::xml_find_all(doc, "rss:item", ns = ns)
      res <- suppressWarnings({
        tibble::tibble(
          title = xml2::xml_text(xml2::xml_find_all(site, "rss:title", ns = ns)),
          date = xml2::xml_text(xml2::xml_find_first(site, "rss:pubDate", ns = ns)) %>%
            lubridate::parse_date_time(orders = formats) %>%
            as.character() %>% substr(1, 10),
          linkTitle = xml2::xml_text(xml2::xml_find_all(site, "rss:link", ns = ns)),
          source = xml2::xml_text(xml2::xml_find_first(channel, "rss:title", ns = ns)),
          description = xml2::xml_text(xml2::xml_find_first(site, "rss:description", ns = ns))
        )
      })
    } else {
      site <- xml2::xml_find_all(channel, "item")
      res <- suppressWarnings({
        tibble::tibble(
          title = xml2::xml_text(xml2::xml_find_first(site, "title")),
          date = xml2::xml_text(xml2::xml_find_first(site, "pubDate")) %>%
            lubridate::parse_date_time(orders = formats) %>%
            as.character() %>% substr(1, 10),
          linkTitle = xml2::xml_text(xml2::xml_find_first(site, "link")),
          source = xml2::xml_text(xml2::xml_find_first(channel, "title")),
          description = xml2::xml_text(xml2::xml_find_first(site, "description"))
        )
      })
      res <- Filter(function(x) !all(is.na(x)), res)
    }
    return(res)
  }

  atom_parse <- function(doc) {
    ns <- c(atom = "http://www.w3.org/2005/Atom")
    feed_title <- xml2::xml_text(xml2::xml_find_first(doc, ".//atom:feed/atom:title", ns = ns))
    entries <- xml2::xml_find_all(doc, ".//atom:entry", ns = ns)
    
    parse_entry <- function(entry) {
      title <- xml2::xml_text(xml2::xml_find_first(entry, ".//atom:title", ns = ns))
      
      pub <- xml2::xml_find_first(entry, ".//atom:published", ns = ns)
      upd <- xml2::xml_find_first(entry, ".//atom:updated", ns = ns)
      date <- if (length(pub) > 0) xml2::xml_text(pub) else xml2::xml_text(upd)
      date_parsed <- lubridate::parse_date_time(date, orders = formats) %>%
        as.character() %>% substr(1, 10)
      
      link <- xml2::xml_find_first(entry, ".//atom:link", ns = ns)
      link_href <- if (length(link) > 0) xml2::xml_attr(link, "href") else NA_character_
      
      content <- xml2::xml_find_first(entry, ".//atom:content", ns = ns)
      summary <- xml2::xml_find_first(entry, ".//atom:summary", ns = ns)
      description <- if (length(content) > 0) {
        xml2::xml_text(content)
      } else if (length(summary) > 0) {
        xml2::xml_text(summary)
      } else {
        NA_character_
      }
      
      list(
        title = title,
        date = date_parsed,
        linkTitle = link_href,
        description = description
      )
    }
    
    entries_list <- lapply(entries, parse_entry)
    res <- tibble::tibble(
      title = sapply(entries_list, `[[`, "title"),
      date = sapply(entries_list, `[[`, "date"),
      linkTitle = sapply(entries_list, `[[`, "linkTitle"),
      source = feed_title,
      description = sapply(entries_list, `[[`, "description")
    )
    return(res)
  }

  invisible({
    suppressWarnings({
      stopifnot(identical(length(feed), 1L))
      
      msg <- "Error in feed parse; please check URL."
      httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
      response <- try(httr::GET(feed), silent = TRUE)
      if (inherits(response, "try-error")) stop(msg)
      
      if (grepl("application/json", response$headers$`content-type`)) {
        result <- json_parse(feed)
      } else {
        doc <- xml2::read_xml(response)
        root <- xml2::xml_root(doc)
        root_name <- xml2::xml_name(root)
        root_xmlns <- xml2::xml_attr(root, "xmlns")
        
        if (root_name == "feed" && root_xmlns == "http://www.w3.org/2005/Atom") {
          result <- atom_parse(doc)
        } else {
          result <- rss_parse(doc)
        }
      }
      
      if (NROW(result) > 0) result else NULL
    })
  })
}
