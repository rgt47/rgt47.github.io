zz.cp = 
function (title, collection = "posts", author = "auto", slug = "auto", 
    date = Sys.Date(), date_prefix = NULL, draft = TRUE, edit = FALSE) 
{
    site_dir <- distill:::find_site_dir(".")
    if (is.null(site_dir)) 
        stop("You must call create_post from within a Distill website")
    site_config <- site_config(site_dir)
    posts_dir <- file.path(site_dir, paste0("_", collection))
    posts_index <- file.path(site_dir, site_config$output_dir, 
        collection, paste0(collection, ".json"))
    slug <- distill:::resolve_slug(title, slug)
    post_dir <- distill:::resolve_post_dir(posts_dir, slug, date_prefix)
    if (identical(author, "auto")) {
        author <- NULL
        if (file.exists(posts_index)) 
            posts <- distill:::read_json(posts_index)
        else posts <- list()
        if (length(posts) > 0) 
            author <- list(author = posts[[1]]$author)
    }
    if (is.null(author)) {
        author <- list(author = list(list(name = fullname(fallback = "Unknown"))))
    }
    else if (is.character(author)) {
        author <- list(author = author)
    }
    author <- yaml::as.yaml(author, indent.mapping.sequence = TRUE)
    if (draft) 
        draft <- "\ndraft: true"
    else draft <- ""
    yaml <- sprintf("---\ntitle: \"%s\"\ndescription: |\n  A short description of the post.\n%sdate: %s\noutput:\n  distill::distill_article:\n    self_contained: false%s\n---", 
        title, author, format.Date(date, "%F"), draft)
    body <- "\n\n
```{r init, include=FALSE}

options(dplyr.print_max = 1e9)
library(pacman)

p_load(rmarkdown, tidyverse,  broom,  tidyr,  purrr,readr,
       dplyr,  lubridate, zoo,  ggplot2, printr, lsmeans, ggthemes,  knitr,
       nlme, pwr)

opts_chunk$set(warning = F, message = F, echo=F, fig.width=3.2,
	       fig.height=3,  results='asis', dev='pdf')
source('~/shr/zz.tools.R')
options(scipen = 1, digits = 2)
``` 

# Introduction

"
    if (distill:::dir_exists(post_dir)) 
        stop("Post directory '", post_dir, "' already exists.", 
            call. = FALSE)
    dir.create(post_dir, recursive = TRUE)
    post_file <- file.path(post_dir, distill:::file_with_ext(slug, "Rmd"))
    con <- file(post_file, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    xfun::write_utf8(yaml, con)
    xfun::write_utf8(body, con)
    bullet <- "v"
    circle <- "o"
    new_collection <- !(collection %in% names(distill:::site_collections(site_dir, 
        site_config)))
    if (new_collection) {
        cat(paste0(bullet, " Created new collection at _", collection), 
            "\n")
    }
    cat(paste(bullet, "Created post at", paste0("_", collection, 
        "/", basename(post_dir))), "\n")
    if (new_collection) {
        cat(paste0(circle, " ", "TODO: Register '", collection, 
            "' collection in _site.yml\n"))
        cat(paste0(circle, " ", "TODO: Create listing page for '", 
            collection, "' collection\n\n"))
        cat("See docs at https://rstudio.github.io/distill/blog.html#creating-a-collection")
    }
    invisible(post_file)
}
