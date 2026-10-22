# Update publication metadata with Crossref
#
# Expected columns in the `publications` sheet:
# authors, title, journal, status, year, volume, issue, pages,
# doi, pdf_url, include_cv
#
# Missing DOI values are searched by title. Once a DOI is available, missing
# year, volume, issue and pages values are fetched from Crossref. Existing
# values are never overwritten. Authors, title, journal and status are never
# sent back to or changed from Crossref metadata.


required_packages <- c(
  "rcrossref",
  "readxl",
  "openxlsx",
  "dplyr",
  "tibble"
)


metadata_columns <- c(
  "year",
  "volume",
  "issue",
  "pages"
)


check_required_packages <- function() {
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing_packages) > 0) {
    stop(
      "Install the following packages before running the script: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
}


is_blank <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}


normalise_doi <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x, ignore.case = TRUE)
  x <- sub("^doi:\\s*", "", x, ignore.case = TRUE)
  tolower(x)
}


normalise_title <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}


title_similarity <- function(target, candidate) {
  target <- normalise_title(target)
  candidate <- normalise_title(candidate)
  
  if (
    is.na(target) || is.na(candidate) ||
    !nzchar(target) || !nzchar(candidate)
  ) {
    return(NA_real_)
  }
  
  denominator <- max(nchar(target), nchar(candidate))
  
  if (denominator == 0) {
    return(NA_real_)
  }
  
  1 - as.numeric(utils::adist(target, candidate)) / denominator
}


first_existing_column <- function(data, candidates, default = NA_character_) {
  column <- candidates[candidates %in% names(data)][1]
  
  if (length(column) == 0 || is.na(column)) {
    return(rep(default, nrow(data)))
  }
  
  as.character(data[[column]])
}


first_nonblank_value <- function(data, candidates) {
  for (column in candidates[candidates %in% names(data)]) {
    values <- as.character(data[[column]])
    
    if (length(values) > 0 && !is_blank(values[[1]])) {
      return(values[[1]])
    }
  }
  
  NA_character_
}


extract_year <- function(data) {
  date_value <- first_nonblank_value(
    data,
    c(
      "published.print",
      "published-print",
      "published.online",
      "published-online",
      "issued"
    )
  )
  
  if (is_blank(date_value)) {
    return(NA_character_)
  }
  
  year_match <- regmatches(
    date_value,
    regexpr("[0-9]{4}", date_value)
  )
  
  if (length(year_match) == 0 || !nzchar(year_match)) {
    return(NA_character_)
  }
  
  year_match
}


search_crossref_title <- function(title, limit = 5) {
  result <- rcrossref::cr_works(
    flq = c(query.bibliographic = title),
    limit = limit,
    sort = "score",
    order = "desc"
  )
  
  data <- result$data
  
  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }
  
  candidate_title <- first_existing_column(data, c("title", "Title"))
  candidate_doi <- first_existing_column(data, c("doi", "DOI"))
  candidate_journal <- first_existing_column(
    data,
    c("container.title", "container-title", "journal")
  )
  candidate_score <- suppressWarnings(as.numeric(
    first_existing_column(data, c("score"), default = NA_character_)
  ))
  
  tibble::tibble(
    candidate_rank = seq_len(nrow(data)),
    candidate_title = candidate_title,
    candidate_doi = normalise_doi(candidate_doi),
    candidate_journal = candidate_journal,
    crossref_score = candidate_score,
    exact_title_match = normalise_title(candidate_title) == normalise_title(title),
    title_similarity = vapply(
      candidate_title,
      function(x) title_similarity(title, x),
      numeric(1)
    )
  ) |>
    dplyr::filter(!is_blank(candidate_doi))
}


fetch_crossref_metadata <- function(doi) {
  result <- rcrossref::cr_works(
    dois = normalise_doi(doi)
  )
  
  data <- result$data
  
  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }
  
  tibble::tibble(
    year = extract_year(data),
    volume = first_nonblank_value(data, c("volume")),
    issue = first_nonblank_value(data, c("issue")),
    pages = first_nonblank_value(
      data,
      c("page", "pages", "article.number", "article-number")
    )
  )
}


write_crossref_updates <- function(
    xlsx_path,
    sheet,
    publications,
    updates,
    create_backup = TRUE
) {
  if (nrow(updates) == 0) {
    return(invisible(NULL))
  }
  
  if (create_backup) {
    timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
    backup_path <- file.path(
      dirname(xlsx_path),
      paste0(
        tools::file_path_sans_ext(basename(xlsx_path)),
        "_backup_",
        timestamp,
        ".xlsx"
      )
    )
    
    copied <- file.copy(xlsx_path, backup_path, overwrite = FALSE)
    
    if (!copied) {
      stop("Could not create backup: ", backup_path, call. = FALSE)
    }
    
    message("Backup created: ", backup_path)
  }
  
  workbook <- openxlsx::loadWorkbook(xlsx_path)
  
  for (i in seq_len(nrow(updates))) {
    row <- updates$data_row[[i]]
    column_name <- updates$field[[i]]
    column <- match(column_name, names(publications))
    
    openxlsx::writeData(
      workbook,
      sheet = sheet,
      x = publications[[column_name]][[row]],
      startCol = column,
      startRow = row + 1,
      colNames = FALSE,
      rowNames = FALSE
    )
  }
  
  tryCatch(
    openxlsx::saveWorkbook(workbook, xlsx_path, overwrite = TRUE),
    error = function(e) {
      stop(
        "Could not save ", xlsx_path,
        ". Check that the file is not open in Excel. Original error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}


update_publication_metadata <- function(
    xlsx_path = file.path("Data", "marsja_publications.xlsx"),
    sheet = "publications",
    candidates_path = file.path(dirname(xlsx_path), "crossref_candidates.csv"),
    write_changes = TRUE,
    create_backup = TRUE,
    limit = 5
) {
  check_required_packages()
  
  if (!file.exists(xlsx_path)) {
    stop("File not found: ", xlsx_path, call. = FALSE)
  }
  
  if (!nzchar(Sys.getenv("crossref_email"))) {
    message(
      "Tip: add crossref_email=your.email@example.com to ~/.Renviron ",
      "for more reliable Crossref requests."
    )
  }
  
  publications <- readxl::read_excel(
    xlsx_path,
    sheet = sheet,
    na = c("", "NA")
  )
  
  required_columns <- c("title", "doi", metadata_columns)
  missing_columns <- setdiff(required_columns, names(publications))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns in sheet '", sheet, "': ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  columns_as_text <- c("doi", metadata_columns)
  
  for (column in columns_as_text) {
    publications[[column]] <- as.character(publications[[column]])
  }
  
  has_missing_metadata <- Reduce(
    `|`,
    lapply(metadata_columns, function(column) {
      is_blank(publications[[column]])
    })
  )
  
  rows_to_process <- which(
    (is_blank(publications$doi) & !is_blank(publications$title)) |
      (!is_blank(publications$doi) & has_missing_metadata)
  )
  
  if (length(rows_to_process) == 0) {
    message("No missing DOI or publication metadata was found.")
    
    return(invisible(list(
      publications = publications,
      updates = tibble::tibble(),
      candidates = tibble::tibble(),
      errors = tibble::tibble()
    )))
  }
  
  candidate_results <- list()
  error_results <- list()
  updates <- list()
  
  add_update <- function(row, field, new_value, source) {
    old_value <- publications[[field]][[row]]
    
    if (!is_blank(old_value) || is_blank(new_value)) {
      return(FALSE)
    }
    
    publications[[field]][[row]] <<- as.character(new_value)
    
    updates[[length(updates) + 1]] <<- tibble::tibble(
      data_row = row,
      excel_row = row + 1,
      title = as.character(publications$title[[row]]),
      field = field,
      old_value = as.character(old_value),
      new_value = as.character(new_value),
      source = source
    )
    
    TRUE
  }
  
  for (i in seq_along(rows_to_process)) {
    row <- rows_to_process[[i]]
    title <- publications$title[[row]]
    
    message(
      "Processing ", i, "/", length(rows_to_process), ": ", title
    )
    
    if (is_blank(publications$doi[[row]])) {
      candidates <- tryCatch(
        search_crossref_title(title, limit = limit),
        error = function(e) {
          error_results[[length(error_results) + 1]] <<- tibble::tibble(
            excel_row = row + 1,
            title = title,
            doi = NA_character_,
            stage = "DOI search",
            error = conditionMessage(e)
          )
          
          tibble::tibble()
        }
      )
      
      if (nrow(candidates) == 0) {
        candidate_results[[length(candidate_results) + 1]] <- tibble::tibble(
          excel_row = row + 1,
          target_title = title,
          candidate_rank = NA_integer_,
          candidate_title = NA_character_,
          candidate_doi = NA_character_,
          candidate_journal = NA_character_,
          crossref_score = NA_real_,
          exact_title_match = FALSE,
          title_similarity = NA_real_,
          decision = "no candidate found"
        )
        
        next
      }
      
      exact_matches <- which(candidates$exact_title_match)
      
      if (length(exact_matches) == 1) {
        matched <- candidates[exact_matches, , drop = FALSE]
        matched_doi <- matched$candidate_doi[[1]]
        
        add_update(
          row = row,
          field = "doi",
          new_value = matched_doi,
          source = "Crossref exact title match"
        )
        
        decision <- "DOI accepted: one exact title match"
      } else if (length(exact_matches) > 1) {
        decision <- "manual review: several exact title matches"
      } else {
        decision <- "manual review: no exact title match"
      }
      
      candidate_results[[length(candidate_results) + 1]] <- candidates |>
        dplyr::mutate(
          excel_row = row + 1,
          target_title = title,
          decision = decision,
          .before = 1
        )
      
      if (is_blank(publications$doi[[row]])) {
        next
      }
    }
    
    missing_metadata <- metadata_columns[
      vapply(
        metadata_columns,
        function(column) is_blank(publications[[column]][[row]]),
        logical(1)
      )
    ]
    
    if (length(missing_metadata) == 0) {
      next
    }
    
    doi <- normalise_doi(publications$doi[[row]])
    
    metadata <- tryCatch(
      fetch_crossref_metadata(doi),
      error = function(e) {
        error_results[[length(error_results) + 1]] <<- tibble::tibble(
          excel_row = row + 1,
          title = title,
          doi = doi,
          stage = "metadata lookup",
          error = conditionMessage(e)
        )
        
        tibble::tibble()
      }
    )
    
    if (nrow(metadata) == 0) {
      next
    }
    
    for (field in missing_metadata) {
      add_update(
        row = row,
        field = field,
        new_value = metadata[[field]][[1]],
        source = paste0("Crossref DOI ", doi)
      )
    }
  }
  
  candidates <- dplyr::bind_rows(candidate_results)
  errors <- dplyr::bind_rows(error_results)
  updates <- dplyr::bind_rows(updates)
  
  if (nrow(candidates) > 0) {
    utils::write.csv(
      candidates,
      candidates_path,
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    
    message("Crossref candidates saved to: ", candidates_path)
  }
  
  if (write_changes && nrow(updates) > 0) {
    write_crossref_updates(
      xlsx_path = xlsx_path,
      sheet = sheet,
      publications = publications,
      updates = updates,
      create_backup = create_backup
    )
    
    doi_count <- sum(updates$field == "doi")
    metadata_count <- sum(updates$field %in% metadata_columns)
    
    message(
      doi_count, " DOI value(s) and ",
      metadata_count, " metadata value(s) written to: ",
      xlsx_path
    )
  } else if (!write_changes && nrow(updates) > 0) {
    message(
      nrow(updates),
      " secure update(s) found; write_changes = FALSE, so the workbook was not changed."
    )
  } else {
    message("No DOI or publication metadata was updated automatically.")
  }
  
  if (nrow(errors) > 0) {
    warning(
      nrow(errors),
      " Crossref request(s) failed. The remaining publications were still processed."
    )
  }
  
  invisible(list(
    publications = publications,
    updates = updates |>
      dplyr::select(-dplyr::any_of("data_row")),
    candidates = candidates,
    errors = errors
  ))
}


# Backwards-compatible name used by the existing Rmd file.
update_publication_dois <- update_publication_metadata


# Run the update when the file is executed directly with Rscript.
# When sourced from an Rmd file, call update_publication_dois() explicitly.
if (sys.nframe() == 0) {
  arguments <- commandArgs(trailingOnly = TRUE)
  
  xlsx_path <- if (length(arguments) >= 1) {
    arguments[[1]]
  } else {
    file.path("Data", "marsja_publications.xlsx")
  }
  
  update_publication_metadata(xlsx_path = xlsx_path)
}
