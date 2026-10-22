#!/usr/bin/env Rscript

# Generate a GitHub-friendly Markdown CV from the main R Markdown CV.
#
# Default usage from the repository root:
#   Rscript generate_github_cv.R
#
# Optional input and output paths:
#   Rscript generate_github_cv.R marsja_erik2.Rmd CV.md

args <- commandArgs(trailingOnly = TRUE)

input_rmd <- if (length(args) >= 1) args[[1]] else "dr_marsja_cv.Rmd"
output_md <- if (length(args) >= 2) args[[2]] else "README.md"

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop(
    "Package 'rmarkdown' is required. Install it with ",
    "install.packages('rmarkdown').",
    call. = FALSE
  )
}

if (!file.exists(input_rmd)) {
  stop("R Markdown file not found: ", input_rmd, call. = FALSE)
}

project_dir <- normalizePath(
  dirname(input_rmd),
  winslash = "/",
  mustWork = TRUE
)

input_rmd <- normalizePath(
  input_rmd,
  winslash = "/",
  mustWork = TRUE
)

if (!grepl("^([A-Za-z]:)?[/\\\\]", output_md)) {
  output_md <- file.path(project_dir, output_md)
}

output_md <- normalizePath(
  output_md,
  winslash = "/",
  mustWork = FALSE
)

dir.create(
  dirname(output_md),
  recursive = TRUE,
  showWarnings = FALSE
)


remove_yaml <- function(lines) {
  if (length(lines) == 0 || trimws(lines[[1]]) != "---") {
    return(lines)
  }

  yaml_end <- which(trimws(lines[-1]) == "---")

  if (length(yaml_end) == 0) {
    stop("Could not find the end of the YAML header.", call. = FALSE)
  }

  yaml_end <- yaml_end[[1]] + 1L

  if (yaml_end >= length(lines)) {
    return(character())
  }

  lines[(yaml_end + 1L):length(lines)]
}


remove_r_chunks_matching <- function(lines, patterns) {
  output <- character()
  i <- 1L

  while (i <= length(lines)) {
    if (grepl("^```\\{r", trimws(lines[[i]]))) {
      closing <- which(
        trimws(lines[(i + 1L):length(lines)]) == "```"
      )

      if (length(closing) == 0) {
        stop(
          "An R code chunk is missing its closing backticks.",
          call. = FALSE
        )
      }

      j <- i + closing[[1]]
      block <- lines[i:j]
      block_text <- paste(block, collapse = "\n")
      remove_block <- any(vapply(
        patterns,
        function(pattern) grepl(pattern, block_text, perl = TRUE),
        logical(1)
      ))

      if (!remove_block) {
        output <- c(output, block)
      }

      i <- j + 1L
    } else {
      output <- c(output, lines[[i]])
      i <- i + 1L
    }
  }

  output
}


remove_raw_tex_fences <- function(lines) {
  output <- character()
  inside_raw_tex <- FALSE

  for (line in lines) {
    trimmed <- trimws(line)

    if (!inside_raw_tex && trimmed == "```{=tex}") {
      inside_raw_tex <- TRUE
      next
    }

    if (inside_raw_tex && trimmed == "```") {
      inside_raw_tex <- FALSE
      next
    }

    output <- c(output, line)
  }

  output
}


clean_latex_for_github <- function(lines) {
  text <- paste(lines, collapse = "\n")

  # Join date ranges that are split over two lines in the PDF-oriented CV.
  text <- gsub(
    paste0(
      "(?m)^([A-Z][a-z]+ [0-9]{4}) -[[:space:]]*",
      "\\n[[:space:]]*\\n",
      "([A-Z][a-z]+ [0-9]{4})[[:space:]]*"
    ),
    "\\1–\\2 — ",
    text,
    perl = TRUE
  )

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  lines <- vapply(
    lines,
    function(line) {
      line <- gsub(
        "\\\\href\\{([^{}]+)\\}\\{([^{}]+)\\}",
        "[\\2](\\1)",
        line,
        perl = TRUE
      )
      line <- gsub(
        "\\\\textbf\\{([^{}]+)\\}",
        "**\\1**",
        line,
        perl = TRUE
      )
      line <- gsub("\\\\hspace\\{[^{}]+\\}", " ", line, perl = TRUE)
      line <- gsub("\\\\vspace\\{[^{}]+\\}", "", line, perl = TRUE)
      line <- gsub("\\\\sloppy[[:space:]]*", "", line, perl = TRUE)
      line <- gsub("\\\\LaTeX", "LaTeX", line)
      line <- gsub("\\\\%", "%", line)
      line <- gsub("\\\\&", "&", line)
      line <- gsub(
        paste0(
          " See Table 1 for an overview of my teaching responsibilities, ",
          "clock hours, and courses\\."
        ),
        "",
        line
      )
      line <- gsub("[[:space:]]+$", "", line)

      line <- gsub(
        "^([A-Z][a-z]+ [0-9]{4}) -[[:space:]]+(\\*\\*)",
        "\\1–present — \\2",
        line,
        perl = TRUE
      )
      line <- gsub(
        "^([A-Z][a-z]+ [0-9]{4})[[:space:]]+(\\*\\*)",
        "\\1 — \\2",
        line,
        perl = TRUE
      )
      line <- gsub(
        "^([0-9]{4})[[:space:]]+(\\*\\*)",
        "\\1 — \\2",
        line,
        perl = TRUE
      )

      trimws(line, which = "right")
    },
    character(1),
    USE.NAMES = FALSE
  )

  remove_line <- grepl(
    paste0(
      "^[[:space:]]*(\\\\hrule|",
      "\\\\begin\\{changemargin\\}|",
      "\\\\end\\{changemargin\\}|",
      "\\\\newpage|",
      "\\\\pagestyle\\{empty\\})"
    ),
    lines,
    perl = TRUE
  )

  lines <- lines[!remove_line]

  # GitHub does not need more than one blank paragraph between blocks.
  output <- character()
  blank_count <- 0L

  for (line in lines) {
    if (!nzchar(trimws(line))) {
      blank_count <- blank_count + 1L

      if (blank_count <= 2L) {
        output <- c(output, "")
      }
    } else {
      blank_count <- 0L
      output <- c(output, line)
    }
  }

  output
}


source_lines <- readLines(
  input_rmd,
  warn = FALSE,
  encoding = "UTF-8"
)

body <- remove_yaml(source_lines)

# Keep the teaching description and supervision lists, but remove the hidden
# teaching-data preparation and the large table at the end of the PDF CV.
body <- remove_r_chunks_matching(
  body,
  patterns = c(
    "teachinghours2\\.csv",
    "kableExtra::kbl",
    "flextable\\(newdata\\)"
  )
)

body <- remove_raw_tex_fences(body)
body <- clean_latex_for_github(body)

github_header <- c(
  "---",
  "title: \"Erik Marsja — Academic CV\"",
  "output:",
  "  github_document:",
  "    toc: true",
  "    toc_depth: 2",
  "    html_preview: false",
  "---",
  "",
  "## Erik Marsja",
  "",
  "**Assistant Professor and Docent in Disability Research**  ",
  "Linköping University, Sweden",
  "",
  paste0(
    "[Website](https://marsja.se) · ",
    "[Email](mailto:erik@marsja.se) · ",
    "[GitHub](https://github.com/marsja)"
  ),
  "",
  paste0(
    "This Markdown CV is generated from `",
    basename(input_rmd),
    "` and the accompanying data files."
  ),
  ""
)

temporary_rmd <- tempfile(
  pattern = "github_cv_",
  tmpdir = project_dir,
  fileext = ".Rmd"
)

on.exit(unlink(temporary_rmd), add = TRUE)

writeLines(
  c(github_header, body),
  temporary_rmd,
  useBytes = TRUE
)

rmarkdown::render(
  input = temporary_rmd,
  output_format = rmarkdown::github_document(
    toc = TRUE,
    toc_depth = 2,
    html_preview = FALSE
  ),
  output_file = basename(output_md),
  output_dir = dirname(output_md),
  knit_root_dir = project_dir,
  envir = new.env(parent = globalenv()),
  clean = TRUE,
  quiet = FALSE
)

message("GitHub Markdown CV created: ", output_md)
