#' @title download voting records
#' @description downloadsmember_vorting_records from toronto open data
#' @param dest_dir path, Default: 'inst/extdata/to_open_data'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname download_voting_records
#' @export
download_voting_records <- function(dest_dir = "inst/extdata/to_open_data") {
  # Create destination directory if it doesn't exist
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  # List of URLs and corresponding filenames
  urls <- c(
    "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7f5232d6-0d2a-4f95-864a-417cbf341cc4/resource/c4feb78c-c867-42a9-b803-7c6d859df969/download/member-voting-record-2022-2026.csv",
    "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7f5232d6-0d2a-4f95-864a-417cbf341cc4/resource/373390e9-af88-4b58-a6a4-6863e3606a4b/download/member-voting-record-2018-2022.csv",
    "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7f5232d6-0d2a-4f95-864a-417cbf341cc4/resource/11d89d61-24c3-4241-8194-04b22098745e/download/member-voting-record-2014-2018.csv",
    "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7f5232d6-0d2a-4f95-864a-417cbf341cc4/resource/6c7dd98b-08d0-4f68-bee7-a0ba77a7da92/download/member-voting-record-2010-2014.csv",
    "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/7f5232d6-0d2a-4f95-864a-417cbf341cc4/resource/01655cbd-dc66-4339-9f27-891e64413cbf/download/member-voting-record-2006-2010.csv"
  )

  filenames <- basename(urls)
  file_paths <- file.path(dest_dir, filenames)

  # Download each file
  for (i in seq_along(urls)) {
    dest_file <- file.path(dest_dir, filenames[i])
    utils::download.file(urls[i], destfile = dest_file, mode = "wb")
    message("Downloaded: ", filenames[i])
  }

  df_list <- lapply(file_paths, utils::read.csv, stringsAsFactors = FALSE)
  member_voting_record <- do.call(rbind, df_list)

  # Save to data/ directory
  save(member_voting_record, file = "data/member_voting_record.rda")
  save(member_voting_record, file = "data/member_voting_record.rda", compress = "xz")
}

# download_voting_records()
