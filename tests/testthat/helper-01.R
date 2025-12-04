#' a wrapper function for unlink, ensuring that connections are closed
force_unlink <- function(x, ...) {
  # x: Vektor of path
  # ...: arguments to unlink()
  conns <- showConnections(all = TRUE)
  for (file in x) {
    idx <- which(normalizePath(file, mustWork = FALSE) == conns[, "description"])
    if (length(idx) > 0) {
      for (i in idx) {
        close(getConnection(as.integer(rownames(conns)[i])))
      }
    }
  }

  unlink(x, ...)
}

# helper functions for testthat tests uploading a file (via tempdir() and ensuring an appropriate unlink)
test_upload <- function(file_name = "test.csv", file_content = mtcars, drop_path = NULL) {
  if (!grepl("^rdrop2_package_test_", file_name)) file_name <- traceless(file_name)
  tmp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  write.csv(file_content, tmp_file)
  if (file.exists(tmp_file)) {
    testthat::expect_message(rdrop2::drop_upload(tmp_file, path = drop_path), "uploaded")
    force_unlink(tmp_file)
  } else {
    message("File was not created")
  }
  return(tmp_file)
}


#' Function makes file/folder names unique to prevent race conditions during concurrent tests. Pun on race condition
traceless <- function(file) {
  paste0("rdrop2_package_test_", uuid::UUIDgenerate(), "_", file)
}


# This should clean out any remaining/old test files and folders
clean_test_data <- function(pattern = "rdrop2_package_test", dtoken = get_dropbox_token()) {
  files <- drop_dir()
  if (nrow(files) > 0) {
    filenames <- files[["name"]]
    matching_files <- grep(pattern, filenames, value = TRUE)
    if (length(matching_files)) {
      suppressWarnings(purrr::walk(matching_files, drop_delete))
    }
    return(paste(length(matching_files), "files deleted from dropbox."))
  }
}
