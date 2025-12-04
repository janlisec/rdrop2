testthat::test_that(
  desc = "drop_share works correctly",
  code = {
    testthat::skip_on_cran()

    # upload file, get share infos and check
    tmp_file <- test_upload(file_name = traceless("test-drop_share.csv"))
    res <- rdrop2::drop_share(basename(tmp_file))
    testthat::expect_equal(length(res), 12)
    share_names <- sort(c(".tag", "created_timestamp", "url", "id", "name", "path_lower", "link_permissions", "preview_type","client_modified", "server_modified", "rev", "size"))
    res_names <- sort(names(res))
    testthat::expect_identical(share_names, res_names)

    # cleanup
    rdrop2::drop_delete(basename(tmp_file))
})

testthat::test_that(
  desc = "drop_search works correctly",
  code = {
    testthat::skip_on_cran()

    # upload file, search and check
    drop_path <- traceless("test-drop_search")
    tmp_file <- test_upload(file_name = "test-drop_search.csv", drop_path = drop_path)
    if (rdrop2::drop_exists(paste0(drop_path, "/", basename(tmp_file)))) {
      x <- try(rdrop2::drop_search(query = basename(tmp_file), path = NULL), silent = TRUE)
      if (inherits(x, "try-error")) {
        warnings("drop_search returned: ", attr(x, "condition")$message)
      } else {
        testthat::expect_true(is.list(x))
        testthat::expect_true(length(x[[1]])>=1)
        testthat::expect_equal(x$matches[[1]]$metadata$name, basename(tmp_file))
      }
    } else {
      warning("Could not find ", basename(tmp_file))
    }

    # A search with no query should fail
    testthat::expect_error(rdrop2::drop_search(query = NULL))

    # cleanup
    rdrop2::drop_delete(drop_path)
})


testthat::test_that(
  desc = "drop_history works correctly",
  code = {
    testthat::skip_on_cran()

    # upload file once
    tmp_file <- test_upload(file_name = "test-drop_history.csv")

    revisions <- rdrop2::drop_history(basename(tmp_file))

    testthat::expect_true(inherits(revisions, "tbl_df"))
    testthat::expect_equal(nrow(revisions), 1)

    # delete, upload a different file and check revision
    # TODO: add proper revision once drop_upload supports it
    rdrop2::drop_delete(basename(tmp_file))
    tmp_file <- test_upload(file_name = basename(tmp_file), file_content = iris)
    revisions <- rdrop2::drop_history(basename(tmp_file))
    testthat::expect_equal(nrow(revisions), 2)

    # test limit arguments
    revisions <- rdrop2::drop_history(basename(tmp_file), limit = 1)
    testthat::expect_equal(nrow(revisions), 1)

    # cleanup
    rdrop2::drop_delete(basename(tmp_file))
})


testthat::test_that(
  desc = "drop_media works correctly",
  code = {
    testthat::skip_on_cran()

    tmp_file <- test_upload(file_name = "test-drop_media.gif")
    utils::download.file("http://media4.giphy.com/media/YaXcVXGvBQlEI/200.gif", destfile = tmp_file, quiet = TRUE)
    testthat::expect_true(file.exists(tmp_file))
    testthat::expect_message(rdrop2::drop_upload(tmp_file), "uploaded")
    media_url <- rdrop2::drop_media(basename(tmp_file))
    testthat::expect_match(media_url$link, "dl.dropboxusercontent.com")

    # cleanup
    force_unlink(tmp_file)
    rdrop2::drop_delete(basename(tmp_file))
})

# minor test for strip slashes
testthat::test_that(
  desc = "strip slashes works correctly",
  code = {
    orig <- "//test/"
    testthat::expect_true(grepl("^//", orig))
    testthat::expect_false(grepl("^//", strip_slashes(orig)))
    testthat::expect_true(grepl("^/", strip_slashes(orig)))
    testthat::expect_true(grepl("/$", orig))
    testthat::expect_false(grepl("/$", strip_slashes(orig)))
})

# drop_read_csv
testthat::test_that(
  desc = "drop_read_csv works correctly",
  code = {
    testthat::skip_on_cran()

    # upload, download and check
    tmp_file <- test_upload(file_name = "test-drop_read.csv")
    # JL$$ ToDo: progress bar can not be avoided for rdrop2::drop_read_csv
    if (rdrop2::drop_exists(basename(tmp_file))) {
      #testthat::expect_message(x <- rdrop2::drop_read_csv(basename(tmp_file)), "Downloaded")
      x <- rdrop2::drop_read_csv(basename(tmp_file))
      testthat::expect_true(inherits(x, "data.frame"))
    } else {
      warning("Could not find ", basename(tmp_file))
    }

    # cleanup
    force_unlink(tmp_file)
    rdrop2::drop_delete(basename(tmp_file))
})

# Final cleanup of test files
testthat::test_that(
  desc = "final cleanup",
  code = {
    testthat::skip_on_cran()
    # all testthat tests should clean up after themselves, but in case of failures
    # this function will trace all rdrop2 related test files and remove them
    testthat::expect_match(clean_test_data(), "deleted")
})
