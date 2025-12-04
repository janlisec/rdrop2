testthat::test_that(
  desc = "drop_download works as expected",
  code = {
  skip_on_cran()

  # create and upload a file for testing
  tmp_file <- test_upload(file_name = traceless("test-drop_download.csv"))

  # download to same path
  testthat::expect_true(drop_download(path = basename(tmp_file), local_path = tmp_file))
  testthat::expect_true(file.exists(tmp_file))

  # cleanup
  force_unlink(tmp_file)
  rdrop2::drop_delete(basename(tmp_file))
})

test_that("drop_get works, but is deprecated", {
  skip_on_cran()

  # create and upload a file for testing, then delete locally
  tmp_file <- test_upload(file_name = traceless("test-drop_download.csv"))

  # check for deprecation warning
  testthat::expect_message(testthat::expect_warning(rdrop2::drop_get(path = basename(tmp_file), local_file = tmp_file)))

  # make sure the file was downloaded
  testthat::expect_true(file.exists(tmp_file))

  # cleanup
  force_unlink(tmp_file)
  rdrop2::drop_delete(basename(tmp_file))
})
