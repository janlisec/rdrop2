testthat::test_that(
  desc = "Test that basic file upload works correctly",
  code = {
    testthat::skip_on_cran()

    # This is a simple test to see if we can upload a csv file successfully
    tmp_file <- normalizePath(file.path(tempdir(), traceless("file-ops.csv")), mustWork = FALSE)
    write.csv(mtcars, tmp_file)
    row_count <- nrow(mtcars)

    # Check to see if file uploads are successful
    testthat::expect_message(drop_upload(file = tmp_file), "successfully at")
    force_unlink(tmp_file, force = TRUE)
    rdrop2::drop_download(path = basename(tmp_file), local_path = tmp_file, progress = FALSE)
    y <- utils::read.csv(tmp_file)
    server_row_count <- nrow(y)

    # Make sure the downloaded csv has the same number of rows
    testthat::expect_equal(row_count, server_row_count)

    # clean up locally and in dropbox
    force_unlink(tmp_file)
    rdrop2::drop_delete(basename(tmp_file))
})

testthat::test_that(
  desc = "Image upload works correctly",
  code = {
    testthat::skip_on_cran()

    # This test is to see if we can upload an image (a png in this case) and make
    # sure that it maintains file integrity. We compare hashes of local file, then
    # the roundtrip copy.
    test_file <- testthat::test_path('rdrop2_package_test_image.png')
    testthat::expect_true(file.exists(test_file))

    # copy the file to a temp folder and hash
    tmp_file <- normalizePath(file.path(tempdir(), traceless("image.png")), mustWork = FALSE)
    testthat::expect_true(file.copy(from = test_file, to = tmp_file, copy.mode = FALSE))
    local_file_hash <- digest::digest(tmp_file)

    # Upload file to Dropbox, delete the local copy, download and hash.
    testthat::expect_message(rdrop2::drop_upload(tmp_file), "uploaded")
    force_unlink(tmp_file, force = TRUE)
    rdrop2::drop_download(path = basename(tmp_file), local_path = tmp_file, progress = FALSE)
    roundtrip_file_hash <- digest::digest(tmp_file)

    # compare hashes and cleanup
    testthat::expect_equal(local_file_hash, roundtrip_file_hash)
    force_unlink(tmp_file)
    rdrop2::drop_delete(basename(tmp_file))
})

testthat::test_that(
  desc = "Upload of a non-existent file fails",
  code = {
    testthat::skip_on_cran()

    testthat::expect_error(drop_upload("higgledy-piggledy.csv"))
    testthat::expect_error(drop_upload("higgledy-piggledy.csv", mode = "stuff"))
})

testthat::test_that(
  desc = "Autorename upon upload works correctly",
  code = {
    testthat::skip_on_cran()

    # create an empty folder in dropbox
    drop_path <- traceless("test-autorename")
    testthat::expect_message(rdrop2::drop_create(drop_path), "created")
    blank <- rdrop2::drop_dir(drop_path)
    testthat::expect_true(inherits(blank, "tbl"))
    testthat::expect_equal(nrow(blank), 0)

    # upload a test-file and confirm
    tmp_file <- test_upload(file_name = "test-drop_autorename.csv", drop_path = drop_path)
    one_file <- rdrop2::drop_dir(drop_path)
    testthat::expect_equal(nrow(one_file), 1)
    testthat::expect_identical(one_file[1,]$path_lower, paste0("/", drop_path, "/", basename(tmp_file)))

    # Write a slightly different object to the same file name
    write.csv(iris[1:6, ], file = tmp_file)
    testthat::expect_message(rdrop2::drop_upload(tmp_file, path = drop_path, mode = "add"), "uploaded")
    two_files <- rdrop2::drop_dir(drop_path)
    testthat::expect_equal(nrow(two_files), 2)
    # This is the auto-rename that should happen
    expected_files <- c(
      paste0("/", drop_path, "/", basename(tmp_file)),
      paste0("/", drop_path, "/", gsub(".csv$", " (1).csv", basename(tmp_file)))
    )
    testthat::expect_identical(two_files$path_lower, expected_files)

    # clean up locally and in dropbox
    rdrop2::drop_delete(drop_path)
    force_unlink(tmp_file)
})
