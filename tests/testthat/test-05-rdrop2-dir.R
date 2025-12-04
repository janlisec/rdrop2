testthat::test_that(
  desc = "drop_dir lists files normally",
  code = {
    testthat::skip_on_cran()

    # create folders and objects
    folder_name <- traceless("test-drop_dir")
    testthat::expect_message(rdrop2::drop_create(folder_name), "created")
    tmp_file <- test_upload(file_name = traceless("test-drop_dir.csv"), drop_path = folder_name)

    # list contents and check
    results <- rdrop2::drop_dir(folder_name)
    testthat::expect_true(inherits(results, "tbl"))
    testthat::expect_true(inherits(results, "tbl_df"))
    testthat::expect_equal(nrow(results), 1)

    # add more things
    subfolder_name <- paste0(folder_name, "/", traceless("test-drop_subdir"))
    rdrop2::drop_create(subfolder_name)
    tmp_file <- test_upload(file_name = traceless("test-drop_subfile.csv"), file_content = iris, drop_path = subfolder_name)

    results <- rdrop2::drop_dir(folder_name)
    testthat::expect_equal(nrow(results), 2)

    results <- rdrop2::drop_dir(folder_name, recursive = TRUE)
    testthat::expect_equal(nrow(results), 4) # also returns entry for folder_name

    # cleanup
    rdrop2::drop_delete(folder_name)
})

testthat::test_that(
  desc = "drop_dir can detect changes in directory",
  code = {
    testthat::skip_on_cran()

    # create a folder
    folder_name <- traceless("drop_dir")
    testthat::expect_message(rdrop2::drop_create(folder_name), "created")

    # put a file in it
    tmp_file <- test_upload(file_name = traceless("test-drop_dir.csv"), drop_path = folder_name)

    # get a cursor
    cursor <- rdrop2::drop_dir(folder_name, cursor = TRUE)
    testthat::expect_true(is.character(cursor))

    # add another file
    tmp_file <- test_upload(file_name = traceless("test-drop_dir.csv"), drop_path = folder_name)

    # check for contents since cursor
    entries <- rdrop2::drop_dir(cursor = cursor)
    testthat::expect_true(inherits(entries, "tbl_df"))
    testthat::expect_equal(nrow(entries), 1)
    testthat::expect_equal(entries$name[1], basename(tmp_file))

    # check whole folder
    entries <- rdrop2::drop_dir(folder_name)
    testthat::expect_true(inherits(entries, "tbl_df"))
    testthat::expect_equal(nrow(entries), 2)

    # cleanup
    rdrop2::drop_delete(folder_name)
})
