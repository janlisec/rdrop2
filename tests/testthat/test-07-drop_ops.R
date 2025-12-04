testthat::test_that(
  desc = "File operations create/move/copy/delete work correctly",
  code = {
    testthat::skip_on_cran()

    # create file in dropbox
    drop_path <- traceless("test-drop_dir1")
    tmp_file <- test_upload(file_name = "test-drop_copy.csv", drop_path = drop_path)
    tmp_file <- paste0(drop_path, "/", basename(tmp_file))

    testthat::expect_true(rdrop2:::drop_is_folder(drop_path))

    # check that error is given when folder exists
    testthat::expect_true(rdrop2::drop_exists(drop_path))
    testthat::expect_error(rdrop2::drop_create(drop_path))

    # Copy this file to another file with new name
    tmp_file2 <- paste0(drop_path, "/", traceless("test-drop_copy.csv"))
    testthat::expect_message(rdrop2::drop_copy(tmp_file, tmp_file2), "copied")
    testthat::expect_identical(sort(c(basename(tmp_file), basename(tmp_file2))), sort(rdrop2::drop_dir(path = drop_path)$name))

    # Copy to same name, but using autorename=TRUE
    testthat::expect_message(rdrop2::drop_copy(tmp_file, tmp_file, autorename = TRUE), "copied")
    testthat::expect_equal(nrow(rdrop2::drop_dir(drop_path)), 3)

    # Copying files to folders
    drop_path2 <- traceless("test-drop_dir2")
    testthat::expect_message(rdrop2::drop_create(drop_path2), "created")
    # ToDo: $$JL this is making a copy in the destination folder not only of the source file but of the source file including the path (unexpected!)
    testthat::expect_message(rdrop2::drop_copy(tmp_file, drop_path2), "copied")

    # Copying folders to existing folders
    drop_path3 <- traceless("test-drop_dir3")
    # ToDo: $$JL this is making a copy of the content of source into target, I would expect it to copy the folder
    testthat::expect_message(rdrop2::drop_copy(drop_path, drop_path3), "copied")
    testthat::expect_identical(
      sort(rdrop2::drop_dir(drop_path)$name),
      sort(rdrop2::drop_dir(drop_path3)$name)
    )

    # Copying files to new folders (created during copy)
    drop_path4 <- traceless("test-drop_dir4")
    testthat::expect_message(rdrop2::drop_copy(drop_path, drop_path4), "copied")
    testthat::expect_true(rdrop2::drop_exists(drop_path4))

    # moving a file
    drop_path5 <- traceless("test-drop_dir5")
    testthat::expect_message(rdrop2::drop_create(drop_path5), "created")
    testthat::expect_message(rdrop2::drop_move(tmp_file, drop_path5), "moved")
    # tmp_file should be gone from original place and appear in new place
    testthat::expect_false(rdrop2::drop_exists(tmp_file))
    # ToDo: $$JL this is moving the file including its path into target, I would expect it to move only the file
    testthat::expect_true(rdrop2::drop_exists(paste0(drop_path5, "/", tmp_file)))

    # expect error when deleting a non_existent path
    testthat::expect_error(rdrop2::drop_delete(traceless("test-drop_fake_file")))

    # expect FALSE for testing for a non-existent file
    testthat::expect_false(rdrop2::drop_exists(traceless("test-drop_fake_file")))

    # cleanup
    rdrop2::drop_delete(drop_path)
    rdrop2::drop_delete(drop_path2)
    rdrop2::drop_delete(drop_path3)
    rdrop2::drop_delete(drop_path4)
    rdrop2::drop_delete(drop_path5)
})
