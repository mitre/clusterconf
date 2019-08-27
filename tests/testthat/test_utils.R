test_that("Configs can be loaded" , {
  with_mock(
    is_cluster_known=function(x, stopifnot){return(TRUE)},
    get_cluster_package_name=function(x){return("clusterconf")},
    configs <- get_cluster_configs("abc")
  )
  expect_equal(names(configs), c("scope1", "scope2"))
})

test_that("parameter extraction works", {
  with_mock(
    is_cluster_known=function(x, stopifnot){return(TRUE)},
    get_cluster_package_name=function(x){return("clusterconf")},
    configs <- get_cluster_configs("abc")
  )
  
  # without scoping
  param <- get_cluster_param(configs, "param1")
  expect_equal(class(param), "character")
  expect_equal(length(param), 2)
  
  # scope to scope1
  param <- get_cluster_param(configs, "param1", "scope1")
  expect_equal(class(param), "integer")
  expect_equal(length(param), 1)
  
  # scope to scope2
  param <- get_cluster_param(configs, "param1", "scope2")
  expect_equal(class(param), "character")
  expect_equal(length(param), 1)
  
  # invalid scope with NULL default
  param <- get_cluster_param(configs, "param1", "scope3")
  expect_null(param)

  # invalid scope with provided default
  param <- get_cluster_param(configs, "param1", "scope3", default = FALSE)
  expect_false(param)
})

test_that("is_valid_scope works as expected", {
  with_mock(
    is_cluster_known=function(x, stopifnot){return(TRUE)},
    get_cluster_package_name=function(x){return("clusterconf")},
    configs <- get_cluster_configs("abc")
  )
  
  # warning is issued by default
  expect_warning(clusterconf:::is_valid_scope(configs, "scope3"), 
                 regexp = paste0("Cannot return configurations for scope: 'scope3'. ",
                                 "Valid scopes are: 'scope1', 'scope2'. Returning NULL."))
  
  # opt out of warning
  expect_false(clusterconf:::is_valid_scope(configs, "scope3", warn = FALSE))
  
  # normal behavior
  expect_true(clusterconf:::is_valid_scope(configs, "scope1"))
})


test_that("get_java_dependencies_path works with provided cluster name", {
  tmp_dir <- file.path("fake", "package", "dir")
  
  with_mock(
    find.package=function(package){return(tmp_dir)},
    expect_equal(get_java_dependencies_path("abc"), 
                 file.path(tmp_dir, "inst", "java"))
  )
})

test_that("get_java_dependencies_path works with provided package name", {
  tmp_dir <- file.path("fake", "package", "dir")
  
  with_mock(
    find.package=function(package){return(tmp_dir)},
    expect_equal(get_java_dependencies_path(pkg_name = "clusterconf.abc"), 
                 file.path(tmp_dir, "inst", "java"))
  )
})

test_that("get_java_classpath works with provided cluster name", {
  with_mock(
    get_java_dependencies_path=function(cluster_name, pkg_name) {return(file.path("fake", "path"))},
    dir=function(path, include.dirs=FALSE) {return(c("fake1.jar", "fake2.jar"))},
    
    expect_equal(get_java_classpath("fake1.jar", "abc"),
                 file.path("fake", "path", "fake1.jar"))
  )
})

test_that("get_java_classpath throws error with insufficient input", {
  expect_error(get_java_classpath("fake1.jar"),
               "Unable to locate dependencies")
})

