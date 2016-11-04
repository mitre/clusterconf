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
})