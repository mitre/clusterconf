#' Get cluster configurations
#' 
#' Find and read the cluster configuration file for a named cluster.
#' 
#' @param cluster_name Character. The name of the cluster. Capitalization and spacing
#'   is ignored and does not matter (e.g., "My Cluster" is equivalent to "mycluster")
#' @param scope Character. The scope of the configurations of interest. The configuration
#'   YAML may be organized by various headings. Optional. If not provided, the full 
#'   configuration will be returned without any filtering.
#' @param yaml_path Character. The path to the yaml configuration file for the specified
#'   cluster.  Optional.  If not provided, code looks for \code{/inst/config/*yaml} in 
#'   the installed package \code{cluster_name}.
#' @importFrom config get
#' @export
get_cluster_configs <- function(cluster_name, scope, yaml_path) {
  if (missing(yaml_path)) {
    is_cluster_known(cluster_name, stopifnot=TRUE)
    configs <- config::get(file=get_yaml_configuraions_path(cluster_name))
  } else
    configs <- config::get(file=yaml_path)
  
  if (!missing(scope)) {
    if (is_valid_scope(configs, scope)) 
      configs <- configs[[scope]]
     else 
      configs <- NULL
  }
  return(configs)
}

#' Utility function to graph parameter
#' 
#' This function centralizes digging through the configurations list into one place.
#' Given a parameter name and (optionally) a scope, it will find the parameter and
#' return it. If no such parameter can be found then a \code{NULL} value is returned.
#' The idea here is that \code{\link{get_cluster_configs}} has already been used
#' to read the configuration file.
#' 
#' In the event that multiple configurations match the requested parameter (e.g.,
#' there exist parameters in different scopes with the same name) then all matches
#' will be returned provided they are at the same level of nestedness. 
#' 
#' @param configs List. Cluster configurations, usually obtained using a call to
#'   \code{\link{get_cluster_configs}}.
#' @param parameter Character. The name of the parameter of interest. 
#' @param scope Charcter. The scope of the configurations of interest. The configuration
#'   YAML may be organized by various headings.
#' @param default Any Type. This is the retrun value in the event that no parameter
#'   is found. This is useful, for example, if looking for a value you expect to 
#'   be \code{TRUE} or \code{FALSE} and you want to default to one of those if the
#'   parameter is not found.
#' @export
get_cluster_param <- function(configs, parameter, scope, default=NULL) {
  
  # filter by scope
  if (!missing(scope) && !is.null(scope) && !is.na(scope)) {
    if (is_valid_scope(configs, scope)) 
      configs <- configs[[scope]]
    else 
      return(default)
  }
  
  # search for param
  param <- search_for_param(configs, tolower(parameter))
  if (length(param)==0)
    return(default)
  param[param=="NA"] <- NA
  return(param)
}
search_for_param <- function(configs, parameter) {
  names <- names(configs)
  id <- names==parameter
  if (any(id))
    return(configs[[which(id)]])
  
  # didn't find the param, check nested lists
  id <- vapply(configs, is.list, NA)
  if (!any(id))
    return(NULL)
  nested_configs <- configs[id]
  config <- lapply(nested_configs, search_for_param, parameter=parameter)
  config <- unlist(config)
  return(config[!is.null(config)])
}

is_valid_scope <- function(configs, scope, warn=TRUE) {
  if (!(scope %in% names(configs))) {
    if (warn)
      warning(paste0("Cannot return configurations for scope: '", scope, "'. ",
                     "Valid scopes are: '", paste0(names(configs), collapse="', '"),
                     "'. Returning NULL."))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @rdname get_cluster_configs
#' @export
get_yaml_configuraions_path <- function(cluster_name) {
  pkg_path <- find.package(get_cluster_package_name(cluster_name))
  if (!dir.exists(file.path(pkg_path, "configs")))
    pkg_path <- file.path(pkg_path, "inst")
  
  path <- file.path(pkg_path, "configs")
  fname <- dir(path, pattern = "yaml$", include.dirs = FALSE)[1]
  return(file.path(path, fname))
}

#' Get java classpath
#' 
#' Some cluster configuration packages contain java dependencies. This utility will
#' use the jar names from the configuration YAML and find the full file paths to the
#' stated dependencies.
#' 
#' @param jars Character. A vector of jar files to include on the classpath. These should
#'   be listed using the full file name, including the ".jar" extension at the end.
#' @param cluster_name Character. The name of the cluster. Capitalization and spacing
#'   is ignored and does not matter (e.g., "My Cluster" is equivalent to "mycluster")
#' @param base_path Character. A file path to the directory where the dependencies may
#'   be found. This is used in place of the cluster resources if provided.
#' @param configs List. The parsed YAML configurations. This is used if provided and
#'   neither \code{cluster_name} nor \code{base_path} is provided
#' @export
get_java_classpath <- function(jars, cluster_name, base_path, configs) {
  
  # figure out where to look
  if (missing(base_path) && !missing(cluster_name)) {
    base_path <- get_java_dependencies_path(cluster_name)
  } else if (missing(base_path) && missing(cluster_name) && !missing(configs)) {
    base_path <- get_cluster_param(configs, parameter="directory", scope="resources", default = NA)
    if (is.na(base_path))
      base_path <- get_java_dependencies_path(pkg_name=get_cluster_param(
        configs, parameter="package", scope="resources", default = NA))
  } else if (missing(base_path) && missing(cluster_name) && !missing(configs)) {
    stop("Unable to locate dependencies. One of 'cluster_path', 'base_path', or 'configs' must be input")
  }
  
  files <- dir(base_path, include.dirs=FALSE)
  id <- jars %in% files
  if (any(!id))
    warning(paste0("Unable to locate dependencies: '", paste0(jars[!id], collapse="', '"), "'"))
  return(file.path(base_path, jars[id]))
}

#' @rdname get_java_classpath
#' @export
get_java_dependencies_path <- function(cluster_name, pkg_name) {
  if (missing(pkg_name))
    pkg_name <- get_cluster_package_name(cluster_name)
  
  pkg_path <- find.package(pkg_name)
  if (!dir.exists(file.path(pkg_path, "java")))
    pkg_path <- file.path(pkg_path, "inst")
  
  return(file.path(pkg_path, "java"))
}
