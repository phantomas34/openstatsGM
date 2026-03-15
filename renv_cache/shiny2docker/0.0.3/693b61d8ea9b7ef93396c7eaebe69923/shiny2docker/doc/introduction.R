## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----generate-dockerfile, eval=FALSE------------------------------------------
#  library(shiny2docker)
#  
#  # Generate a Dockerfile in the current directory
#  shiny2docker(path = ".")

## ----generate-dockerfile-custom, eval=FALSE-----------------------------------
#  library(shiny2docker)
#  
#  # Generate a Dockerfile with specific paths
#  shiny2docker(
#    path = "path/to/your/app",
#    lockfile = "path/to/your/app/renv.lock",
#    output = "path/to/your/app/Dockerfile"
#  )

## ----customize-dockerfile, eval=FALSE-----------------------------------------
#  # Create the dockerfiler object
#  docker_obj <- shiny2docker()
#  
#  # Add environment variables
#  docker_obj$add_after(  cmd = "ENV ARROW_WITH_S3=1",  after = 1)
#  docker_obj$add_after(  cmd = "ENV ARROW_S3=ON",after = 1)
#  
#  
#  # Write the updated Dockerfile to disk
#  docker_obj$write("Dockerfile")

## ----gitlab-ci, eval=FALSE----------------------------------------------------
#  # Copy the .gitlab-ci.yml file to the current directory
#  set_gitlab_ci(path = ".")
#  
#  # Specify runner tags
#  set_gitlab_ci(path = ".", tags = c("shiny_build", "prod"))

## ----github-actions, eval=FALSE-----------------------------------------------
#  # Copy the docker-build.yml file to the .github/workflows/ directory
#  set_github_action(path = ".")

