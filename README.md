# dockerterm

dockerterm provides an [RStudio Addin](https://rstudio.github.io/rstudio-extensions/rstudio_addins.html) that enables a user to start a [docker container](https://www.docker.com) in the [RStudio terminal](https://blog.rstudio.com/2017/08/11/rstudio-v1-1-preview-terminal/). Initially, this is designed to be a quick way to run R code inside a specific container without the overhead of mounting a volume to Docker and then running RStudio Server through the browser. However, this is certainly not the only application.

The container launches with the current RStudio working directory mounted to `/r_session`. Therefore, any data or other resources within the current RStudio working directory will be available within the context of the container.

## Installation
`dockerterm` can currently be installed from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("blairj09/dockerterm")
```

## Example

Once installed, dockerterm provides an RStudio Addin that can be run from the Addins menu. When the addin is run, a [shiny](https://shiny.rstudio.com) application will launch in the Viewer pane which allows the user to specify either an existing Docker image or a new Docker image (which will be downloaded from [Docker Hub](https://hub.docker.com) and then run). Once the image/container is selected, the user can also specify which command is run within that container, although the default command of `R` is likely the most common use case. Once all selections have been made, the user can select Run at which point the defined container is started in a new terminal. Commands from the RStudio Source Pane can be sent to the active terminal with `CTRL/CMD + ALT + ENTER`.

![](https://github.com/blairj09/dockerterm/blob/master/img/gadget_demo.gif)

Several Docker container pre-configured with R and various packages and utilities (including RStudio Server) can be found on [rocker's Docker Hub](https://hub.docker.com/r/rocker/). Special thanks to the rocker team and their hardwork maintaining these images.

## Limitations
While this approach is convenient for testing and writing R code against an arbitrary R setup within a Docker, there are some significant limitations. First and foremost, RStudio *is not* directly connected to the R session running in Docker. This means that there is no code completion in the source pane or ability to use other RStudio features within the environment in Docker. This also means that help files and plots render within the container and not within RStudio.

If the full RStudio experience within Docker is desired, check out ROpenSci's [great set of tutorials](https://ropenscilabs.github.io/r-docker-tutorial/) on using Docker with R and RStudio. Essentially, this approach involves starting a Docker container with RStudio Server and interacting with R through the browser.
