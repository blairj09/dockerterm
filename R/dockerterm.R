#' dockerterm: Run R code in arbitrary docker containers from within RStudio
#'
#' This package provides an RStudio Addin that makes it easy to create a
#' docker container in the terminal pane and then run R code from a source
#' file in RStudio within that docker container. There are several inherint
#' limitations to this model. Namely, RStudio isn't connected to the R session
#' in Docker. Therefore, code completion in RStudio doesn't map to the R
#' environment within Docker and plots rendered within the Docker container
#' don't show up in RStudio. For a full fledged "R in Docker Experience" it's
#' better to run RStudio Server from a Docker Container. However, this Addin
#' provides the ability to quickly run R code against an arbitrary R backend
#' within a specified Docker container.
#'
#' @docType package
#' @name dockerterm
NULL

#' Create a new terminal window with a R running in a Docker container
#'
#' \code{create_docker_terminal} creates a docker container running R in a new
#' terminal window. Code from the RStudio source pane can be sent to this
#' container with \strong{CTRL/CMD + ALT + ENTER}
#'
#' @param id character. Docker image id to use for the new container.
#'
#' @return The terminal id for the created terminal.
create_docker_terminal <- function(id) {
  docker_term_id <- rstudioapi::terminalCreate(caption = paste0("Docker_", id), show = TRUE)
  rstudioapi::terminalSend(
    docker_term_id,
    paste0("docker run -it --rm --name ",
           docker_term_id,
           " ",
           id,
           " R\n")
  )
  docker_term_id
}

#' Run dockerterm shiny gadget
#'
#' \code{dockerterm} runs a shiny gadget that allows the user to specify a Docker
#' container or image to run in the terminal window. The Docker container must
#' have R already installed as this is the command invoked when the image is run.
#' If the specified image is not found on the users machine, it will be downloaded
#' from Dockerhub and run.
#'
#' @export
dockerterm <- function() {
  docker_images <- system2(
    "docker",
    args = c("image", "ls"),
    stdout = TRUE
  )

  images_list <- lapply(docker_images, strsplit, " +")[-1]

  docker_image_df <- data.frame(
    repo = sapply(images_list, function(x) x[[1]][[1]]),
    tag = sapply(images_list, function(x) x[[1]][[2]]),
    id = sapply(images_list, function(x) x[[1]][[3]]),
    size = sapply(images_list, function(x) x[[1]][[7]]),
    stringsAsFactors = FALSE
  )

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("dockerterm"),
    miniUI::miniContentPanel(
      shiny::fillRow(
        DT::DTOutput(
          outputId = "docker_images",
          height = 700
        )
      )
    )
  )

  server <- function(input, output, session) {
    output$docker_images <- DT::renderDT({
      DT::datatable(
        docker_image_df,
        selection = "single"
      )
    })

    selected_docker_id <- reactive({
      selected_row <- input$docker_images_rows_selected
      docker_image_df[selected_row, "id"]
    })

    # Handle done command
    observeEvent(input$done, {
      # Create and run docker container in new terminal tab
      terminal_id <- create_docker_terminal(selected_docker_id())
      shiny::stopApp(paste0("Terminal id: ", terminal_id))
    })
  }

  viewer <- shiny::paneViewer(minHeight = 300)

  shiny::runGadget(ui, server, viewer = viewer)
}
