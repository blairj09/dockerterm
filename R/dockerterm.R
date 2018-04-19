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
create_docker_terminal <- function(id, image = TRUE, entrypoint = "R") {
  docker_term_id <- rstudioapi::terminalCreate(caption = paste0("Docker_", id), show = TRUE)
  if (image == TRUE) {
    rstudioapi::terminalSend(
      docker_term_id,
      paste0("docker run -it --rm --name ",
             docker_term_id,
             " ",
             id,
             " ",
             entrypoint,
             "\n")
    )
  } else {
    rstudioapi::terminalSend(
      docker_term_id,
      paste0("docker start -i ",
             id,
             "\n")
    )
  }
  docker_term_id
}

#' Run dockerterm shiny gadget
#'
#' \code{dockerterm} runs a shiny gadget that allows the user to specify a Docker
#' container or image to run in the terminal window. The Docker container must
#' have R already installed as this is the command invoked when the image is run.
#' If the specified image is not found on the users machine, it will be downloaded
#' from Dockerhub and run.
dockerterm <- function() {
  # Get docker images
  docker_images <- system2(
    "docker",
    args = c("image", "ls"),
    stdout = TRUE
  )

  images_list <- lapply(docker_images, strsplit, " +")[-1]

  images_df <- data.frame(
    repo = sapply(images_list, function(x) x[[1]][[1]]),
    tag = sapply(images_list, function(x) x[[1]][[2]]),
    id = sapply(images_list, function(x) x[[1]][[3]]),
    size = sapply(images_list, function(x) x[[1]][[7]]),
    stringsAsFactors = FALSE
  )

  # Get docker containers
  docker_containers <- system2(
    "docker",
    args = c("container", "ls", "-a"),
    stdout = TRUE
  )

  containers_list <- lapply(docker_containers, strsplit, " +")[-1]

  containers_df <- data.frame(
    id = sapply(containers_list, function(x) x[[1]][[1]]),
    image = sapply(containers_list, function(x) x[[1]][[2]]),
    name = sapply(containers_list, function(x) x[[1]][[length(x[[1]])]]),
    stringsAsFactors = FALSE
  )

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      title = "dockerterm",
      right = miniUI::miniTitleBarButton("done", "Run", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::fillRow(
        flex = c(1, 3),
        shiny::fillCol(
          shiny::radioButtons(
            inputId = "docker_type",
            label = "Select one:",
            choices = c(
              "Existing Image",
              "New Image",
              "Existing Container"
            )
          ),
          shiny::textInput(
            inputId = "docker_entrypoint",
            label = "Docker entrypoint",
            value = "R"
          )
        ),
        fillCol(
          miniUI::miniContentPanel(
            shiny::uiOutput("docker_select")
          )
        )
      ),
      scrollable = FALSE
    )
  )

  server <- function(input, output, session) {
    output$docker_select <- shiny::renderUI({
      if (input$docker_type == "Existing Image") {
        DT::DTOutput(
          outputId = "docker_images"
        )
      } else if (input$docker_type == "New Image") {
        shiny::textInput(
          inputId = "new_docker_image",
          label = "Image name",
          placeholder = "repo/image"
        )
      } else {
        DT::DTOutput(
          outputId = "docker_containers"
        )
      }
    })

    output$docker_images <- DT::renderDT({
      DT::datatable(
        images_df,
        selection = "single",
        options = list(
          pageLength = 5
        )
      )
    })

    output$docker_containers <- DT::renderDT({
      DT::datatable(
        containers_df,
        selection = "single",
        options = list(
          pageLength = 5
        )
      )
    })

    selected_docker_id <- reactive({
      if (input$docker_type == "Existing Image") {
        selected_row <- input$docker_images_rows_selected
        images_df[selected_row, "id"]
      } else if (input$docker_type == "New Image") {
        input$new_docker_image
      } else {
        selected_row <- input$docker_containers_rows_selected
        containers_df[selected_row, "id"]
      }
    })

    # Handle done command
    observeEvent(input$done, {
      # Create and run docker container in new terminal tab
      if (input$docker_type != "Existing Container") {
        terminal_id <- create_docker_terminal(
          id = selected_docker_id(),
          image = TRUE,
          entrypoint = input$docker_entrypoint
        )
      } else {
        terminal_id <- create_docker_terminal(
          id = selected_docker_id(),
          image = FALSE
        )
      }
      shiny::stopApp(paste0("Terminal id: ", terminal_id))
    })
  }

  viewer <- shiny::paneViewer()

  shiny::runGadget(ui, server, viewer = viewer)
}
