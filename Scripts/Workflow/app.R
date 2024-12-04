# Install necessary packages if not already installed
# install.packages(c("shiny", "ggplot2", "DT", "dplyr"))

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

ui <- fluidPage(
    titlePanel("Workflow Builder with ggplot2"),
    sidebarLayout(
        sidebarPanel(
            selectInput("shape_type", "Choose Shape Type:", choices = c("Square", "Diamond")),
            numericInput("shape_x", "X Coordinate:", value = 0, step = 0.5),
            numericInput("shape_y", "Y Coordinate:", value = 0, step = 0.5),
            actionButton("add_shape", "Add Shape"),
            br(), br(),
            DTOutput("shapes_table"),
            br(),
            actionButton("delete_shape", "Delete Selected Shape"),
            br(), br(),
            actionButton("output_code", "Output Code and DataFrame"),
            br(), br(),
            verbatimTextOutput("code_output")
        ),
        mainPanel(
            plotOutput("workflow_plot", height = "600px")
        )
    )
)

server <- function(input, output, session) {
    # Reactive values to store shapes data
    shapes <- reactiveVal(data.frame(
        id = integer(),
        x = numeric(),
        y = numeric(),
        shape = character(),
        stringsAsFactors = FALSE
    ))
    
    # Counter for assigning unique IDs
    shape_id_counter <- reactiveVal(1)
    
    # Observe add shape button
    observeEvent(input$add_shape, {
        shape_data <- shapes()
        new_id <- shape_id_counter()
        
        # Get user inputs
        new_x <- input$shape_x
        new_y <- input$shape_y
        new_shape_type <- input$shape_type
        
        # Create new shape data
        new_shape <- data.frame(id = new_id, x = new_x, y = new_y, shape = new_shape_type, stringsAsFactors = FALSE)
        
        # Update shapes data
        shapes(rbind(shape_data, new_shape))
        
        # Increment shape ID counter
        shape_id_counter(new_id + 1)
    })
    
    # Observe delete shape button
    observeEvent(input$delete_shape, {
        selected <- input$shapes_table_rows_selected
        if (length(selected)) {
            shape_data <- shapes()
            shape_data <- shape_data[-selected, ]
            shapes(shape_data)
        }
    })
    
    # Output the ggplot
    output$workflow_plot <- renderPlot({
        shape_data <- shapes()
        
        gg <- ggplot()
        
        # Add squares
        squares <- subset(shape_data, shape == "Square")
        if (nrow(squares) > 0) {
            gg <- gg + 
                geom_rect(data = squares,
                          aes(xmin = x - 0.5, xmax = x + 0.5,
                              ymin = y - 0.5, ymax = y + 0.5),
                          fill = "skyblue", color = "black", alpha = 0.6)
        }
        
        # Add diamonds using the provided method
        diamonds <- subset(shape_data, shape == "Diamond")
        if (nrow(diamonds) > 0) {
            # Define xmin, xmax, ymin, ymax for each diamond
            diamonds <- diamonds %>%
                mutate(
                    xmin = x - 0.5,
                    xmax = x + 0.5,
                    ymin = y - 0.5,
                    ymax = y + 0.5
                )
            
            diff <- 0.1
            diamond_list <- list()
            
            for (i in 1:nrow(diamonds)) {
                diam_a <- diamonds[i,]
                
                df <- data.frame(  
                    x = c(diam_a$xmin - diff, diam_a$xmin + diff, diam_a$xmax + diff, diam_a$xmax - diff),
                    y = c(diam_a$ymin, diam_a$ymax, diam_a$ymax, diam_a$ymin),
                    group = diam_a$id
                )
                diamond_list[[i]] <- df
            }
            diamond_polygons <- do.call(rbind, diamond_list)
            
            gg <- gg + 
                geom_polygon(data = diamond_polygons,
                             aes(x = x, y = y, group = group),
                             fill = "orange", color = "black", alpha = 0.6)
        }
        
        # Add text labels
        if (nrow(shape_data) > 0) {
            gg <- gg + 
                geom_text(data = shape_data,
                          aes(x = x, y = y, label = id),
                          color = "black", size = 5)
        }
        
        # Final plot adjustments
        gg <- gg + 
            coord_fixed(ratio = 1) +
            theme_minimal() +
            theme(panel.grid.major = element_line(color = "grey80", size = 0.5),
                  panel.grid.minor = element_blank(),
                  axis.text = element_blank(), axis.title = element_blank())
        
        # Display the plot
        gg
    })
    
    # Output table to show current shapes
    output$shapes_table <- renderDT({
        datatable(shapes(), selection = 'single', editable = FALSE)
    })
    
    # Output code and dataframe when button is clicked
    observeEvent(input$output_code, {
        # Capture the current shape data
        shape_data <- shapes()
        
        # Generate the code used to build the plot
        plot_code <- paste0(
            "ggplot() +\n",
            if (nrow(subset(shape_data, shape == "Square")) > 0) {
                "  geom_rect(data = squares,\n,
                            aes(xmin = x - 0.5, xmax = x + 0.5,\n,
                             ymin = y - 0.5, ymax = y + 0.5),\n,
                           fill = 'skyblue', color = 'black', alpha = 0.6) +\n"
            } else "",
            if (nrow(subset(shape_data, shape == "Diamond")) > 0) {
                "  geom_polygon(data = diamond_polygons,\n,
                               aes(x = x, y = y, group = group),\n,
                               fill = 'orange', color = 'black', alpha = 0.6) +\n"
            } else "",
            "  geom_text(data = shape_data,\n,
                       aes(x = x, y = y, label = id),\n,
                       color = 'black', size = 5) +\n,
             coord_fixed(ratio = 1) +\n,
             theme_minimal() +\n,
             theme(panel.grid.major = element_line(color = 'grey80', size = 0.5),\n,
                   panel.grid.minor = element_blank(),\n,
                   axis.text = element_blank(), axis.title = element_blank())"
        )
        
        # Combine the code and data into a single string
        output_text <- paste(
            "## Data Frame Used:\n",
            capture.output(print(shape_data)),
            "\n## Code Used to Build the Plot:\n",
            plot_code,
            sep = "\n"
        )
        
        # Output the code and data
        output$code_output <- renderText({
            output_text
        })
    })
}

shinyApp(ui, server)
