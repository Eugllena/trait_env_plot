library(shiny)
library(ggplot2)

# interpolate in the scale of 10 x 10 m.

interp = function(x, y, z){
  stopifnot(is.vector(x)&&is.numeric(x),
            is.vector(y)&&is.numeric(y),
            is.vector(z)&&is.numeric(z))
  predLoc = expand.grid(x = seq(min(x), max(x), by = 10),
                        y = seq(min(y), max(y), by = 10)
  )
  tree = randomForest::randomForest(x = cbind(x, y), y = z, ntree = 2000, keep.forest = TRUE)
  z = predict(tree, newdata = predLoc)
  data.frame(predLoc, z)
}

read_data = function(file, encoding){
  
  # shiny will copy a file as a temporary file whose name doesn't have an extension,
  # readxl can't recognize a file without an extension,
  # so to get work done must add an extension to the temporary file.
  
  ext = tools::file_ext(file$name)
  file.rename(file$datapath, paste(file$datapath, ext, sep="."))
  data = rio::import(paste(file$datapath, ext, sep="."))
  
  # traslate the encoding of a file into the encoding of a system,
  # iconv()'s "x" argument must be a vector.
  
  names(data) = iconv(names(data), from = encoding, to = "")
  output = purrr::dmap_if(data, is.character, iconv, from = encoding, to = "")
  return(output)
}

mod_header = function(data){
  
  # some input file may contain asyntatic column names,
  # for example, names contain "()" will cause error in the ggplot function,
  # so here replace the original names with "h1", "h2", ...
  
  names(data) = paste0("h", seq_len(length(data)))
  return(data)
}

# Define UI for application

ui = fluidPage(
  
  # Application title
  
  titlePanel("Interactively View Your Tree Mapping and Traits Data"),
  
  fluidRow(
    column(2, 
           wellPanel(
             fileInput("env_file",
               label = "Environmental file",
               accept = c("txt/plain",
                          "application/vnd.ms-excel",
                          "text/csv",
                          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                          "application/vnd.oasis.opendocument.formula"
                         )
               )
           ),
           wellPanel(
             selectInput("env_encoding",
                         "Encoding",
                         choices = c("UTF-8", "GBK")
                        )
           ),
           wellPanel(uiOutput("env_gx_temp")),
           wellPanel(uiOutput("env_gy_temp")),
           wellPanel(uiOutput("env_var_temp"))
    ),
    column(8,
           fluidRow(column(6, wellPanel(uiOutput("family_temp"))),
                    column(6, wellPanel(uiOutput("species_temp")))
                   ),
           plotOutput("hist"),
           plotOutput("dist")
    ),
    column(2,
           wellPanel(
             fileInput("trait_file",
                       label = "Trait file",
                       accept = c("txt/plain",
                                  "application/vnd.ms-excel",
                                  "text/csv",
                                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                  "application/vnd.oasis.opendocument.formula"
                       )
             )
           ),
           wellPanel(
             selectInput("trait_encoding",
                         "Encoding",
                         choices = c("UTF-8", "GBK")
             )
           ),
           wellPanel(uiOutput("trait_gx_temp")),
           wellPanel(uiOutput("trait_gy_temp")),
           wellPanel(uiOutput("family_col_temp")),
           wellPanel(uiOutput("species_col_temp")),
           wellPanel(uiOutput("trait_var_temp"))
    )
  )
)

# Define server logic

server = function(input, output) {
  env_data = reactive({if(is.null(input$env_file)) return(NULL)
                       read_data(input$env_file, input$env_encoding)
                     })
  trait_data = reactive({if(is.null(input$trait_file)) return(NULL)
                         read_data(input$trait_file, input$trait_encoding)
                       })
  
  env_names = reactive(names(env_data()))
  trait_names = reactive(names(trait_data()))
  
  env_data_mod = reactive(mod_header(env_data()))
  trait_data_mod = reactive(mod_header(trait_data()))
  
  trait_var_name = reactive({
    x = names(trait_data_mod())[which(input$trait_var == trait_names())]
    ifelse(length(x), x, "NULL")
  })
  trait_gx_name = reactive(names(trait_data_mod())[which(input$trait_gx == trait_names())])
  trait_gy_name = reactive(names(trait_data_mod())[which(input$trait_gy == trait_names())])
  family_name = reactive(names(trait_data_mod())[which(input$family_col == trait_names())])
  species_name = reactive({
    x = names(trait_data_mod())[which(input$species_col == trait_names())]
    ifelse(length(x), x, "NULL")
  })
  env_var_name = reactive(names(env_data_mod())[which(input$env_var == env_names())])
  env_gx_name = reactive(names(env_data_mod())[which(input$env_gx == env_names())])
  env_gy_name = reactive(names(env_data_mod())[which(input$env_gy == env_names())])
  scale_name = reactive(input$trait_var)
  fill_name = reactive(input$env_var)
  
  output$env_gx_temp = renderUI(
    selectInput("env_gx",
                "x",
                choices = env_names()
    )
  )
  
  output$env_gy_temp = renderUI(
    selectInput("env_gy",
                "y",
                choices = env_names()
    )
  )
  
  output$env_var_temp = renderUI(
    selectInput("env_var",
                "Envrionmental variable",
                choices = c("NULL", env_names())
    )
  )
  
  output$trait_gx_temp = renderUI(
    selectInput("trait_gx",
                "x",
                choices = trait_names()
    )
  )
  
  output$trait_gy_temp = renderUI(
    selectInput("trait_gy",
                "y",
                choices = trait_names()
    )
  )
  
  output$family_col_temp = renderUI(
    selectInput("family_col",
                "Family",
                choices = c("NULL", trait_names())
    )
  )
  
  output$species_col_temp = renderUI(
    selectInput("species_col",
                "Species",
                choices = c("NULL", trait_names())
    )
  )
  
  output$trait_var_temp = renderUI(
    selectInput("trait_var",
                "Trait",
                choices = c("NULL", trait_names())
    )
  )
  
  family_list = reactive({
    if(input$family_col == "NULL") return(NULL)
    unique(trait_data_mod()[, family_name()])
  })
  
  output$family_temp = renderUI(
    selectInput("family",
                "Choose a family to display",
                choices = c("All", family_list())
    )
  )  
  
  fdata = reactive({
    if(input$family == "All") return(trait_data_mod())
    subset(trait_data_mod(), trait_data_mod()[, family_name()] == input$family)
  })
  
  species_list = reactive({
    if(input$species_col == "NULL") return(NULL)
    unique(fdata()[, species_name()])
  })
  
  output$species_temp = renderUI(
    selectInput("species",
                "Choose a Species to display",
                choices = c("All", species_list())
    )
  )
  
  sdata = reactive({
    if(input$species == "All") return(fdata())
    subset(fdata(), fdata()[, species_name()] == input$species)
  })
  
  output$hist = renderPlot({
    if(input$trait_var == "NULL") stop("Please select a trait variable!")
    ggplot(sdata(), aes_string(trait_var_name())) +
      geom_histogram(fill = "lightblue") +
      xlab(scale_name())
  })
  
  env_x = reactive(env_data_mod()[, env_gx_name()])
  env_y = reactive(env_data_mod()[, env_gy_name()])
  pred_dat = reactive(interp(env_x(), env_y(), env_data_mod()[, env_var_name()]))
  
  output$dist = renderPlot({
    if(is.null(env_data()) && is.null(trait_data())) 
      stop("Please upload one or both files!")
    if(is.null(trait_data()) && input$env_var == "NULL")
      stop("Please select an environmental variable!")
    if(input$env_var == "NULL"){
      ggplot(sdata(), aes_string(x = trait_gx_name(), y = trait_gy_name())) +
        geom_point(aes_string(colour = species_name(), size = trait_var_name())) +
        scale_size(name = scale_name(), range = c(0.1, 3)) +
        labs(x = "x", y = "y", colour = "species")
    }else if(is.null(trait_data())){
      ggplot(pred_dat(), aes(x = x, y = y, z = z)) +
        geom_raster(aes(fill = z)) +
        geom_contour(colour = "black") +
        scale_fill_gradient(low = "yellow", high = "red") +
        labs(fill = fill_name())
    }else{
      ggplot(pred_dat(), aes(x = x, y = y, z = z)) +
        geom_raster(aes(fill = z)) +
        geom_contour(colour = "black") +
        scale_fill_gradient(low = "yellow", high = "red") +
        geom_point(data = sdata(),
                   mapping = aes_string(x = trait_gx_name(),
                                        y = trait_gy_name(),
                                        colour = species_name(),
                                        size = trait_var_name()
                   ),
                   inherit.aes = FALSE
        ) +
        scale_size(name = scale_name(), range = c(0.1, 3)) +
        labs(colour = "species", fill = fill_name())
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
