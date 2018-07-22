#
# A small demo app for the shinyviewr function
#
# devtools::install_github("nstrayer/shinysense")
library(shiny)
library(shinythemes)
library(shinysense)
library(tidyverse)
library(keras)

center_crop_photo <- function(photo){
  photo_height <- dim(photo)[1]
  photo_width <- dim(photo)[2]
  edge_diff <- photo_height - photo_width
  crop_distance <- round(abs(edge_diff)/2)
  # if our photo is already square we can just send it back as is. 
  if(crop_distance == 0) return(photo)
  
  # is the width greater than the height?
  wide_photo <- edge_diff < 0
  
  if(wide_photo){
    desired_width_index <- (1:photo_height) + crop_distance
    photo_cropped <- photo[,desired_width_index, ]
  } else {
    desired_height_index <- (1:photo_width) + side_diff
    photo_cropped <- photo[desired_height_index,, ]
  }
  photo_cropped
}
# instantiate the model
# model <- application_mobilenet_v2(weights = 'imagenet')
model <- application_resnet50(weights = 'imagenet')

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Shinyviewr!"),
  p(
    'This is a demo of the function',
    code('shinyviewr'),
    'from the', code('shinysense'), 'package.',
    code('shinyviewr'), 'allows you to take photos with your webcam and send them to shiny for all your photo-driven goodness.'
    ),
  p(
    'This app uses the package', a(href = 'https://keras.rstudio.com', 'Keras'),
    'to run your last snapshot through a neural network trained on',
    a(href = 'https://keras.rstudio.com', 'imagenet.'),'It will guess which of the 1,000 classes it was trained to detect it',
    'is contained in your photo.'
  ),
  hr(),
  fluidRow(
    column(width = 7,
           h3("Webcam"),
           shinyviewrUI("myCamera", height = '250px')),
    column(width = 4, offset = 1,
           h3('Last Photo'),
           imageOutput("snapshot", height = '250px')
    )
  ),
  h3("Predictions"),
  plotOutput("predPlot")
)


server <- function(input, output) {
  
  #server side call of the drawr module
  myCamera <- callModule(shinyviewr,"myCamera", outputWidth = 500, outputHeight = 500)
  
  #logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  observeEvent(myCamera(), {
    
    # this gets rid of the opacity channel so kindly given to us by the png
    photo <- myCamera()[,,-4] %>% 
      center_crop_photo()
    
    photo_processed <- photo %>% 
      image_array_resize(224, 224) %>% 
      {.*255} %>%  #get image to 0-255 instead of 0-1
      array_reshape(c(1, dim(.))) %>% 
      imagenet_preprocess_input()
    
    # make predictions then decode and print them
    preds <- model %>% 
      predict(photo_processed) %>% 
      imagenet_decode_predictions(top = 20) %>% 
      .[[1]]
    
    output$predPlot <- renderPlot({
      preds %>% 
        mutate(
          class_description = str_replace(class_description,'_', ' ')
        ) %>% 
        ggplot(aes(x = reorder(class_description, score), y = score)) +
        geom_pointrange(aes(ymin = 0, ymax = score)) +
        coord_flip() +
        theme_minimal() +
        theme(
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 14)
        ) +
        labs(x = '')
    })
    
    output$snapshot <- renderPlot({
      par(mar=rep(0, 4))
      photo %>% 
        as.raster() %>% 
        plot()
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
