library(shiny)
library(shinysense)
library(tidyverse)
library(keras)

# instantiate the model
model <- application_resnet50(weights = 'imagenet')
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Imagenet Classifier"),
  fluidRow(
    column(width = 7, h3("Webcam"),
      shinyviewrUI("myCamera", height = '250px') ),
    column(width = 4, offset = 1,  h3('Predictions'),
      plotOutput("predPlot"))
  ),
  h3("Last Photo"),
  imageOutput("snapshot", height = '250px') 
)
server <- function(input, output) {
  #server side call of the viewr module
  myCamera <- callModule(
    shinyviewr, "myCamera", 
    outputWidth = 224, 
    outputHeight = 224 )
  # Watch for photos being taken
  observeEvent(myCamera(), {
    photo_processed <- myCamera() %>% 
      {.*255} %>%  #get image to 0-255 instead of 0-1
      {
        this <- .
        write_rds(this, 'webcam_photo.png')
        this
      } %>% 
      array_reshape(c(1, dim(.))) %>% 
      imagenet_preprocess_input()
    # make predictions then decode and print them
    preds <- model %>% 
      predict(photo_processed) %>% 
      imagenet_decode_predictions(top = 8) %>% 
      .[[1]]
    # Plot predictions
    output$predPlot <- renderPlot({
      preds %>% 
        mutate(
          class_description = str_replace(
            class_description, '_', ' ' )
        ) %>% 
        ggplot(aes(
          x = reorder(class_description, score), 
          y = score) ) + coord_flip() +
        geom_pointrange(aes(ymin = 0, ymax = score))
    })
    # show photo
    output$snapshot <- renderPlot({
      plot(as.raster(myCamera()))
    })
  })
}
# Run the application
shinyApp(ui = ui, server = server)
