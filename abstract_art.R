library(keras)
library(tidyverse)

photo <- read_rds('webcam_photo.png')/255

photo_dims <- photo %>% dim()
photo_width <- photo_dims[2]
photo_height <- photo_dims[1]

get_rgb <- function(x,y) photo[x,y,c(1,2,3)]
  
x_y_combs <- expand.grid(
  x = 1:photo_width,
  y = 1:photo_height
) 

rgb_mat <- purrr::map2(x_y_combs$x, x_y_combs$y, get_rgb) %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = TRUE)

location_mat <- x_y_combs %>% as.matrix()

# go from predicted rbg values back to a 3d array for raster plotting.
mat_to_raster <- function(rgb_mat){
  photo_matrix <- array(0, dim = c(photo_width, photo_height, 3))
  
  for(row in 1:nrow(x_y_combs)){
    x_pos <- x_y_combs$x[row]
    y_pos <- x_y_combs$y[row]
    photo_matrix[x_pos,y_pos,] <- rgb_mat[row,]
  }
  photo_matrix %>% as.raster()
}





hidden_units <- 20
num_hidden_layers <- 6
last_activation <- 'sigmoid'

model <- keras_model_sequential()
model %>% 
  layer_dense(
    input_shape = c(2),
    units = hidden_units,
    activation = 'relu',
    kernel_initializer = initializer_variance_scaling(scale = 2.5, mode = 'fan_out')
  ) 

for(i in 1:6){
  model %>% 
    layer_dense(
      units = hidden_units,
      activation = 'relu',
      kernel_initializer = initializer_variance_scaling(scale = 2.5, mode = 'fan_out')
    )
}

model %>% 
  layer_dense(
    units = 3,
    activation = last_activation,
    kernel_initializer = initializer_variance_scaling(scale = 2.5, mode = 'fan_out')
  )


# Compile model
model %>% compile(
  loss = "mean_squared_error",
  optimizer = "adamax",
  metrics = "mean_squared_error"
)

trainHistory <- model %>%
  fit(
    x = location_mat, y = rgb_mat,
    epochs = 5
  )

output_rgb <- model %>% predict(location_mat)

par(mfrow = c(1,2))
as.raster(photo) %>% plot()
mat_to_raster(output_rgb) %>% plot()

