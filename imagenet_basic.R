library(keras)
library(tidyverse)

# instantiate the model
model <- application_mobilenet_v2(weights = 'imagenet')
# model <- application_resnet50(weights = 'imagenet')

elephant <- image_load(
  "data/elephant.jpg", 
  target_size = c(224,224)
) %>% 
  image_to_array()

photo <- read_rds('data/snapshot.rds')[,,-4]

cat

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

center_crop_photo(photo) %>% dim()
center_crop_photo(photo_cropped) %>% dim()

desired_width_index <- (1:photo_height) + side_diff

dim(photo_cropped)

par(mar=rep(0, 4))
photo_cropped %>% 
  as.raster() %>% 
  plot()

smallest_edge <- min(photo_height, photo_width) 

edge_difference <- 

photo <- read_rds('data/snapshot.rds')[,,-4] %>% 
  image_array_resize(224, 224) %>% 
  {.*255}

par(mar=rep(0, 4))
read_rds('data/snapshot.rds')[,,-4] %>% 
  as.raster() %>% 
  plot()

(cat/255) %>% 
  as.raster() %>% 
  plot()

process_image <- . %>% 
  array_reshape(c(1, dim(.))) %>% 
  imagenet_preprocess_input()


processed_image_elephant <- elephant %>% process_image()
processed_image_cat <- cat %>% process_image()

summary(cat)
summary(elephant)

summary(processed_image_elephant)
summary(processed_image_cat)

predict_on_image <- function(image){
  model %>% 
    predict(image) %>% 
    imagenet_decode_predictions(top = 10) %>% 
    .[[1]]
}

preds <- predict_on_image(processed_image_elephant)
preds <- predict_on_image(processed_image_cat)

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
