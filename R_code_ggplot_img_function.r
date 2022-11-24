##### let's write a Function to create ggplot images #####

ggplot_img <- function(temp_obj, temp_title)  
      # 'function' Function (base package) creates a function in R. The Arguments 'temp_obj' 
      # and temp_title are the ones we pass and we return 'ggp_obj'
{
  ggp_obj <- ggplot() +
  geom_raster(temp_obj, mapping =aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis(option = "inferno") +
  ggtitle(temp_title)
  return(ggp_obj)
}
