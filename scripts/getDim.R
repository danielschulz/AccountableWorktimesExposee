getDim = function(width, height) {
  
  x = 12.5 - (width / 2)
  y =  8.5 - ((height^(354 / 377)) / 2)
  
  x = round(x, 2)
  y = round(y, 2)
  
  paste(x, y, sep = "; ", collapse = NULL)
}
