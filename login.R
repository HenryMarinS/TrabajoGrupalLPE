#Cargar en R el login hecho en python
library(shiny)

reticulate::use_python("C:/Anaconda3/python.exe")

reticulate::py_config()

reticulate::source_python("loginLPE.py")

registro("enrike2190@gmail.com","12345678")

#prueba erronea login
login("enrike2190@gmail.com","12345123")

#prueba exitosa del login
login("enrike2190@gmail.com","12345678")

#no funciona por falta de permisos
#datosUsuario ("enrike2190@gmail.com","Enrique","6788KHM","Citroen","celysse")