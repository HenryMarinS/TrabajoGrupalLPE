# Proyecto GasoLITE
## Grupo 4 Lenguajes de Programación Estadística
## Índice

1. [Aplicación](#ind1)
2. [Archivos en el relevantes repositorio](#ind2)
3. [Miembros del equipo](#ind3)

### Aplicación<a name="ind1"></a>

Esta aplicación se ha desarrollado en base a los datos abiertos del gobierno y su principal función es obtener información útil de una forma rápida sobre gasolineras cercanas. Tambien cuenta con información en forma de estádisticos y la posibilidad de descargar un informe. En la interfaz de la aplicación se pueden identificar los siguientes elementos clave:

#### Sidebar Principal

Panel lateral donde podemos seleccionar diferentes variables como: comunidad autónoma, provincia, etc. Aquí se situa el botón buscar el cual incia gran parte de las funcionalidad de nuestra app.

#### Mapa

Mapa donde se muestran las gasolineras según los parámetros introducidos en la Sidebar.

#### Mapa por ubicación

Pestaña donde se puede introducir una dirección y una distancia para mostrar las gasolineras cercanas. 

#### Dashboard

Pestaña con diferentes estadísticos útiles generados a partir de los datos introducidos en la Sidebar.

#### Empresario

Pestaña para descargar un informe con información útil para el empresario con los datos introducidos en la Sidebar.

#### Sugerencias

Pestaña donde se pueden enviar sugerencias para mejorar la app. Estos datos se almacenan en la sección de sugerencias en nuestra database de Firebase.

#### Login

Pestaña con 3 opciones para el usuario relacionadas con el login:
- Login: login del usuario a Firebase y recupera la información rellenada en el registro y lo autocompleta en la app.
- Registro: registro del usuario a Firebase que tambien almacena unas variables 'favoritas' y lo sube a nuestra database en la sección de users
- Actualización: introduce los datos de login y nuevas variables para actualizar la información del usuario en la database

### Archivos en el relevantes repositorio<a name="ind2"></a>

- server.R &rarr; parte del servidor de nuestra aplicación
- ui.R &rarr; parte de la interfaz de nuestra aplicación
- Trabajo Grupal.R &rarr; archivo en R donde trabajabamos con los datos originales y haciamos diferentes pruebas para su posterior implementación en la app
- loginLPE.py &rarr; prueba de login y diferentes funciones para Firebase en python
- login.R &rarr; prueba en R del login usando la librería reticulate y el archivo loginLPE.py
- csv de datos &rarr; csv necesarios para la elaboración de un historico sobre el precio
- informe.Rmd &rarr; archivo que a través de la aplicación genera el informe con datos útiles sobre las gasolineras

### Miembros del equipo<a name="ind3"></a>

- Marta Díaz Fuentes
- Alba María Laguna Moraleda
- Andrea López del Hierro
- Enrique Marín Sánchez
- Emilia Marqués García
