
<div style="display:flex; gap:20px; justify-content: flex-end;">
  <img src="/images/ouroborus.jpg" alt="Logo 1" style="height:65px;">
  <img src="/images/Logo_FAMAF_UNC_color.png" alt="Logo 2" style="height:85px;">
</div>


# 🐉 strangelooplab 

En este repostitorio se encuentra el código de los modelos lógicos realizados en el Trabajo Especial de grado "La computadora como laboratorio filosófico: experimentos y herramientas para el modelado de sistemas reflexivos". 

### Contenido

El código está dividido en tres librerías: **Hypersets**, la base de todo el trabajo; **HypersetsParadox**, una extensión para el modelado de paradojas; y **HypersetsIncScheme**, una modificación para el modelado del *Inclosure Scheme*.

Cada librería tiene un `Main.hs` interactivo, el cual ingresa ejemplos pertinentes al pipeline de **Hypersets**. Estos ejemplos son los que están en el escrito y algunos extra. Se generan los archivos dot y los diagramas asociados.

![Esquema](/images/general_utilities_types_functions.jpg)

En `/trabajo_especial` pueden encontrar el Trabajo Especial de grado. Los capítulo 3, 4 y 5 actúan como explicación de la implementación. También, están disponibles las notas del trabajo especial que documentan la cronología del trabajo y del desarrollo de la librería. 

En cuanto a la documentación del código, pueden generarla ejecutando el comando `cabal haddock` desde la raíz del repositorio. En `/trabajo_especial/diagrams` pueden encontrar los resultados de los experiemntos presentados en el Trabajo Especial.

### Ejecución

Desde la raíz del repositorio, ejecutar los siguientes comandos:

1. (importante) `cabal build all` 
2. `cabal run hypersets`
3. `cabal run hypersetsparadox`
4. `cabal run hypersetsincscheme`