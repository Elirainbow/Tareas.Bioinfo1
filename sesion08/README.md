Versión de blastp:
blastp: 2.16.0+
 Package: blast 2.16.0, build Nov 27 2024 09:40:57

Versión de mafft:
MAFFT v7.526 (2024/Apr/26)

De dónde se copiaron los datos:
/mnt/data/bioinfo1/jmiranda/sesion08/datos

¿Cuántos receptores LGR completos tiene la babosa y en qué cromosomas?
Como se puede ver en tabla_ej_8.md, la babosa tiene 2 receptores LGR realmente, en los cromosomas 6 y 10. 
Esto va más allá que solo comparar los datos genómicos con la base de datos, sino que es información filtrada por un camino de regreso 
base->datos para ver cuáles eran realmente LGR de la babosa. 

¿Por qué el gen dlaeveM00000909458 no es uno de ellos a pesar de tener E-values de 10⁻¹⁴ contra LGR5?
Porque el mero valor del e-value no es un indicador directo de ortología, ya que aún tiene que pasar otros filtros (como de identidad, covertura o 
una técnica inversa para determinar si ese gen igual "escogería ser LGR"). 
