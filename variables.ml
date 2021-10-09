
(*Los nombres de las variables deben empezar
por letra minuscula o guion bajo
let <variable> = <expr>
Toda variable tiene un ambito, en utop es todo lo que se lsigue despues
de la variable* *)
(*Let tambien puede usarse con in, cuya sintaxis permite un
uso limitado de la variable
let <variable> = <expr1> in <expr2>
Esto evalua expr1 y luewgo expr2 con la variable ligada a cualquier valor
producido por expr1, en pocas palabras, se usa la vairbale en expr2
con el valor devuelto en expr1* *)

let languajes = "Ocaml,Perl,C++,C";;

let dashed_languajes =
    let languajes = String.split_on_char ',' languajes in (*Una vinculacion let en un ambito interno
                                                            puede ensombrecer u ocultar la definicion de un ambito externo
                                                            asi como lo es languajes, fuera de este ambito
                                                            languajes seguira teniendo el mismo valor de arriba
                                                            en pocas palabras: se oculta la definicion original
                                                            pero una vez la definicion de dashed_langujes se ha completado
                                                            el ambito interno se cierra y la definicion de languajes vuelve
                                                            a la original
                                                            * *)
    String.concat "-" languajes
    ;;
(*val dashed_languajes : string = "Ocaml-Perl-C++-C"* *)

(**Casi siempre se usa expresiones anidadas let/in en un programa grande *)

let area_of_ring inner_radius outer_radius =
    let pi = acos (-1.) in
    let area_of_circle r = pi *. r *. r in
    let pi = 0. in (*No confundir secuencia de enlaces let con la modificacion de una variable* *)
    (*lo que hace esto es que sombrea la definicion original de pi a 0
    no la cambia por eso esto no tiene ningun efecto sobre la funcion
    y esto explcia la advertencia del programa que no se esta usando pi despues de la nueva definicion que es 0* *)
    area_of_circle outer_radius -. area_of_circle inner_radius
    ;;
    (*- : float = 47.123889803846893* *)

(*Concordancia de patrones con let; caracteristica: let admite el uso de patrones en el lado izquierdo* *)
let upcase_first_entry line =
    (*let (first::rest) = String.split_on_char ',' line in; val first : string = "que" val rest : string list = ["wewe"]* *)
    (*Es mejor usaqr match para esto, asi manejamos todos los casos posibles* *)
    match String.split_on_char '-' line with
    | [] -> assert false (*split.on_char devuelve al menos un elemento* *)
    | first::rest -> String.concat "," (String.uppercase first::rest)
    ;;