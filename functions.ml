
(*Funciones anonimas
Una funcion anonima es una funcion que se declara sin ser nombrada
se declara usando la palabra clave fun* *)
let () = 
    (*Esta funcion anonima toma el valor de 7 y se ejecuta* *)
    (fun x -> x + 1) 7 (**- : int = 8 ; asi se aplica una funcion anonima a un argumento*)
    List.map (fun x -> x + 1) [1; 2; 3] (*- : int list = [2; 3; 4]* toma la funcion y los argumentos
                                        y se va pasando cada elemento uno por uno dentro de la funcion anonima *)

    (*val increments : (int -> int) list = [<fun>; <fun>]* *)
    let increments = [(fun x -> x + 1); (fun x -> x + 2)] (*Incluso se las puede meter en una estructura de datos* *)
    List.map (fun x -> x 5) increments (*- : int list = [6; 7]* mapea cada funcion de la lista y la toma la funcion anonima y la ejecuta *)
    
    (*Definiendo funcion con let y anonima* *)
    let plusone = (fun x -> x + 1) (* plusone 1 = 2* *)
    (*Estas dos definiciones son equivalentes, es decir de ambas formas se pueden declarar una funcion nombrada* *)
    let plusone x = x + 1 (*Forma mas comun y conveniente de declarar funciones* *)
    
    (*Funciones multiargumento* *)

    let abs_diff x y = abs (x - y)

    let abs_diff = (fun x -> (fun y -> abs (x - y))) (*Esto es en realidad una funcion de un argumento que devuelve
                                                    otra funcion de un argumento que a su vez devuelve el resultado final
                                                    las funciones estan anidadas por eso tiene acceso tanto a x como y
                                                    en abs (x - y) este estilo de funcion se denomina funcion currificada
                                                    gracias a haskell curry; la clave para enteder esto es -> es asociativo
                                                    a la derecha, por l otanto la firma de la funcion anterior
                                                    puede ser tambien: val abs_diff : int -> (int -> int) * *)
    
    let dist_from_3 = abs_diff 3 (*Como el valor del argumento de la segunda funcion no esta, entonces esta retorna la ultima funcion* *)
    dist_from_3 8 
    
    (*La practica de aplicar algunos argumentos a una funcion para obtener otra funcion se llama aplicacion parcial* *)
    (*Fun tiene su propia sintaxis que admite el curriying, por lo que esta definicion y la anterior son equivalentes *)
    let abs_diff = (fun x y -> abs (x - y))
    (*Nota: las funciones currificadas solo tiene un pequeño coste, no son caras* *)

    (*Tambien se puede aplicar tuplas como diferentes argumentos de una funcion* *)

    let abs_diff (x, y) = abs (x - y) (*abs_diff (3, 6)* - : int = 3 *)
    (*Nota: aqui uno no puede aplicar currying por el uso de tuplas y ademas no se hace uso de fun,
    no se puede haber aplicacion parcial, ambos estilos de funciones esta bien pero ocaml se maneja mas con funciones de curry
    ya que ese es su estilo por defecto* *)

    (*Funciones recursivas* *)
    let rec find_first_stutter list =
        match list with
        | [] | [_] -> (*Este es un patron or, lo cual dice que se haga lo de abajo si list coincide con cualquiera de las dos
                        coincida con [] que quiere decir que tiene 0 elementos o con [_] que quiere decir que tiene solo 1 elemento
                        ponemos _ para no tener que nombrar ese elemento * *)
        (* only zero or one elements, so no repeats *)
            None
        | x::y::tl ->
            if x = y then Some x else find_first_stutter (y::tl)

    let rec is_even x = (*Podemos definir multiples valores mutuamente recursivos con and* *)
        if x = 0 then true else is_odd (x - 1) (*val is_even : int -> bool = <fun> - val is_odd : int -> bool = <fun>* *)
    and is_odd x =
        if x = 0 then false else is_even (x - 1)

    (*Operadores de infijo y prefijo* *)
    Int.max 4 5 (*Prefijo* *)
    4 + 6 (*Infijo*tal vez no se vea como funcion pero esto tambien es una funcion, pero se escribe con parentesiso solo *)
    (+) 5 6 (*=11* es el equivalente a lo de arriba solo que usando funcion de prefijo *)
    (*Nota: Una función se trata sintácticamente como un operador si el nombre de esa función se elige
    de entre un conjunto especializado de identificadores de ocaml:
    ! $ % & * + - . / : < = > ? @ ^ | ~
    * *)
    let (+!) (x1, y1) (x2, y2) = (x1 + x2, y1+ y2) (*Podemos definir o redefinir el significado de un operador
                                                    ejemplo de operador de suma de vectores sobre dos  pares de de numeros* *)
    (3, 2) +! (-2, 4) (*- : int * int = (1, 6)* *)
    
    let (!=) p1 p2 = p1 <> p2
    true != true (*- : bool = false* *)

    let (***) x y = (x ** y) ** y (*Esto da error, hay que tener cuidado con * ya que esto puede interpretarse
                                    como un comentari, para esto se lo pone espacios al inicio y al final al usar * operador ¨* *)
    
    let ( *** ) x y = (x ** y) ** y

    let (|>) x f = f x

    let path = "/usr/bin:/usr/local/bin:/bin:/sbin"
    String.split_on_char ':' path 
    |> List.dedup String.compare  (*Lo que hace es coger el resultado de la izquiera y usarlo como lo declaraom en la funcion del operador
                                aqui tambien se esta haciendo aplicacion parcial, no se usa un argumento para usarlo con el resultado de lo que nos de arriba segun la funcion declarada de |>* *)
    |> List.iter print_endline (*|> funciona solo xq es asociativa a la izquierda* *)
    
    (*Con function*  es equivalente a hacerlo con match with*)
    let some_or_zero = function (*Function es otra forma de declarar funciones solo que en vez
                                de tener multi-argumento tiene una coincidencia de patrones incorporada para el unico argumento que tiene* *)
        | Some x -> x
        | None -> 0
    
    let some_or_default default = function (*Este es un ejemplo de combinacion de funcion ordinaria con function* *)
        | Some x -> x
        | None -> default

    (*Argumentos etiquetados* *)
    (*Los argumentos etiquetados sirven para permitir identificar a un arguemento por su nombre
    se los usa con una tilde una plica y dos puntos; ademas permite pasar los argumentos en el orden que querramos siempre y cuando usemos el nombre ~num:10* *)
    let ratio ~num ~denom = float num /. float denom

    let num = 23
    let denom 123
    (*ratio ~num ~denom* ocaml tambien soporta juego de palabras, si el nombre de la etiqueta es el mismo que una variable entonces se hara uso de la variable como su valor*)
    
    (*Funciones de orden superior y etiquetas* *)
    let apply_to_tuple f (first, second) = f ~first ~second
    let apply_to_tuple_2 f (first, second) = f ~second ~first
    (*val apply_to_tuple : (first:'a -> second:'b -> 'c) -> 'a * 'b -> 'c = <fun>
    val apply_to_tuple_2 : (second:'a -> first:'b -> 'c) -> 'b * 'a -> 'c = <fun>* *)
    let divide ~first ~second = first / second (*Da error, el orden los argumentos etiquetados dentro de uan funcion que esta en otra si es importante* *)
    
    (*Argumentos opcionales* *)
    (*Los argumentos opcionales son como los agumentos etiquetados se usan igual
    la unica diferencia es que la persona puede elegir si pasarlos o no* *)
    let concat ?sep x y =
        let sep = match sep with 
            | None -> "" 
            | Some x -> x 
        in
        x ^ sep ^ y

    (*concat "foo" "baar";; - : string = "foobaar"
    concat ~sep:"-" "foo" "baar";;  - : string = "foo-baar"* *)

    let concat ?(sep="") x y = x ^ sep ^ y (*Quiere decir que si no se pasa el sep argumento como defecto tendra "" valor, es una forma mas concisa de escribir lo anterior* *)
    
    concat ?sep:(Some ":") "foo" "bar" (*Esto sirve para pasar un some o un none, en vez de usar ~ se usa ?* *)
    concat ?sep:None "foo" "bar"

    let uppercase_concat ?(sep="") a b = concat ~sep (String.uppercase a) b
    (*
    uppercase_concat "Foo" "bar" ~sep:":"
    - : string = "FOO:bar"
    * *)
    let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b (*Podemos hacer que up_c pase el argumento opcional a concat usando ?sep* *)
    
    let numeris_deriv ~delta ~x ~y ~f =
        let x' = x +. delta in
        let y' = y +. delta in
        let base = f ~x ~y in
        let dx = (f ~x:x' ~y -. base) /. delta in
        let dy = (f ~x ~y:y' -. base) /. delta in
        (dx, dy)

    let numeris_deriv ~delta ~x ~y ~(f: x:float -> y:float -> float) =
        let x' = x +. delta in
        let y' = y +. delta in
        let base = f ~x ~y in
        let dx = (f ~y ~x:x' -. base) /. delta in (*Ahi si acepta que los argumentos de f se llamen en cualquier orden porque estamos declarando los tipos explicitamente* *)
        let dy = (f ~x ~y:y' -. base) /. delta in
        (dx, dy)
    
    (*Argumentos opcionales y aplicacion parcial* *)
    let colon_concat = concat ~sep:":"
    colon_concat "a" "b" (*- : string = "a:b* *)

    (*verficando parcialidad* *)
    let prepend_pund = concat "# "
    prepend_pund "a BASH comment" (*- : string = "# a BASH comment"* *)
    prepend_pund "a BASH comment" ~sep:":" (*Si intentamos pasar este argumento sera rechazado
                                        no exsite la aplicacion de parcialidad* *)

    (*La regla es: un argumento opcional se borra tan pronto como el primer argumento posicional (es decir, ni
    etiquetado ni opcional) definido después del argumento opcional es pasado.* *)

    let concat x ?(sep="") y =  x ^ sep ^ y 
    let prepend_pound = concat "# "  (*Esta aplicacion del primer argumento no haria que el argumento opcional se borre* *)
    prepend_pound "a BASH comment"
    prepend_pound "a BASH comment" ~sep:"--- "

    let concat x y ?(sep="") = x ^ sep ^ y (*Un argumento opcional que no tiene ningun argumento posicional siguiente no puede ser borrado* *)
    concat "a" "b"; (*Y efectivamente, cuando proporcionamos los dos argumentos posicionales, el argumento sep no se
    borrado, sino que devuelve una función que espera que se proporcione el argumento sep:* *)
    ;;