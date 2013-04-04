
let x = 'x';;
let y = 'y';;

let i = fun x -> x;;
let k = fun x y -> x;;
let s = fun x y z -> x z (y z);;

s i i;;

s (k (s i)) k x y;;

let t = i;;
let f = k i;;
k x y;;
k i x y;;
