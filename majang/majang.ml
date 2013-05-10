(*
順子、面子…とかを型にするか？

Syuntsu



Manzu(x)
Souzu(x)
Pinzu(x)
N E W S
Hatsu Haku Tyunn

*)

let (m1,m2,m3,m4,m5,m6,m7,m8,m9) = (1,2,3,4,5,6,7,8,9);;

let (s1,s2,s3,s4,s5,s6,s7,s8,s9) =(11,12,13,14,15,16,17,18,19);;

let (p1,p2,p3,p4,p5,p6,p7,p8,p9) = (21,22,23,24,25,26,27,28,29);;

let (w,s,e,n,hatsu,haku,tyun) = (0,10,20,30,31,32,33);;

let maxpai = 34;;

let rec random_pai n =
  if n <= 0 then []
  else
    Random.int maxpai :: random_pai (n - 1);;

let set_format pai_list =
  let pai_array = Array.make maxpai 0 in
  let setarr arr x = Array.set arr x ((Array.get arr x) + 1);arr in
  List.fold_left setarr pai_array pai_list;;


let getMentsu 
