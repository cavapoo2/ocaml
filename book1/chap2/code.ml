let mul10 a = 10 * a

 let both a b = 
   (a <> 0) && (b <> 0)


 let sum' n =
   let rec sumInner n acc = 
     if n <= 0 then acc else sumInner (n-1) (acc+n) in
   sumInner n 0;;

let rec pow x n = 
  if n == 0 then 1 else x * pow (x) (n-1);;

let isconsonant c =
  if c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'
 then false else
   true;;
