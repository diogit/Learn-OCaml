let move p dp = {x = p.x +. dp.dx; y = p.y +. dp.dy; z = p.z +. dp.dz};;

let next obj =
  {
    position = move obj.position obj.velocity;
    velocity = obj.velocity
  };;

let distance p1 p2 = sqrt(
    (p1.position.x +. p1.velocity.dx -. p2.position.x +. p2.velocity.dx) ** 2.
    +. (p1.position.y +. p1.velocity.dy -. p2.position.y +. p2.velocity.dy) ** 2.
    +. (p1.position.z +. p1.velocity.dz -. p2.position.z +. p2.velocity.dz) ** 2.
  )
;; 
   

let distance p1 p2 =
  let nextP1 = next p1 in
  let nextP2 = next p2 in
  sqrt(
    (nextP1.position.x -. nextP2.position.x) ** 2.
    +. (nextP1.position.y -. nextP2.position.y) ** 2.
    +. (nextP1.position.z -. nextP2.position.z) ** 2.
  )
;; 

let will_collide_soon p1 p2 = distance p1 p2 <= 2.0;;