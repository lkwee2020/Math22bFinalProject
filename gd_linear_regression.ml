(* Set a learning rate as specified in the problem *)
let cLEARNING_RATE = 0.01 ;;

let gradient_descent ((a,b) : float * float) : float * float =
  (* Calculate the gradient at the point (x0,y0) *)
  let grad_a = 4. *. (-. 7. +. 2. *. a +. 5. *. b) in
  let grad_b = ~-. 78. +. 20. *. a +. 60. *. b in
  (* Add the step size to the original point to get x_1 *)
  a -. cLEARNING_RATE *. grad_a, b -. cLEARNING_RATE *. grad_b ;;

let rec calc_nth_step (initial: float * float) (n : int) = 
  if n = 0 then initial
  else calc_nth_step (gradient_descent initial) (pred n) ;;





(* The gradient descent algorithm - takes in a initial point x_0 and returns x_1 *)
let gradient_descent1 ((a,b) : float * float) : float * float =
  (* calculating the gradient at an the point (x0,y0) *)
  let grad_a = -. 16. +. 8. *. a -. 32. *. b in
  let grad_b = 82. -. 32. *. a +. 180. *. b in
  (* adding the step size to the original point to get x_1 *)
  a -. cLEARNING_RATE *. grad_a, b -. cLEARNING_RATE *. grad_b ;;

(* A recursive function that calculates the nth gradient descent step given an initial point *)
let rec calc_nth_step1 (initial: float * float) (n : int) = 
  if n = 0 then initial
  else calc_nth_step1 (gradient_descent1 initial) (pred n) ;;


