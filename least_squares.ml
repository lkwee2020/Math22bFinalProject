let gradient_descent (a,b : float * float) (cLEARNING_RATE : float): float * float =
  (* Calculate the gradient at the point (a0,b0) *)
  let grad_a = 8. *. (~-.2. +. a) in
  let grad_b = 90. *. (~-. 1. +. 2. *. b) in
  (* Add the step size to the original point to get x_1 *)
  a -. cLEARNING_RATE *. grad_a, b -. cLEARNING_RATE *. grad_b ;;

(* A recursive function that calculates the nth gradient descent step given an initial point *)
let rec calc_nth_step (initial: float * float) (cLEARNING_RATE : float) (n : int) = 
  if n = 0 then initial
  else calc_nth_step (gradient_descent initial cLEARNING_RATE) cLEARNING_RATE (pred n) ;;








