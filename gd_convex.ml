(* Set a Learning Rate as specified in the problem *)
let cLEARNING_RATE = 0.1 ;;

(* The gradient descent algorithm - takes in an initial point x_0 and returns x_1 *)
let gradient_descent ((x0,y0) : float * float) : float * float =
  (* Calculate the gradient at the point (x0,y0) *)
  let grad_x0, grad_y0 = (2. *. x0), (4. *. y0) in
  (* Add the step to the original point to get x_1. Essentially x_1 = x_0 - t\nabla f(x_0). *)
  x0 -. cLEARNING_RATE *. grad_x0, y0 -. cLEARNING_RATE *. grad_y0 ;;

(* A recursive function that calculates the nth gradient descent step given an initial point *)
let rec calc_nth_step (initial: float * float) (n : int) = 
  if n = 0 then initial
  else calc_nth_step (gradient_descent initial) (pred n) ;;



