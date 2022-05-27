
let rec length = function
    | [] -> 0
    | h :: t -> 1 + (length t)

(* The given (tail-recursive) solution is
 *
 * let length list =
 *   let rec aux n = function
 *     | [] -> n
 *     | _ :: t -> aux (n + 1) t
 *   in
 *   aux 0 list
 *
 * Looking at the disassembly for our code, we (partially) have the following
 *
 * camlExample__length_5:
 *        subq    $8, %rsp
 *        cmpq    $1, %rax
 *        je      .L100
 *        movq    8(%rax), %rax
 *        call    camlExample__length_5@PLT
 * .L102:
 *        addq    $2, %rax
 *        addq    $8, %rsp
 *        ret
 * .L100:
 *        movl    $1, %eax
 *        addq    $8, %rsp
 *        ret
 *
 * Whoops! The call wasn't omitted, so our version isn't tail recursive.
 * This makes sense; we may have gotten used to how aggressive C++ compilers
 * can kind of be even when we aren't strictly writing in a TCO style.
 *
 * It should be noted that their solution mimics the accumulator-style arguments
 * of sum functions, except the starting-offset is hidden from the user through
 * the inner recursive function. If we didn't want to hide it, maybe we'd write
 * it like this:
 *)

let rec length_opt ?(start=0) l =
    match l with
    | [] -> start
    | h :: t -> length_opt ~start:(start + 1) t
