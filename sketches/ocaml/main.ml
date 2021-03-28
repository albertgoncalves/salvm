type type' =
    | Int
    | Float

type token =
    | Comma
    | Semicolon
    | LParen
    | RParen
    | Equals
    | Plus
    | TType of type'
    | TInt of int
    | TFloat of float
    | TVar of string

type token' = (int * token)

type expr =
    | EInt of int
    | EFloat of float
    | EVar of string
    | EAssign of (expr' * expr')
    | ECall of (string * expr' list)

and expr' = (int * expr)

type stmt =
    | SDecl of (type' * string)
    | SEffect of expr'
    | SEmpty

type stmt' = (int * stmt)

let type_to_string : type' -> string = function
    | Int -> "Int"
    | Float -> "Float"

let token_to_string : token -> string = function
    | Comma -> ","
    | Semicolon -> ";"
    | LParen -> "("
    | RParen -> ")"
    | Equals -> "="
    | Plus -> "+"
    | TType x -> type_to_string x |> Printf.sprintf "Type %s"
    | TInt x -> Printf.sprintf "TInt %d" x
    | TFloat x -> Printf.sprintf "TFloat %.2f" x
    | TVar x -> Printf.sprintf "TVar %s" x

let is_digit (x : char) : bool =
    ('0' <= x) && (x <= '9')

let get_token : string -> token = function
    | "i32" -> TType Int
    | "f32" -> TType Float
    | x ->
        try TInt (int_of_string x) with _ ->
        try TFloat (float_of_string x) with _ -> TVar x

let split_to_tokens (s : string) : token' Queue.t =
    let tokens : token' Queue.t = Queue.create () in
    let n : int = String.length s in

    let rec loop (ln : int) (i : int) (j : int) : unit =
        if n <= j then
            push ln i j
        else (
            match s.[j] with
                | '\n' ->
                    (
                        push ln i j;
                        newline ln j
                    )
                | '#' -> skip ln j
                | ' ' ->
                    (
                        push ln i j;
                        adv ln j
                    )
                | ',' ->
                    (
                        push ln i j;
                        Queue.add (ln, Comma) tokens;
                        adv ln j
                    )
                | ';' ->
                    (
                        push ln i j;
                        Queue.add (ln, Semicolon) tokens;
                        adv ln j
                    )
                | '(' ->
                    (
                        push ln i j;
                        Queue.add (ln, LParen) tokens;
                        adv ln j
                    )
                | ')' ->
                    (
                        push ln i j;
                        Queue.add (ln, RParen) tokens;
                        adv ln j
                    )
                | '=' ->
                    (
                        push ln i j;
                        Queue.add (ln, Equals) tokens;
                        adv ln j
                    )
                | '+' ->
                    (
                        push ln i j;
                        Queue.add (ln, Plus) tokens;
                        adv ln j
                    )
                | _ -> loop ln i (j + 1)
        )

    and push (ln : int) (i : int) (j : int) : unit =
        if i <> j then
            Queue.add (ln, String.sub s i (j - i) |> get_token) tokens

    and adv (ln : int) (j : int) : unit =
        let i : int = j + 1 in
        loop ln i i

    and skip (ln : int) (j : int) : unit =
        if n <= j then
            ()
        else match s.[j] with
            | '\n' -> newline ln j
            | _ -> skip ln (j + 1)

    and newline (ln : int) (j : int) : unit =
        let i : int = j + 1 in
        loop (ln + 1) i i in

    loop 1 0 0;
    tokens

let rec expr_to_string : expr' -> string = function
    | (ln, EInt x) -> Printf.sprintf "(EInt #%d %d)" ln x
    | (ln, EFloat x) -> Printf.sprintf "(EFloat #%d %.2f)" ln x
    | (ln, EVar x) -> Printf.sprintf "(EVar #%d %s)" ln x
    | (ln, EAssign (l, r)) ->
        Printf.sprintf
            "(EAssign #%d %s = %s)" ln (expr_to_string l) (expr_to_string r)
    | (ln, ECall (x, xs)) ->
        List.map expr_to_string xs |> String.concat ", "
        |> Printf.sprintf "(ECall #%d %s %s)" ln x

let rec parse_expr (tokens : token' Queue.t) : expr' =
    let x : expr' = match Queue.take_opt tokens with
        | Some (ln, TInt x) -> (ln, EInt x)
        | Some (ln, TFloat x) -> (ln, EFloat x)
        | Some (ln, TVar x) -> parse_var tokens ln x
        | Some (_, t) ->
            token_to_string t |> Printf.sprintf "%d %s" __LINE__ |> failwith
        | _ -> failwith (string_of_int __LINE__) in
    match Queue.peek_opt tokens with
        | Some (ln, Equals) ->
            (
                ignore (Queue.take tokens);
                (ln, EAssign (x, parse_expr tokens))
            )
        | _ -> x

and parse_var (tokens : token' Queue.t) (ln : int) (x : string) : expr' =
    match Queue.peek_opt tokens with
        | Some (_, LParen) ->
            (
                ignore (Queue.take tokens);
                let xs : expr' list = parse_paren_args tokens in
                (ln, ECall (x, xs));
            )
        | _ -> (ln, EVar x)

and parse_paren_args (tokens : token' Queue.t) : expr' list =
    let x : expr' = parse_expr tokens in
    match Queue.peek_opt tokens with
        | Some (_, Comma) ->
            (
                ignore (Queue.take tokens);
                x :: (parse_paren_args tokens)
            )
        | Some (_, RParen) ->
            (
                ignore (Queue.take tokens);
                [x]
            )
        | Some (_, t) ->
            token_to_string t |> Printf.sprintf "%d %s" __LINE__ |> failwith
        | _ -> failwith (string_of_int __LINE__)

let rec stmt_to_string : stmt' -> string = function
    | (ln, SDecl (t, s)) ->
        Printf.sprintf "(SDecl #%d %s :: %s)" ln s (type_to_string t)
    | (ln, SEffect e) ->
        Printf.sprintf "(SEffect #%d %s)" ln (expr_to_string e)
    | (ln, SEmpty) -> Printf.sprintf "(SEmpty #%d)" ln

let get_var_string (tokens : token' Queue.t) : string =
    match Queue.take_opt tokens with
        | Some (ln, TVar x) -> x
        | Some (_, t) ->
            token_to_string t |> Printf.sprintf "%d %s" __LINE__ |> failwith
        | _ -> failwith (string_of_int __LINE__)

let eat_semicolon (tokens : token' Queue.t) : unit =
    match Queue.take_opt tokens with
        | Some (_, Semicolon) -> ()
        | Some (_, t) ->
            token_to_string t |> Printf.sprintf "%d %s" __LINE__ |> failwith
        | _ -> failwith (string_of_int __LINE__)

let rec parse_stmt (tokens : token' Queue.t) : stmt' =
    match Queue.peek_opt tokens with
        | Some (ln, Semicolon) ->
            (
                eat_semicolon tokens;
                (ln, SEmpty)
            )
        | Some (ln, TType t) ->
            (
                ignore (Queue.take tokens);
                let s : string = get_var_string tokens in
                eat_semicolon tokens;
                (ln, SDecl (t, s))
            )
        | Some (ln, _) ->
            (
                let x : expr' = parse_expr tokens in
                eat_semicolon tokens;
                (ln, SEffect x)
            )
        | _ -> failwith (string_of_int __LINE__)

let () : unit =
    let tokens : token' Queue.t =
        split_to_tokens
            {_|
# ...
i32 x;
i32 y;
f32 z;
x = 1;
y = f(x, 2, 0.1);
z = 1.0;
# ...
g(x, y, z);
println(z);
            |_} in
    while not (Queue.is_empty tokens) do
        parse_stmt tokens |> stmt_to_string |> print_endline
    done
