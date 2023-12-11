open Result

(** Type definitions for parse tree nodes

These mirror exactly the productions of the context-free grammar.  *)

(** A program is a list of assignments *)
type program = assignment list

(** An assignment is an identifier and an expression *)
and assignment = string * expr

(** An expr is a term and an expr', to avoid left recursion *)
and expr = term * expr'

(** An expr' is a binary operator, a term, and another expr', or simply an empty string *)
and expr' = Plus of term * expr' | Minus of term * expr' | EmptyExpr

(** A term is a fact and a term', to avoid left recursion *)
and term = fact * term'

(** A term' is a binary operator, a fact, and another term', or simply an empty string *)
and term' = Mult of fact * term' | EmptyTerm

(** A fact is an identifier, an integer, or a parenthetical expression *)
and fact = Identifier of string | Integer of int | Expr of expr

(** UTILITIES FOR FORMATTING PARSER NODES *)

let rec format_program p = p |> List.map format_assignment |> String.concat "\n"
and format_assignment (id, e) = id ^ " = " ^ format_expr e ^ ";"
and format_expr (t, e) = format_term t ^ format_expr' e

and format_expr' e =
  match e with
  | Plus (t, e') -> " + " ^ format_term t ^ format_expr' e'
  | Minus (t, e') -> " - " ^ format_term t ^ format_expr' e'
  | EmptyExpr -> ""

and format_term (f, t) = format_fact f ^ format_term' t

and format_term' t =
  match t with
  | Mult (f, t) -> " * " ^ format_fact f ^ format_term' t
  | EmptyTerm -> ""

and format_fact f =
  match f with
  | Identifier id -> id
  | Integer n -> string_of_int n
  | Expr e -> "(" ^ format_expr e ^ ")"

(** A set of strings. Used to track identifiers we've encountered so far. *)
module TEnv = Set.Make (String)

(** The actual parser state. The currently unparsed tokens and all assignments. *)
type parser = { tokens : Scanner.token list; env : TEnv.t }

(** Report that an error occurred at the current token *)
let parser_error parser message =
  error
    (match parser.tokens with
    | token :: _ -> message ^ " " ^ Scanner.format_token token
    | _ -> message)

(** The main event! Parse some tokens into a parse tree. 
    
The remaining functions herein are uncommented but map 1:1 to the parse nodes
defined above. *)
let rec parse tokens = parse_program { env = TEnv.empty; tokens } []

and parse_program parser program =
  match parser.tokens with
  | [] -> Ok program
  | _ ->
      Result.bind (parse_assignment parser) (fun (parser, assignment) ->
          parse_program parser (program @ [ assignment ]))

and parse_assignment parser =
  match parser.tokens with
  | (Scanner.Identifier id, _, _) :: tokens -> (
      match tokens with
      | (Scanner.Equals, _, _) :: tokens ->
          Result.bind
            (parse_expr { parser with tokens })
            (fun (parser, expr) ->
              match parser.tokens with
              | (Scanner.Semicolon, _, _) :: tokens ->
                  ok ({ env = TEnv.add id parser.env; tokens }, (id, expr))
              | _ -> parser_error parser "expected_semicolon")
      | _ -> parser_error { parser with tokens } "expected_equals")
  | _ -> parser_error parser "expected_identifier"

and parse_expr parser =
  Result.bind (parse_term parser) (fun (parser, term) ->
      Result.map
        (fun (parser, expr') -> (parser, (term, expr')))
        (parse_expr' parser))

and parse_expr' parser =
  match parser.tokens with
  | (Scanner.Plus, _, _) :: tokens ->
      Result.bind
        (parse_term { parser with tokens })
        (fun (parser, term) ->
          Result.map
            (fun (parser, expr') -> (parser, Plus (term, expr')))
            (parse_expr' parser))
  | (Scanner.Minus, _, _) :: tokens ->
      Result.bind
        (parse_term { parser with tokens })
        (fun (parser, term) ->
          Result.map
            (fun (parser, expr') -> (parser, Minus (term, expr')))
            (parse_expr' parser))
  | _ -> ok (parser, EmptyExpr)

and parse_term parser =
  Result.bind (parse_fact parser) (fun (parser, fact) ->
      Result.map
        (fun (parser, term') -> (parser, (fact, term')))
        (parse_term' parser))

and parse_term' parser =
  match parser.tokens with
  | (Scanner.Mult, _, _) :: tokens ->
      Result.bind
        (parse_fact { parser with tokens })
        (fun (parser, fact) ->
          Result.map
            (fun (parser, term') -> (parser, Mult (fact, term')))
            (parse_term' parser))
  | _ -> ok (parser, EmptyTerm)

and parse_fact parser =
  match parser.tokens with
  | (Scanner.Minus, _, _) :: tokens ->
      Result.map
        (fun (parser, fact) ->
          (parser, Expr ((fact, Mult (Integer (-1), EmptyTerm)), EmptyExpr)))
        (parse_fact { parser with tokens })
  | (Scanner.Plus, _, _) :: tokens -> parse_fact { parser with tokens }
  | (Scanner.Identifier id, _, _) :: tokens ->
      if TEnv.mem id parser.env then ok ({ parser with tokens }, Identifier id)
      else parser_error parser ("uninitialized_variable: " ^ id)
  | (Scanner.Integer n, _, _) :: tokens -> ok ({ parser with tokens }, Integer n)
  | (Scanner.OpenParens, _, _) :: tokens ->
      Result.bind
        (parse_expr { env = parser.env; tokens })
        (fun (parser, expr) ->
          match parser.tokens with
          | (Scanner.CloseParens, _, _) :: tokens ->
              ok ({ parser with tokens }, Expr expr)
          | _ -> parser_error parser "expected_close_parens")
  | tok :: _ ->
      parser_error parser ("invalid_token: " ^ Scanner.format_token tok)
  | [] -> error "unexpected_eof"
