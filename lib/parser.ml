(** Parse raw input into a parse tree.scanner
    
We read one token at a time. The implementation uses a callback
which takes the current token and the scanner, or nothing
and the scanner (if at EOF). This is sort of gnarly but it
seems to work. *)

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
type parser = { scanner : Scanner.scanner; env : TEnv.t }

(** Read another token from the input and pass it into the provided callback *)
let next_token _ parser f =
  match Scanner.scan parser.scanner with
  | Error e -> error e
  | Ok t -> (
      match t with
      | None -> f parser.scanner None
      | Some (token, scanner) -> f scanner (Some token))

(** Report that an error occurred at the current token *)
let parser_error maybe_token message =
  match maybe_token with
  | Some token -> error (message ^ " " ^ Scanner.format_token token)
  | None -> error (message ^ " (got EOF)")

(** The main event! Parse some tokens into a parse tree. 
    
The remaining functions herein are uncommented but map 1:1 to the parse nodes
defined above. *)
let rec parse input =
  parse_program
    {
      env = TEnv.empty;
      scanner =
        { input = input |> String.to_seq |> List.of_seq; line = 0; pos = 0 };
    }
    []

and parse_program parser program =
  match Scanner.scan parser.scanner with
  | Error e -> error e
  | Ok t -> (
      match t with
      | None -> ok program
      | Some _ ->
          Result.bind (parse_assignment parser) (fun (parser, assignment) ->
              parse_program parser (program @ [ assignment ])))

and parse_assignment parser =
  next_token "assignment" parser (fun scanner token ->
      match token with
      | Some (Scanner.Identifier id, _, _) ->
          next_token "id" { parser with scanner } (fun scanner token ->
              match token with
              | Some (Scanner.Equals, _, _) ->
                  Result.bind
                    (parse_expr { parser with scanner })
                    (fun (parser, expr) ->
                      next_token "semicolon" parser (fun scanner token ->
                          match token with
                          | Some (Scanner.Semicolon, _, _) ->
                              ok
                                ( { env = TEnv.add id parser.env; scanner },
                                  (id, expr) )
                          | _ -> parser_error token "expected_semicolon"))
              | _ -> parser_error token "expected_equals")
      | _ -> parser_error token "expected_identifier")

and parse_expr parser =
  Result.bind (parse_term parser) (fun (parser, term) ->
      Result.map
        (fun (parser, expr') -> (parser, (term, expr')))
        (parse_expr' parser))

and parse_expr' parser =
  next_token "expr'" parser (fun scanner token ->
      match token with
      | Some (Scanner.Plus, _, _) ->
          Result.bind
            (parse_term { parser with scanner })
            (fun (parser, term) ->
              Result.map
                (fun (parser, expr') -> (parser, Plus (term, expr')))
                (parse_expr' parser))
      | Some (Scanner.Minus, _, _) ->
          Result.bind (parse_term parser) (fun (parser, term) ->
              Result.map
                (fun (parser, expr') -> (parser, Minus (term, expr')))
                (parse_expr' parser))
      | _ -> ok (parser, EmptyExpr))

and parse_term parser =
  Result.bind (parse_fact parser) (fun (parser, fact) ->
      Result.map
        (fun (parser, term') -> (parser, (fact, term')))
        (parse_term' parser))

and parse_term' parser =
  next_token "term'" parser (fun scanner token ->
      match token with
      | Some (Scanner.Mult, _, _) ->
          Result.bind
            (parse_fact { parser with scanner })
            (fun (parser, fact) ->
              Result.map
                (fun (parser, term') -> (parser, Mult (fact, term')))
                (parse_term' parser))
      | _ -> ok (parser, EmptyTerm))

and parse_fact parser =
  next_token "fact" parser (fun scanner token ->
      match token with
      | Some (Scanner.Minus, _, _) ->
          Result.map
            (fun (parser, fact) ->
              (parser, Expr ((fact, Mult (Integer (-1), EmptyTerm)), EmptyExpr)))
            (parse_fact { parser with scanner })
      | Some (Scanner.Plus, _, _) -> parse_fact { parser with scanner }
      | Some (Scanner.Identifier id, _, _) ->
          if TEnv.mem id parser.env then
            ok ({ parser with scanner }, Identifier id)
          else parser_error token ("uninitialized_variable: " ^ id)
      | Some (Scanner.Integer n, _, _) -> ok ({ parser with scanner }, Integer n)
      | Some (Scanner.OpenParens, _, _) ->
          Result.bind
            (parse_expr { parser with scanner })
            (fun (parser, expr) ->
              next_token "closeparens" parser (fun scanner token ->
                  match token with
                  | Some (Scanner.CloseParens, _, _) ->
                      ok ({ parser with scanner }, Expr expr)
                  | _ -> parser_error token "expected_close_parens"))
      | _ -> parser_error token "invalid_token")
