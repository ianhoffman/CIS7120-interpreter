(** Scan raw input and return a token *)

open Result

(** Types of tokens which exist in the language. *)
type token_type =
  | Integer of int
  | Identifier of string
  | Plus
  | Minus
  | Mult
  | Equals
  | OpenParens
  | CloseParens
  | Semicolon

(* A single scanned token, consisting of a type, line, and position. *)
type token = token_type * int * int

(** The scanner's state, consisting of the unprocessed input, the current position and the current line. *)
type scanner = { input : char list; pos : int; line : int }

let format_token_type t =
  match t with
  | Integer n -> "Integer(" ^ string_of_int n ^ ")"
  | Identifier s -> "Identifier(" ^ s ^ ")"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mult -> "Mult"
  | Equals -> "Equals"
  | OpenParens -> "OpenParens"
  | CloseParens -> "CloseParens"
  | Semicolon -> "Semicolon"

let format_token (ttype, line, pos) =
  "<type=" ^ format_token_type ttype ^ ", line=" ^ string_of_int line ^ ", pos="
  ^ string_of_int pos ^ ">"

let string_to_list s = s |> String.to_seq |> List.of_seq

let rec skip_whitespace scanner =
  match scanner.input with
  | ' ' :: input | '\t' :: input ->
      skip_whitespace { pos = scanner.pos + 1; line = scanner.line; input }
  | '\n' :: input -> skip_whitespace { pos = 0; line = scanner.line + 1; input }
  | _ -> scanner

let char_to_int c = Char.code c - Char.code '0'

let rec scan_nonzero_integer scanner curr =
  match scanner.input with
  | h :: input -> (
      match h with
      | '0' .. '9' ->
          scan_nonzero_integer
            { input; pos = scanner.pos + 1; line = scanner.line }
            ((curr * 10) + char_to_int h)
      | _ -> (scanner, curr))
  | _ -> (scanner, curr)

let scan_int scanner =
  match scanner.input with
  | '0' :: input -> ({ input; pos = scanner.pos + 1; line = scanner.line }, 0)
  | _ -> scan_nonzero_integer scanner 0

let rec scan_id scanner curr =
  match scanner.input with
  | h :: input -> (
      match h with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' ->
          scan_id
            { input; pos = scanner.pos + 1; line = scanner.line }
            (curr ^ String.make 1 h)
      | _ -> (scanner, curr))
  | _ -> (scanner, curr)

let scan scanner =
  let scanner = skip_whitespace scanner in
  match scanner.input with
  | [] -> ok None
  | h :: t -> (
      let make_token tok rest =
        ok (Some ((tok, scanner.line, scanner.pos), rest))
      in
      let make_token_simple tok =
        make_token tok { input = t; pos = scanner.pos + 1; line = scanner.line }
      in
      match h with
      | '(' -> make_token_simple OpenParens
      | ')' -> make_token_simple CloseParens
      | '+' -> make_token_simple Plus
      | '-' -> make_token_simple Minus
      | '*' -> make_token_simple Mult
      | '=' -> make_token_simple Equals
      | ';' -> make_token_simple Semicolon
      | '0' .. '9' ->
          let r, n = scan_int scanner in
          make_token (Integer n) r
      | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
          let r, id = scan_id scanner "" in
          make_token (Identifier id) r
      | _ ->
          error
            ("invalid_character (line=" ^ string_of_int scanner.line ^ ", pos="
           ^ string_of_int scanner.pos ^ ")"))
(*
   let scan inp = scan_inner [] { input = string_to_list inp; pos = 0; line = 0 } *)
