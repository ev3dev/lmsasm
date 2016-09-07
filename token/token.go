// Copyright 2009 The Go Authors. All rights reserved.
//           2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package token defines constants representing the lexical tokens of the Go
// programming language and basic operations on tokens (printing, predicates).
//
package token

import "strconv"

// Token is the set of lexical tokens of the lmsasm programming language.
type Token int

// The list of tokens.
const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	COMMENT

	literal_begin
	// Identifiers and basic type literals
	// (these tokens stand for classes of literals)
	IDENT  // main
	INT    // 12345
	FLOAT  // 123.45F
	STRING // 'abc'
	literal_end

	operator_begin
	// Operators and delimiters
	ADD // +
	SUB // -
	MUL // *
	QUO // /

	LPAREN // (
	LBRACE // {
	COMMA  // ,

	RPAREN    // )
	RBRACE    // }
	COLON     // :
	SEMICOLON // ;
	operator_end

	keyword_begin
	// Keywords
	DEFINE
	VMTHREAD
	SUBCALL
	keyword_end

	type_begin
	DATATYPE
	PARAMTYPE
	type_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",

	EOF:     "EOF",
	COMMENT: "COMMENT",

	IDENT:  "IDENT",
	INT:    "INT",
	FLOAT:  "FLOAT",
	STRING: "STRING",

	ADD: "+",
	SUB: "-",
	MUL: "*",
	QUO: "/",

	LPAREN: "(",
	LBRACE: "{",
	COMMA:  ",",

	RPAREN:    ")",
	RBRACE:    "}",
	COLON:     ":",
	SEMICOLON: ";",

	DEFINE:    "define",
	VMTHREAD:  "vmthread",
	SUBCALL:   "subcall",

	DATATYPE:  "DATATYPE",
	PARAMTYPE: "PARAMTYPE",
}

// String returns the string corresponding to the token tok.
// For operators, delimiters, and keywords the string is the actual
// token character sequence (e.g., for the token ADD, the string is
// "+"). For all other tokens the string corresponds to the token
// constant name (e.g. for the token IDENT, the string is "IDENT").
//
func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tok)) + ")"
	}
	return s
}

// A set of constants for precedence-based expression parsing.
// Non-operators have lowest precedence, followed by operators
// starting with precedence 1 up to unary operators. The highest
// precedence serves as "catch-all" precedence for selector,
// indexing, and other operator and delimiter tokens.
//
const (
	LowestPrec  = 0 // non-operators
	UnaryPrec   = 6
	HighestPrec = 7
)

// Precedence returns the operator precedence of the binary
// operator op. If op is not a binary operator, the result
// is LowestPrecedence.
//
func (op Token) Precedence() int {
	switch op {
	case ADD, SUB:
		return 4
	case MUL, QUO:
		return 5
	}
	return LowestPrec
}

var keywords map[string]Token

func init() {
	keywords = make(map[string]Token)
	for i := keyword_begin + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

// Lookup maps an identifier to its keyword token, datatype token or IDENT
// (if not a keyword or data type).
//
func Lookup(ident string) Token {
	if tok, is_keyword := keywords[ident]; is_keyword {
		return tok
	}
	switch ident {
	case "DATA8", "DATA16", "DATA32", "DATAF", "DATAS",
			"ARRAY8", "ARRAY16", "ARRAY32", "ARRAYF",
			"HANDLE":
		return DATATYPE
	case "IN_8", "IN_16", "IN_32", "IN_F", "IN_S",
			"OUT_8", "OUT_16", "OUT_32", "OUT_F", "OUT_S",
			"IO_8", "IO_16", "IO_32", "IO_F", "IO_S":
		return PARAMTYPE
	default:
		return IDENT
	}
}

// Predicates

// IsLiteral returns true for tokens corresponding to identifiers
// and basic type literals; it returns false otherwise.
//
func (tok Token) IsLiteral() bool { return literal_begin < tok && tok < literal_end }

// IsOperator returns true for tokens corresponding to operators and
// delimiters; it returns false otherwise.
//
func (tok Token) IsOperator() bool { return operator_begin < tok && tok < operator_end }

// IsKeyword returns true for tokens corresponding to keywords;
// it returns false otherwise.
//
func (tok Token) IsKeyword() bool { return keyword_begin < tok && tok < keyword_end }
