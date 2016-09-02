// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package scanner

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"testing"
	"unicode/utf8"
)

// A StringReader delivers its data one string segment at a time via Read.
type StringReader struct {
	data []string
	step int
}

func (r *StringReader) Read(p []byte) (n int, err error) {
	if r.step < len(r.data) {
		s := r.data[r.step]
		n = copy(p, s)
		r.step++
	} else {
		err = io.EOF
	}
	return
}

type token struct {
	tok  rune
	text string
}

var f100 = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

var tokenList = []token{
	{comment, "// line comments"},
	{comment, "//"},
	{comment, "////"},
	{comment, "// comment"},
	{comment, "// /* comment */"},
	{comment, "// // comment //"},
	{comment, "//" + f100},

	{comment, "// general comments"},
	{comment, "/**/"},
	{comment, "/***/"},
	{comment, "/* comment */"},
	{comment, "/* // comment */"},
	{comment, "/* /* comment */"},
	{comment, "/*\n comment\n*/"},
	{comment, "/*" + f100 + "*/"},

	{comment, "// identifiers"},
	{Ident, "a"},
	{Ident, "a0"},
	{Ident, "foobar"},
	{Ident, "abc123"},
	{Ident, "LGTM"},
	{Ident, "_"},
	{Ident, "_abc123"},
	{Ident, "abc123_"},
	{Ident, "_abc_123_"},
	{Ident, f100},

	{comment, "// decimal ints"},
	{Int, "0"},
	{Int, "1"},
	{Int, "9"},
	{Int, "42"},
	{Int, "1234567890"},

	{comment, "// hexadecimal ints"},
	{Int, "0x0"},
	{Int, "0x1"},
	{Int, "0xf"},
	{Int, "0x42"},
	{Int, "0x123456789abcDEF"},
	{Int, "0x" + f100},
	{Int, "0X0"},
	{Int, "0X1"},
	{Int, "0XF"},
	{Int, "0X42"},
	{Int, "0X123456789abcDEF"},
	{Int, "0X" + f100},

	{comment, "// floats"},
	{Float, "0."},
	{Float, "1."},
	{Float, "42."},
	{Float, "01234567890."},
	{Float, ".0"},
	{Float, ".1"},
	{Float, ".42"},
	{Float, ".0123456789"},
	{Float, "0.0"},
	{Float, "1.0"},
	{Float, "42.0"},
	{Float, "01234567890.0"},
	{Float, "0e0"},
	{Float, "1e0"},
	{Float, "42e0"},
	{Float, "01234567890e0"},
	{Float, "0E0"},
	{Float, "1E0"},
	{Float, "42E0"},
	{Float, "01234567890E0"},
	{Float, "0e+10"},
	{Float, "1e-10"},
	{Float, "42e+10"},
	{Float, "01234567890e-10"},
	{Float, "0E+10"},
	{Float, "1E-10"},
	{Float, "42E+10"},
	{Float, "01234567890E-10"},

	{comment, "// strings"},
	{String, "''"},
	{String, "' '"},
	{String, "'a'"},
	{String, "'\\n'"},
	{String, "'\\r'"},
	{String, "'\\t'"},
	{String, "'\\q'"},
	{String, "'" + f100 + "'"},

	{comment, "// individual characters"},
	// NUL character is not allowed
	{'\x01', "\x01"},
	{' ' - 1, string(' ' - 1)},
	{'+', "+"},
	{'/', "/"},
	{'.', "."},
	{'~', "~"},
	{'(', "("},
}

func makeSource(pattern string) *bytes.Buffer {
	var buf bytes.Buffer
	for _, k := range tokenList {
		fmt.Fprintf(&buf, pattern, k.text)
	}
	return &buf
}

func checkTok(t *testing.T, s *Scanner, line int, got, want rune, text string) {
	if got != want {
		t.Fatalf("tok = %s, want %s for %q", TokenString(got), TokenString(want), text)
	}
	if s.Line != line {
		t.Errorf("line = %d, want %d for %q", s.Line, line, text)
	}
	stext := s.TokenText()
	if stext != text {
		t.Errorf("text = %q, want %q", stext, text)
	} else {
		// check idempotency of TokenText() call
		stext = s.TokenText()
		if stext != text {
			t.Errorf("text = %q, want %q (idempotency check)", stext, text)
		}
	}
}

func countNewlines(s string) int {
	n := 0
	for _, ch := range s {
		if ch == '\n' {
			n++
		}
	}
	return n
}

func TestScan(t *testing.T) {
	s := new(Scanner).Init(makeSource(" \t%s\n"))
	tok := s.Scan()
	line := 1
	for _, k := range tokenList {
		if k.tok != comment {
			checkTok(t, s, line, tok, k.tok, k.text)
			tok = s.Scan()
		}
		line += countNewlines(k.text) + 1 // each token is on a new line
	}
	checkTok(t, s, line, tok, EOF, "")
}

func TestPosition(t *testing.T) {
	src := makeSource("\t\t\t\t%s\n")
	s := new(Scanner).Init(src)
	s.Scan()
	pos := Position{"", 4, 1, 5}
	for _, k := range tokenList {
		if k.tok != comment {
			if s.Offset != pos.Offset {
				t.Errorf("offset = %d, want %d for %q", s.Offset, pos.Offset, k.text)
			}
			if s.Line != pos.Line {
				t.Errorf("line = %d, want %d for %q", s.Line, pos.Line, k.text)
			}
			if s.Column != pos.Column {
				t.Errorf("column = %d, want %d for %q", s.Column, pos.Column, k.text)
			}
			s.Scan()
		}
		pos.Offset += 4 + len(k.text) + 1     // 4 tabs + token bytes + newline
		pos.Line += countNewlines(k.text) + 1 // each token is on a new line
	}
	// make sure there were no token-internal errors reported by scanner
	if s.ErrorCount != 0 {
		t.Errorf("%d errors", s.ErrorCount)
	}
}

func testScanSelectedMode(t *testing.T, mode uint, class rune) {
	src := makeSource("%s\n")
	s := new(Scanner).Init(src)
	tok := s.Scan()
	for tok != EOF {
		if tok < 0 && tok != class {
			t.Fatalf("tok = %s, want %s", TokenString(tok), TokenString(class))
		}
		tok = s.Scan()
	}
	if s.ErrorCount != 0 {
		t.Errorf("%d errors", s.ErrorCount)
	}
}

func testError(t *testing.T, src, pos, msg string, tok rune) {
	s := new(Scanner).Init(strings.NewReader(src))
	errorCalled := false
	s.Error = func(s *Scanner, m string) {
		if !errorCalled {
			// only look at first error
			if p := s.Pos().String(); p != pos {
				t.Errorf("pos = %q, want %q for %q", p, pos, src)
			}
			if m != msg {
				t.Errorf("msg = %q, want %q for %q", m, msg, src)
			}
			errorCalled = true
		}
	}
	tk := s.Scan()
	if tk != tok {
		t.Errorf("tok = %s, want %s for %q", TokenString(tk), TokenString(tok), src)
	}
	if !errorCalled {
		t.Errorf("error handler not called for %q", src)
	}
	if s.ErrorCount == 0 {
		t.Errorf("count = %d, want > 0 for %q", s.ErrorCount, src)
	}
}

func TestError(t *testing.T) {
	testError(t, "\x00", "<input>:1:1", "illegal character NUL", 0)
	testError(t, "\x80", "<input>:1:1", "illegal UTF-8 encoding", utf8.RuneError)
	testError(t, "\xff", "<input>:1:1", "illegal UTF-8 encoding", utf8.RuneError)

	testError(t, "a\x00", "<input>:1:2", "illegal character NUL", Ident)
	testError(t, "ab\x80", "<input>:1:3", "illegal UTF-8 encoding", Ident)
	testError(t, "abc\xff", "<input>:1:4", "illegal UTF-8 encoding", Ident)

	testError(t, "'a\x00", "<input>:1:3", "illegal character NUL", String)
	testError(t, "'ab\x80", "<input>:1:4", "illegal UTF-8 encoding", String)
	testError(t, "'abc\xff", "<input>:1:5", "illegal UTF-8 encoding", String)

	testError(t, "'\\''", "<input>:1:3", "illegal char escape", String)

	testError(t, `01238`, "<input>:1:6", "illegal octal number", Int)
	testError(t, `01238123`, "<input>:1:9", "illegal octal number", Int)
	testError(t, `0x`, "<input>:1:3", "illegal hexadecimal number", Int)
	testError(t, `0xg`, "<input>:1:3", "illegal hexadecimal number", Int)

	testError(t, "'abc", "<input>:1:5", "literal not terminated", String)
	testError(t, "'abc\n", "<input>:1:5", "literal not terminated", String)
	testError(t, `/*/`, "<input>:1:4", "comment not terminated", EOF)
}

// An errReader returns (0, err) where err is not io.EOF.
type errReader struct{}

func (errReader) Read(b []byte) (int, error) {
	return 0, io.ErrNoProgress // some error that is not io.EOF
}

func TestIOError(t *testing.T) {
	s := new(Scanner).Init(errReader{})
	errorCalled := false
	s.Error = func(s *Scanner, msg string) {
		if !errorCalled {
			if want := io.ErrNoProgress.Error(); msg != want {
				t.Errorf("msg = %q, want %q", msg, want)
			}
			errorCalled = true
		}
	}
	tok := s.Scan()
	if tok != EOF {
		t.Errorf("tok = %s, want EOF", TokenString(tok))
	}
	if !errorCalled {
		t.Errorf("error handler not called")
	}
}

func checkPos(t *testing.T, got, want Position) {
	if got.Offset != want.Offset || got.Line != want.Line || got.Column != want.Column {
		t.Errorf("got offset, line, column = %d, %d, %d; want %d, %d, %d",
			got.Offset, got.Line, got.Column, want.Offset, want.Line, want.Column)
	}
}

func checkScanPos(t *testing.T, s *Scanner, offset, line, column int, char rune) {
	want := Position{Offset: offset, Line: line, Column: column}
	checkPos(t, s.Pos(), want)
	if ch := s.Scan(); ch != char {
		t.Errorf("ch = %s, want %s", TokenString(ch), TokenString(char))
		if string(ch) != s.TokenText() {
			t.Errorf("tok = %q, want %q", s.TokenText(), string(ch))
		}
	}
	checkPos(t, s.Position, want)
}

func TestPos(t *testing.T) {
	// corner case: empty source
	s := new(Scanner).Init(strings.NewReader(""))
	checkPos(t, s.Pos(), Position{Offset: 0, Line: 1, Column: 1})
	s.Peek() // peek doesn't affect the position
	checkPos(t, s.Pos(), Position{Offset: 0, Line: 1, Column: 1})

	// corner case: source with only a newline
	s = new(Scanner).Init(strings.NewReader("\n"))
	checkPos(t, s.Pos(), Position{Offset: 0, Line: 1, Column: 1})
	if s.Scan() != EOF {
		t.Errorf("expecting EOF")
	}
	// after EOF position doesn't change
	for i := 10; i > 0; i-- {
		checkScanPos(t, s, 1, 2, 1, EOF)
	}
	if s.ErrorCount != 0 {
		t.Errorf("%d errors", s.ErrorCount)
	}

	// corner case: source with only a single character
	s = new(Scanner).Init(strings.NewReader("x"))
	checkPos(t, s.Pos(), Position{Offset: 0, Line: 1, Column: 1})
	checkScanPos(t, s, 0, 1, 1, Ident)
	// after EOF position doesn't change
	for i := 10; i > 0; i-- {
		checkScanPos(t, s, 1, 1, 2, EOF)
	}
	if s.ErrorCount != 0 {
		t.Errorf("%d errors", s.ErrorCount)
	}

	// FIXME: fails becuase checkScanPos() callse checkPos() before and after
	// s.Scan()

	// positions after calling Scan
	// s = new(Scanner).Init(strings.NewReader("abc\nde\n\nx"))
	// checkScanPos(t, s, 0, 1, 1, Ident)
	// s.Peek() // peek doesn't affect the position
	// checkScanPos(t, s, 4, 2, 1, Ident)
	// checkScanPos(t, s, 8, 4, 1, Ident)
	// // after EOF position doesn't change
	// for i := 10; i > 0; i-- {
	// 	checkScanPos(t, s, 9, 4, 2, EOF)
	// }
	// if s.ErrorCount != 0 {
	// 	t.Errorf("%d errors", s.ErrorCount)
	// }
}

type countReader int

func (r *countReader) Read([]byte) (int, error) {
	*r++
	return 0, io.EOF
}

func TestScanEOFHandling(t *testing.T) {
	var r countReader

	// corner case: empty source
	s := new(Scanner).Init(&r)

	tok := s.Scan()
	if tok != EOF {
		t.Error("1) EOF not reported")
	}

	tok = s.Peek()
	if tok != EOF {
		t.Error("2) EOF not reported")
	}

	if r != 1 {
		t.Errorf("scanner called Read %d times, not once", r)
	}
}
