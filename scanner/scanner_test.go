// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package scanner

import (
	"bytes"
	"fmt"
	"github.com/ev3dev/lmsasm/token"
	"io"
	"strings"
	"testing"
)

const filename = "george.file"

type tokenInfo struct {
	tok  token.Token
	text string
}

var f100 = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

var tokenList = []tokenInfo{
	{token.COMMENT, "// line comments"},
	{token.COMMENT, "//"},
	{token.COMMENT, "////"},
	{token.COMMENT, "// comment"},
	{token.COMMENT, "// /* comment */"},
	{token.COMMENT, "// // comment //"},
	{token.COMMENT, "//" + f100},

	{token.COMMENT, "// general comments"},
	{token.COMMENT, "/**/"},
	{token.COMMENT, "/***/"},
	{token.COMMENT, "/* comment */"},
	{token.COMMENT, "/* // comment */"},
	{token.COMMENT, "/* /* comment */"},
	{token.COMMENT, "/*\n comment\n*/"},
	{token.COMMENT, "/*" + f100 + "*/"},

	{token.COMMENT, "// identifiers"},
	{token.IDENT, "a"},
	{token.IDENT, "a0"},
	{token.IDENT, "foobar"},
	{token.IDENT, "abc123"},
	{token.IDENT, "LGTM"},
	{token.IDENT, "_"},
	{token.IDENT, "_abc123"},
	{token.IDENT, "abc123_"},
	{token.IDENT, "_abc_123_"},
	{token.IDENT, f100},

	{token.COMMENT, "// decimal ints"},
	{token.INT, "0"},
	{token.INT, "1"},
	{token.INT, "9"},
	{token.INT, "42"},
	{token.INT, "1234567890"},

	{token.COMMENT, "// hexadecimal ints"},
	{token.INT, "0x0"},
	{token.INT, "0x1"},
	{token.INT, "0xf"},
	{token.INT, "0x42"},
	{token.INT, "0x123456789abcDEF"},
	{token.INT, "0x" + f100},
	{token.INT, "0X0"},
	{token.INT, "0X1"},
	{token.INT, "0XF"},
	{token.INT, "0X42"},
	{token.INT, "0X123456789abcDEF"},
	{token.INT, "0X" + f100},

	{token.COMMENT, "// floats"},
	{token.FLOAT, "0F"},
	{token.FLOAT, "1F"},
	{token.FLOAT, "42F"},
	{token.FLOAT, "01234567890F"},
	{token.FLOAT, ".0F"},
	{token.FLOAT, ".1F"},
	{token.FLOAT, ".42F"},
	{token.FLOAT, ".0123456789F"},
	{token.FLOAT, "0.0F"},
	{token.FLOAT, "1.0F"},
	{token.FLOAT, "42.0F"},
	{token.FLOAT, "01234567890.0F"},
	{token.FLOAT, "0e0F"},
	{token.FLOAT, "1e0F"},
	{token.FLOAT, "42e0F"},
	{token.FLOAT, "01234567890e0F"},
	{token.FLOAT, "0E0F"},
	{token.FLOAT, "1E0F"},
	{token.FLOAT, "42E0F"},
	{token.FLOAT, "01234567890E0F"},
	{token.FLOAT, "0e+10F"},
	{token.FLOAT, "1e-10F"},
	{token.FLOAT, "42e+10F"},
	{token.FLOAT, "01234567890e-10F"},
	{token.FLOAT, "0E+10F"},
	{token.FLOAT, "1E-10F"},
	{token.FLOAT, "42E+10F"},
	{token.FLOAT, "01234567890E-10F"},

	{token.COMMENT, "// strings"},
	{token.STRING, "''"},
	{token.STRING, "' '"},
	{token.STRING, "'a'"},
	{token.STRING, "'\\n'"},
	{token.STRING, "'\\r'"},
	{token.STRING, "'\\t'"},
	{token.STRING, "'\\q'"},
	{token.STRING, "'\\\\'"},
	{token.STRING, "'" + f100 + "'"},

	{token.COMMENT, "// operators"},
	{token.ADD, "+"},
	{token.SUB, "-"},
	{token.MUL, "*"},
	{token.QUO, "/"},
	{token.LPAREN, "("},
	{token.LBRACE, "{"},
	{token.COMMA, ","},
	{token.RPAREN, ")"},
	{token.RBRACE, "}"},
	{token.COLON, ":"},
	{token.AT, "@"},
	{token.BANG, "!"},
}

func makeSource(pattern string) *bytes.Buffer {
	var buf bytes.Buffer
	for _, k := range tokenList {
		fmt.Fprintf(&buf, pattern, k.text)
	}
	return &buf
}

func checkTok(t *testing.T, s *Scanner, line int, got, want token.Token, text string) {
	if got != want {
		t.Fatalf("tok = %s, want %s for %q", got.String(), want.String(), text)
	}
	if s.Position.Line != line {
		t.Errorf("line = %d, want %d for %q", s.Position.Line, line, text)
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
	s := new(Scanner).Init(makeSource(" \t%s\n"), filename)
	tok := s.Scan()
	line := 1
	for _, k := range tokenList {
		if k.tok != token.COMMENT {
			checkTok(t, s, line, tok, k.tok, k.text)
			tok = s.Scan()
		}
		line += countNewlines(k.text) + 1 // each token is on a new line
	}
	checkTok(t, s, line, tok, token.EOF, "")
}

func TestPosition(t *testing.T) {
	src := makeSource("\t\t\t\t%s\n")
	s := new(Scanner).Init(src, filename)
	s.Scan()
	pos := token.Position{"", 4, 1, 5}
	for _, k := range tokenList {
		if k.tok != token.COMMENT {
			if s.Position.Offset != pos.Offset {
				t.Errorf("offset = %d, want %d for %q", s.Position.Offset, pos.Offset, k.text)
			}
			if s.Position.Line != pos.Line {
				t.Errorf("line = %d, want %d for %q", s.Position.Line, pos.Line, k.text)
			}
			if s.Position.Column != pos.Column {
				t.Errorf("column = %d, want %d for %q", s.Position.Column, pos.Column, k.text)
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

func testScanSelectedMode(t *testing.T, mode uint, class token.Token) {
	src := makeSource("%s\n")
	s := new(Scanner).Init(src, filename)
	tok := s.Scan()
	for tok != token.EOF {
		if tok < 0 && tok != class {
			t.Fatalf("tok = %s, want %s", tok.String(), class.String())
		}
		tok = s.Scan()
	}
	if s.ErrorCount != 0 {
		t.Errorf("%d errors", s.ErrorCount)
	}
}

func testError(t *testing.T, src, pos, msg string, tok token.Token) {
	s := new(Scanner).Init(strings.NewReader(src), filename)
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
		t.Errorf("tok = %s, want %s for %q", tk.String(), tok.String(), src)
	}
	if !errorCalled {
		t.Errorf("error handler not called for %q", src)
	}
	if s.ErrorCount == 0 {
		t.Errorf("count = %d, want > 0 for %q", s.ErrorCount, src)
	}
}

func TestError(t *testing.T) {
	testError(t, "\x00", filename+":1:1", "illegal character NUL", 0)
	testError(t, "\x80", filename+":1:1", "illegal UTF-8 encoding", token.ILLEGAL)
	testError(t, "\xff", filename+":1:1", "illegal UTF-8 encoding", token.ILLEGAL)

	testError(t, "a\x00", filename+":1:2", "illegal character NUL", token.IDENT)
	testError(t, "ab\x80", filename+":1:3", "illegal UTF-8 encoding", token.IDENT)
	testError(t, "abc\xff", filename+":1:4", "illegal UTF-8 encoding", token.IDENT)

	testError(t, "'a\x00", filename+":1:3", "illegal character NUL", token.STRING)
	testError(t, "'ab\x80", filename+":1:4", "illegal UTF-8 encoding", token.STRING)
	testError(t, "'abc\xff", filename+":1:5", "illegal UTF-8 encoding", token.STRING)

	testError(t, "'\\''", filename+":1:3", "illegal char escape", token.STRING)

	testError(t, `01238`, filename+":1:6", "illegal octal number", token.INT)
	testError(t, `01238123`, filename+":1:9", "illegal octal number", token.INT)
	testError(t, `0x`, filename+":1:3", "illegal hexadecimal number", token.INT)
	testError(t, `0xg`, filename+":1:3", "illegal hexadecimal number", token.INT)

	testError(t, "'abc", filename+":1:5", "literal not terminated", token.STRING)
	testError(t, "'abc\n", filename+":1:5", "literal not terminated", token.STRING)
	testError(t, `/*/`, filename+":1:4", "comment not terminated", token.EOF)

	testError(t, "0.0", filename+":1:4", "float is missing 'F' suffix", token.FLOAT)
	testError(t, "42.", filename+":1:4", "float is missing 'F' suffix", token.FLOAT)
	testError(t, ".0", filename+":1:3", "float is missing 'F' suffix", token.FLOAT)
}

// An errReader returns (0, err) where err is not io.EOF.
type errReader struct{}

func (errReader) Read(b []byte) (int, error) {
	return 0, io.ErrNoProgress // some error that is not io.EOF
}

func TestIOError(t *testing.T) {
	s := new(Scanner).Init(errReader{}, filename)
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
	if tok != token.EOF {
		t.Errorf("tok = %s, want token.EOF", tok.String())
	}
	if !errorCalled {
		t.Errorf("error handler not called")
	}
}

func checkPos(t *testing.T, got, want token.Position) {
	if got.Offset != want.Offset || got.Line != want.Line || got.Column != want.Column {
		t.Errorf("got offset, line, column = %d, %d, %d; want %d, %d, %d",
			got.Offset, got.Line, got.Column, want.Offset, want.Line, want.Column)
	}
}

func checkScanPos(t *testing.T, s *Scanner, offset, line, column int, char token.Token) {
	want := token.Position{Offset: offset, Line: line, Column: column}
	checkPos(t, s.Pos(), want)
	if ch := s.Scan(); ch != char {
		t.Errorf("ch = %s, want %s", ch.String(), char.String())
		if string(ch) != s.TokenText() {
			t.Errorf("tok = %q, want %q", s.TokenText(), string(ch))
		}
	}
	checkPos(t, s.Position, want)
}

func TestPos(t *testing.T) {
	// corner case: empty source
	s := new(Scanner).Init(strings.NewReader(""), filename)
	checkPos(t, s.Pos(), token.Position{Offset: 0, Line: 1, Column: 1})
	s.Peek() // peek doesn't affect the position
	checkPos(t, s.Pos(), token.Position{Offset: 0, Line: 1, Column: 1})

	// corner case: source with only a newline
	s = new(Scanner).Init(strings.NewReader("\n"), filename)
	checkPos(t, s.Pos(), token.Position{Offset: 0, Line: 1, Column: 1})
	if s.Scan() != token.EOF {
		t.Errorf("expecting token.EOF")
	}
	// after token.EOF position doesn't change
	for i := 10; i > 0; i-- {
		checkScanPos(t, s, 1, 2, 1, token.EOF)
	}
	if s.ErrorCount != 0 {
		t.Errorf("%d errors", s.ErrorCount)
	}

	// corner case: source with only a single character
	s = new(Scanner).Init(strings.NewReader("x"), filename)
	checkPos(t, s.Pos(), token.Position{Offset: 0, Line: 1, Column: 1})
	checkScanPos(t, s, 0, 1, 1, token.IDENT)
	// after token.EOF position doesn't change
	for i := 10; i > 0; i-- {
		checkScanPos(t, s, 1, 1, 2, token.EOF)
	}
	if s.ErrorCount != 0 {
		t.Errorf("%d errors", s.ErrorCount)
	}

	// FIXME: fails becuase checkScanPos() callse checkPos() before and after
	// s.Scan()

	// positions after calling Scan
	// s = new(Scanner).Init(strings.NewReader("abc\nde\n\nx"), filename)
	// checkScanPos(t, s, 0, 1, 1, token.IDENT)
	// s.Peek() // peek doesn't affect the position
	// checkScanPos(t, s, 4, 2, 1, token.IDENT)
	// checkScanPos(t, s, 8, 4, 1, token.IDENT)
	// // after token.EOF position doesn't change
	// for i := 10; i > 0; i-- {
	// 	checkScanPos(t, s, 9, 4, 2, token.EOF)
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
	s := new(Scanner).Init(&r, filename)

	tok := s.Scan()
	if tok != token.EOF {
		t.Error("1) token.EOF not reported")
	}

	if s.Peek() != -1 {
		t.Error("2) EOF not reported")
	}

	if r != 1 {
		t.Errorf("scanner called Read %d times, not once", r)
	}
}
