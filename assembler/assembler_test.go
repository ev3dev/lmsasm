// Copyright 2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package assembler

import (
	"github.com/ev3dev/lmsasm/ast"
	"github.com/ev3dev/lmsasm/token"
	"testing"
)

type literalInfo struct {
	Lit   ast.Expr
	Bytes []byte
}

var literals = []literalInfo{
	{&ast.BasicLit{Kind: token.INT, Value: "0"}, []byte{0x00}},
	{&ast.BasicLit{Kind: token.INT, Value: "1"}, []byte{0x01}},
	{&ast.BasicLit{Kind: token.INT, Value: "-1"}, []byte{0x3f}},
	{&ast.BasicLit{Kind: token.INT, Value: "31"}, []byte{0x1F}},
	{&ast.BasicLit{Kind: token.INT, Value: "-31"}, []byte{0x21}},
	{&ast.BasicLit{Kind: token.INT, Value: "32"}, []byte{0x81, 0x20}},
	{&ast.BasicLit{Kind: token.INT, Value: "-32"}, []byte{0x81, 0xe0}},
	{&ast.BasicLit{Kind: token.INT, Value: "127"}, []byte{0x81, 0x7f}},
	{&ast.BasicLit{Kind: token.INT, Value: "-127"}, []byte{0x81, 0x81}},
	{&ast.BasicLit{Kind: token.INT, Value: "128"}, []byte{0x82, 0x80, 0x00}},
	{&ast.BasicLit{Kind: token.INT, Value: "-128"}, []byte{0x82, 0x80, 0xff}},
	{&ast.BasicLit{Kind: token.INT, Value: "32767"}, []byte{0x82, 0xff, 0x7f}},
	{&ast.BasicLit{Kind: token.INT, Value: "-32767"}, []byte{0x82, 0x01, 0x80}},
	{&ast.BasicLit{Kind: token.INT, Value: "32768"}, []byte{0x83, 0x00, 0x80, 0x00, 0x00}},
	{&ast.BasicLit{Kind: token.INT, Value: "-32768"}, []byte{0x83, 0x00, 0x80, 0xff, 0xff}},
	{&ast.BasicLit{Kind: token.INT, Value: "2147483647"}, []byte{0x83, 0xff, 0xff, 0xff, 0x7f}},
	{&ast.BasicLit{Kind: token.INT, Value: "0x01"}, []byte{0x01}},
	{&ast.BasicLit{Kind: token.INT, Value: "-2147483647"}, []byte{0x83, 0x01, 0x00, 0x00, 0x80}},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "0.0F"}, []byte{0x83, 0x00, 0x00, 0x00, 0x00}},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "1.0F"}, []byte{0x83, 0x00, 0x00, 0x80, 0x3f}},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "-1.0F"}, []byte{0x83, 0x00, 0x00, 0x80, 0xbf}},
	{&ast.BasicLit{Kind: token.STRING, Value: ""}, []byte{0x80, 0x00}},
	{&ast.BasicLit{Kind: token.STRING, Value: "test"}, []byte{0x80, 0x74, 0x65, 0x73, 0x74, 0x00}},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\n"}, []byte{0x80, 0x0a, 0x00}},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\q"}, []byte{0x80, 0x27, 0x00}},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\r"}, []byte{0x80, 0x0d, 0x00}},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\t"}, []byte{0x80, 0x09, 0x00}},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\t\\t\\t\\t"}, []byte{0x80, 0x09, 0x09, 0x09, 0x09, 0x00}},
}

func TestBasicLit(t *testing.T) {
	for _, r := range literals {
		b := getBytes(r.Lit)
		l := r.Lit.(*ast.BasicLit)
		if len(b) == len(r.Bytes) {
			for i, x := range b {
				if x != r.Bytes[i] {
					t.Errorf("bad bytes for '%v' at index %v:\nexpecting 0x%02x but got 0x%02x", l.Value, i, r.Bytes[i], x)
				}
			}
		} else {
			t.Errorf("bad byte length for '%v': expecting %v but got %v", l.Value, len(r.Bytes), len(b))
		}
	}
}
