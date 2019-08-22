// Copyright 2016,2019 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package assembler

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strings"
	"testing"

	"github.com/ev3dev/lmsasm/ast"
	"github.com/ev3dev/lmsasm/bytecodes"
	"github.com/ev3dev/lmsasm/parser"
	"github.com/ev3dev/lmsasm/token"
)

func TestResolveConstIntWithBadExpr(t *testing.T) {
	expr := ast.BadExpr{}
	_, err := resolveConstInt(&expr)
	if err == nil {
		t.Error("should have received error because expr is ast.BadExpr")
	}
}

func TestResolveConstIntWithIdentWithNilObj(t *testing.T) {
	expr := ast.Ident{}
	_, err := resolveConstInt(&expr)
	if err == nil {
		t.Error("should have received error because expr.Obj == nil")
	}
}

func TestResolveConstIntWithIdentWithInvalidObjKind(t *testing.T) {
	for kind := ast.Bad; kind <= ast.Lbl; kind++ {
		// ast.Con is the only valid kind, so don't test it here
		if kind == ast.Con {
			continue
		}

		obj := ast.Object{Kind: kind}
		expr := ast.Ident{Obj: &obj}
		_, err := resolveConstInt(&expr)
		if err == nil {
			t.Error("should have received error because expr.Obj.Kind is invalid")
		}
	}
}

func TestResolveConstIntWithIdentWithBadCon(t *testing.T) {
	obj := ast.Object{Kind: ast.Con}
	expr := ast.Ident{Obj: &obj}
	_, err := resolveConstInt(&expr)
	if err == nil {
		t.Error("should have received error because expr.Obj.Decl == nil")
	}

	obj.Decl = 0
	_, err = resolveConstInt(&expr)
	if err == nil {
		t.Error("should have received error because expr.Obj.Decl is not *ast.DefineSpec")
	}
}

func TestResolveConstIntWithIdentWithConWithDefineSpec(t *testing.T) {
	expected := int32(99)
	value := ast.BasicLit{Kind: token.INT, Value: fmt.Sprint(expected)}
	spec := ast.DefineSpec{Value: &value}
	obj := ast.Object{Kind: ast.Con, Decl: &spec}
	expr := ast.Ident{Obj: &obj}
	actual, err := resolveConstInt(&expr)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if actual != expected {
		t.Error("expecting to get back the same value we passed in")
	}
}

type literalInfo struct {
	Lit    ast.Expr
	Bytes  []byte
	quirks QuirkFlags
}

var literals = []literalInfo{
	{&ast.BasicLit{Kind: token.INT, Value: "0"}, []byte{0x00}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "1"}, []byte{0x01}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "-1"}, []byte{0x3f}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "31"}, []byte{0x1F}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "-31"}, []byte{0x21}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "32"}, []byte{0x81, 0x20}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "-32"}, []byte{0x81, 0xe0}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "127"}, []byte{0x81, 0x7f}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "-127"}, []byte{0x81, 0x81}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "128"}, []byte{0x82, 0x80, 0x00}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "-128"}, []byte{0x82, 0x80, 0xff}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "32767"}, []byte{0x82, 0xff, 0x7f}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "-32767"}, []byte{0x82, 0x01, 0x80}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "32768"}, []byte{0x83, 0x00, 0x80, 0x00, 0x00}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "-32768"}, []byte{0x83, 0x00, 0x80, 0xff, 0xff}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "2147483647"}, []byte{0x83, 0xff, 0xff, 0xff, 0x7f}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "0x01"}, []byte{0x01}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "-2147483647"}, []byte{0x83, 0x01, 0x00, 0x00, 0x80}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "0xAAAAAAAA"}, []byte{0x83, 0xaa, 0xaa, 0xaa, 0xaa}, 0},
	{&ast.BasicLit{Kind: token.INT, Value: "0xFFFFFFFF"}, []byte{0x83, 0xff, 0xff, 0xff, 0xff}, 0},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "0.0F"}, []byte{0x83, 0x00, 0x00, 0x00, 0x00}, 0},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "0.0F"}, []byte{0x00}, OptimizeFloatConst},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "1.0F"}, []byte{0x83, 0x00, 0x00, 0x80, 0x3f}, 0},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "1.0F"}, []byte{0x83, 0x00, 0x00, 0x80, 0x3f}, OptimizeFloatConst},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "-1.0F"}, []byte{0x83, 0x00, 0x00, 0x80, 0xbf}, 0},
	{&ast.BasicLit{Kind: token.FLOAT, Value: "-1.0F"}, []byte{0x83, 0x00, 0x00, 0x80, 0xbf}, OptimizeFloatConst},
	{&ast.BasicLit{Kind: token.STRING, Value: ""}, []byte{0x80, 0x00}, 0},
	{&ast.BasicLit{Kind: token.STRING, Value: "test"}, []byte{0x80, 0x74, 0x65, 0x73, 0x74, 0x00}, 0},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\n"}, []byte{0x80, 0x0a, 0x00}, 0},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\q"}, []byte{0x80, 0x27, 0x00}, 0},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\r"}, []byte{0x80, 0x0d, 0x00}, 0},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\t"}, []byte{0x80, 0x09, 0x00}, 0},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\\\"}, []byte{0x80, 0x5c, 0x00}, 0},
	{&ast.BasicLit{Kind: token.STRING, Value: "\\t\\t\\t\\t"}, []byte{0x80, 0x09, 0x09, 0x09, 0x09, 0x00}, 0},
}

func TestBasicLit(t *testing.T) {
	for _, r := range literals {
		var inst *Instruction
		var err error
		l := r.Lit.(*ast.BasicLit)
		switch l.Kind {
		case token.INT:
			inst, err = emitIntConst(l.Value, "", bytecodes.ParamTypeInt32, r.quirks)
			if err != nil {
				t.Fatalf("error from emitIntConst: %v", err)
			}
		case token.FLOAT:
			inst, err = emitFloatConst(l.Value, "", bytecodes.ParamTypeFloat, r.quirks)
			if err != nil {
				t.Fatalf("error from emitFloatConst: %v", err)
			}
		case token.STRING:
			inst, err = emitStringConst(l.Value, "", bytecodes.ParamTypeString)
			if err != nil {
				t.Fatalf("error from emitStringConst: %v", err)
			}
		}
		if len(inst.Bytes) == len(r.Bytes) {
			for i, x := range inst.Bytes {
				if x != r.Bytes[i] {
					t.Errorf("bad bytes for '%v' at index %v:\nexpecting 0x%02x but got 0x%02x", l.Value, i, r.Bytes[i], x)
				}
			}
		} else {
			t.Errorf("bad byte length for '%v': expecting %v but got %v", l.Value, len(r.Bytes), len(inst.Bytes))
		}
	}
}

func TestOfficial(t *testing.T) {
	lmsFiles, err := filepath.Glob("testdata/*/*.lms")
	if err != nil {
		t.Fatal("Failed to glob files:", err)
	}
	s, err := bytecodes.Scope("ev3", "official")
	if err != nil {
		t.Fatal("Failed to read bytecodes:", err)
	}
	for _, n := range lmsFiles {
		t.Log("Parsing", n)
		fs := token.NewFileSet()
		f, err := parser.ParseFile(fs, n, nil, s, parser.DeclarationErrors)
		if err != nil {
			t.Error("Failed to parse file:", err)
			continue
		}
		t.Log("Assembling", n)
		a := NewAssembler(fs, f)
		options := AssembleOptions{Version: 109, Quirks: OptimizeFloatConst}
		p, err := a.Assemble(&options)
		if err != nil {
			t.Error("Failed to assemble file:", err)
			continue
		}
		b1 := new(bytes.Buffer)
		if err = p.Write(b1); err != nil {
			t.Error("Failed to write bytes:", err)
			continue
		}
		b2, err := ioutil.ReadFile(strings.Replace(n, ".lms", ".rbf", 1))
		if err != nil {
			t.Error("Failed to read .rbf file:", err)
			continue
		}
		if b1.Len() != len(b2) {
			t.Error("Bad size")
			continue
		}
		for i, b := range b1.Bytes() {
			if b != b2[i] {
				t.Errorf("Bad byte at 0x%04x - expecting 0x%02x, but was 0x%02x", i, b2[i], b)
				goto noGood
			}
		}
		t.Log("GOOD!")
	noGood:
	}
}

func TestMissingParameter(t *testing.T) {
	code := `vmthread main {
		DATA8 x
		CP_EQ8(x,,x)
	}`
	s, err := bytecodes.Scope("ev3", "official")
	if err != nil {
		t.Fatal("Failed to read bytecodes:", err)
	}
	fs := token.NewFileSet()
	f, err := parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a := NewAssembler(fs, f)
	options := AssembleOptions{}
	_, err = a.Assemble(&options)
	if err == nil {
		t.Fatal("Compile should have failed because of missing parameter")
	}

	// verify that test was valid
	code = strings.ReplaceAll(code, ",,", ",0,")
	f, err = parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a = NewAssembler(fs, f)
	_, err = a.Assemble(&options)
	if err != nil {
		t.Fatalf("Compile should have succeeded: %v", err)
	}
}

func TestMissingParameterInCALL(t *testing.T) {
	code := `vmthread main {
		DATA8 x
		CALL(sub,x,,x)
	}
	
	subcall sub {
		IN_8 a
		IN_8 b
		IN_8 c
	}`
	s, err := bytecodes.Scope("ev3", "official")
	if err != nil {
		t.Fatal("Failed to read bytecodes:", err)
	}
	fs := token.NewFileSet()
	f, err := parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a := NewAssembler(fs, f)
	options := AssembleOptions{}
	_, err = a.Assemble(&options)
	if err == nil {
		t.Fatal("Compile should have failed because of missing parameter")
	}

	// verify that test was valid
	code = strings.ReplaceAll(code, ",,", ",0,")
	f, err = parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a = NewAssembler(fs, f)
	_, err = a.Assemble(&options)
	if err != nil {
		t.Fatalf("Compile should have succeeded: %v", err)
	}
}

func TestMissingParameterWithSubcommand(t *testing.T) {
	code := `vmthread main {
		PROGRAM_INFO(OBJ_STOP,,0)
	}`
	s, err := bytecodes.Scope("ev3", "official")
	if err != nil {
		t.Fatal("Failed to read bytecodes:", err)
	}
	fs := token.NewFileSet()
	f, err := parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a := NewAssembler(fs, f)
	options := AssembleOptions{}
	_, err = a.Assemble(&options)
	if err == nil {
		t.Fatal("Compile should have failed because of missing parameter")
	}

	// verify that test was valid
	code = strings.ReplaceAll(code, ",,", ",0,")
	f, err = parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a = NewAssembler(fs, f)
	_, err = a.Assemble(&options)
	if err != nil {
		t.Fatalf("Compile should have succeeded: %v", err)
	}
}

func TestMissingParameterWithPARNO(t *testing.T) {
	code := `vmthread main {
		DATA8 x
		// layer, port, type, mode, format, PARNO, ...
		INPUT_READEXT(0,0,0,0,0,4,x,x,,x)
	}`
	s, err := bytecodes.Scope("ev3", "official")
	if err != nil {
		t.Fatal("Failed to read bytecodes:", err)
	}
	fs := token.NewFileSet()
	f, err := parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a := NewAssembler(fs, f)
	options := AssembleOptions{}
	_, err = a.Assemble(&options)
	if err == nil {
		t.Fatal("Compile should have failed because of missing parameter")
	}

	// verify that test was valid
	code = strings.ReplaceAll(code, ",,", ",0,")
	f, err = parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a = NewAssembler(fs, f)
	_, err = a.Assemble(&options)
	if err != nil {
		t.Fatalf("Compile should have succeeded: %v", err)
	}
}

func TestMissingParameterWithPARVALUES(t *testing.T) {
	code := `vmthread main {
		ARRAY8 x 2
		// DESTINATION, LENGTH, ...
		INIT_BYTES(x,2,,0)
	}`
	s, err := bytecodes.Scope("ev3", "official")
	if err != nil {
		t.Fatal("Failed to read bytecodes:", err)
	}
	fs := token.NewFileSet()
	f, err := parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a := NewAssembler(fs, f)
	options := AssembleOptions{}
	_, err = a.Assemble(&options)
	if err == nil {
		t.Fatal("Compile should have failed because of missing parameter")
	}

	// verify that test was valid
	code = strings.ReplaceAll(code, ",,", ",0,")
	f, err = parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a = NewAssembler(fs, f)
	_, err = a.Assemble(&options)
	if err != nil {
		t.Fatalf("Compile should have succeeded: %v", err)
	}
}

func TestMissingParameterWithSubcommandAndPARNO(t *testing.T) {
	code := `vmthread main {
		DATA8 x
		// layer, port, type, mode, PARNO, ...
		INPUT_DEVICE(READY_SI,0,0,0,DATA_8,4,x,x,,x)
	}`
	s, err := bytecodes.Scope("ev3", "official")
	if err != nil {
		t.Fatal("Failed to read bytecodes:", err)
	}
	fs := token.NewFileSet()
	f, err := parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a := NewAssembler(fs, f)
	options := AssembleOptions{}
	_, err = a.Assemble(&options)
	if err == nil {
		t.Fatal("Compile should have failed because of missing parameter")
	}

	// verify that test was valid
	code = strings.ReplaceAll(code, ",,", ",0,")
	f, err = parser.ParseFile(fs, "test.lms", code, s, parser.DeclarationErrors)
	if err != nil {
		t.Fatal("Failed to parse file:", err)
	}
	a = NewAssembler(fs, f)
	_, err = a.Assemble(&options)
	if err != nil {
		t.Fatalf("Compile should have succeeded: %v", err)
	}
}
