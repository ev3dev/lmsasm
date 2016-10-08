// Copyright 2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package assembler

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"github.com/ev3dev/lmsasm/ast"
	"github.com/ev3dev/lmsasm/bytecodes"
	"github.com/ev3dev/lmsasm/scanner"
	"github.com/ev3dev/lmsasm/token"
	"io"
	"math"
	"strconv"
	"strings"
)

type Assembler struct {
	fs     *token.FileSet
	file   *ast.File
	errors scanner.ErrorList
}

func NewAssembler(fs *token.FileSet, file *ast.File) *Assembler {
	return &Assembler{fs: fs, file: file}
}

func resolveConstInt(expr ast.Expr) (value int32, err error) {
	switch e := expr.(type) {
	case *ast.BadExpr:
		err = errors.New("Bad expression")
	case *ast.BasicLit:
		switch e.Kind {
		case token.INT:
			var i int64
			i, err = strconv.ParseInt(e.Value, 0, 32)
			value = int32(i)
		default:
			err = errors.New("Expecting integer literal")
		}
	case *ast.Ident:
		switch e.Obj.Kind {
		case ast.Con:
			switch c := e.Obj.Data.(type) {
			case bytecodes.Define:
				switch v := c.Value.(type) {
				case int:
					value = int32(v)
				default:
					err = errors.New("Expecting integer identifier")
				}
			case bytecodes.EnumMember:
				value = c.Value
			case nil:
				d := e.Obj.Decl.(*ast.DefineSpec)
				value, err = resolveConstInt(d.Value)
			default:
				err = errors.New("Unknown constant")
			}
		default:
			err = errors.New("Expecting constant identifier")
		}
	case *ast.ParenExpr:
		value, err = resolveConstInt(e.X)
	case *ast.BinaryExpr:
		var x, y int32
		x, err = resolveConstInt(e.X)
		if err != nil {
			return
		}
		y, err = resolveConstInt(e.Y)
		if err != nil {
			return
		}
		switch e.Op {
		case token.ADD:
			value = int32(x + y)
		case token.SUB:
			value = int32(x - y)
		case token.MUL:
			value = int32(x * y)
		case token.QUO:
			value = int32(x / y)
		default:
			err = errors.New("Unknown binary operator")
		}
	default:
		err = errors.New("Expecting constant expression")
	}

	return
}

func getValueSpecSize(spec *ast.ValueSpec) (size int32, err error) {
	switch spec.Type {
	case token.DATA8:
		size = 1
	case token.DATA16, token.HANDLE:
		size = 2
	case token.DATA32, token.DATAF:
		size = 4
	case token.ARRAY8, token.DATAS:
		size, err = resolveConstInt(spec.Length)
	case token.ARRAY16:
		size, err = resolveConstInt(spec.Length)
		if err != nil {
			size *= 2
		}
	case token.ARRAY32, token.ARRAYF:
		size, err = resolveConstInt(spec.Length)
		if err != nil {
			size *= 4
		}
	default:
		err = errors.New(fmt.Sprintf("Unknown value data type '%v'", spec.Type))
	}

	return
}

func getParamSpecSize(spec *ast.ParamSpec) (size int32, err error) {
	switch spec.Type {
	case token.IN_8, token.OUT_8, token.IO_8:
		size = 1
	case token.IN_16, token.OUT_16, token.IO_16:
		size = 2
	case token.IN_32, token.OUT_32, token.IO_32, token.IN_F, token.OUT_F, token.IO_F:
		size = 4
	case token.IN_S, token.OUT_S, token.IO_S:
		size, err = resolveConstInt(spec.Length)
	default:
		err = errors.New(fmt.Sprintf("Unknown parameter data type '%v'", spec.Type))
	}

	return
}

func align(val int32, size int32) int32 {
	a := int32(size)
	val += a - 1
	return a * (val / a)
}

const (
	PRIMPAR_SHORT      byte = 0x00
	PRIMPAR_LONG            = 0x80
	PRIMPAR_CONST           = 0x00
	PRIMPAR_VARIABLE        = 0x40
	PRIMPAR_LOCAL           = 0x00
	PRIMPAR_GLOBAL          = 0x20
	PRIMPAR_HANDLE          = 0x10
	PRIMPAR_ADDR            = 0x08
	PRIMPAR_INDEX           = 0x1F
	PRIMPAR_CONST_SIGN      = 0x20
	PRIMPAR_VALUE           = 0x3F
	PRIMPAR_BYTES           = 0x07
	PRIMPAR_STRING_OLD      = 0
	PRIMPAR_1_BYTE          = 1
	PRIMPAR_2_BYTES         = 2
	PRIMPAR_4_BYTES         = 3
	PRIMPAR_STRING          = 4
	PRIMPAR_LABEL           = 0x20
)

func emitParamType(typ token.ParamType) *Instruction {
	var v uint8

	switch typ {
	case "IN_8":
		v = 0x80
	case "IN_16":
		v = 0x81
	case "IN_32":
		v = 0x82
	case "IN_F":
		v = 0x83
	case "IN_S":
		v = 0x84
	case "OUT_8":
		v = 0x40
	case "OUT_16":
		v = 0x41
	case "OUT_32":
		v = 0x42
	case "OUT_F":
		v = 0x43
	case "OUT_S":
		v = 0x44
	case "IO_8":
		v = 0xc0
	case "IO_16":
		v = 0xc1
	case "IO_32":
		v = 0xc2
	case "IO_F":
		v = 0xc3
	case "IO_S":
		v = 0xc4
	default:
		// TODO: should probably return error
	}

	return emitUint8(v, "param type")
}

func emitUint8(value uint8, desc string) *Instruction {
	return &Instruction{
		Bytes: []byte{value},
		size:  1,
		Desc:  desc,
	}
}

func lcBytes(flag byte, i int32) []byte {
	buf := new(bytes.Buffer)
	switch {
	case -32 < i && i < 32:
		buf.WriteByte(PRIMPAR_SHORT | flag | (byte(i) & PRIMPAR_VALUE))
	case -128 < i && i < 128:
		buf.WriteByte(PRIMPAR_LONG | flag | PRIMPAR_1_BYTE)
		buf.WriteByte(byte(i))
	case -32768 < i && i < 32768:
		buf.WriteByte(PRIMPAR_LONG | flag | PRIMPAR_2_BYTES)
		binary.Write(buf, binary.LittleEndian, int16(i))
	default:
		buf.WriteByte(PRIMPAR_LONG | flag | PRIMPAR_4_BYTES)
		binary.Write(buf, binary.LittleEndian, int32(i))
	}

	return buf.Bytes()
}

func emitIntConst(value, qualifier string) *Instruction {
	i, err := strconv.ParseInt(value, 0, 32)
	b := lcBytes(PRIMPAR_CONST, int32(i))
	if err != nil {
		// TODO: uint32 should not be allowed in lms source code. However,
		// SelfTest/Test2020.lms from the official source code uses 0xFFFFFFFF,
		// so allowing it for now.
		u, _ := strconv.ParseUint(value, 0, 32)
		// TODO: should check and return error here
		binary.LittleEndian.PutUint32(b[1:], uint32(u))
	}

	return &Instruction{
		Bytes: b,
		size:  int32(len(b)),
		Desc:  qualifier + " int",
	}
}

func emitFloatConst(value, qualifier string) *Instruction {
	f, _ := strconv.ParseFloat(strings.TrimSuffix(value, "F"), 32)
	// TODO: check for error
	i := int32(math.Float32bits(float32(f)))
	b := lcBytes(PRIMPAR_CONST, i)

	return &Instruction{
		Bytes: b,
		size:  int32(len(b)),
		Desc:  qualifier + " float",
	}
}

func emitStringConst(value, qualifier string) *Instruction {
	buf := new(bytes.Buffer)
	buf.WriteByte(byte(PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_STRING_OLD))
	s := value
	s = strings.Replace(s, "\\n", "\n", -1)
	// lms2012 uses `"` instead of `'` for \q, but this doesn't make
	// sense because strings are single quoted
	s = strings.Replace(s, "\\q", "'", -1)
	s = strings.Replace(s, "\\r", "\r", -1)
	s = strings.Replace(s, "\\t", "\t", -1)
	s = strings.Replace(s, "\\\\", "\\", -1)
	buf.WriteString(s)
	buf.WriteByte(0) // null terminator

	return &Instruction{
		Bytes: buf.Bytes(),
		size:  int32(buf.Len()),
		Desc:  qualifier + " string",
	}
}

func emitEnum(value int32) *Instruction {
	b := lcBytes(PRIMPAR_CONST, value)

	return &Instruction{
		Bytes: b,
		size:  int32(len(b)),
		Desc:  "enum",
	}
}

func emitVar(offset int32, global bool) *Instruction {
	var flag byte
	var desc string
	if global {
		flag = PRIMPAR_GLOBAL
		desc = "global var"
	} else {
		flag = PRIMPAR_LOCAL
		desc = "local var"
	}
	b := lcBytes(PRIMPAR_VARIABLE|flag, offset)

	return &Instruction{
		Bytes: b,
		size:  int32(len(b)),
		Desc:  desc,
	}
}

func emitHandle(offset int16, global bool) *Instruction {
	flags := byte(PRIMPAR_LONG | PRIMPAR_VARIABLE | PRIMPAR_HANDLE)
	var desc string
	if global {
		flags |= PRIMPAR_GLOBAL
		desc = "global"
	} else {
		flags |= PRIMPAR_LOCAL
		desc = "local"
	}

	buf := new(bytes.Buffer)
	if -128 < offset && offset < 128 {
		buf.WriteByte(flags | PRIMPAR_1_BYTE)
		buf.WriteByte(byte(offset))
	} else {
		buf.WriteByte(flags | PRIMPAR_2_BYTES)
		binary.Write(buf, binary.LittleEndian, offset)
	}

	return &Instruction{
		Bytes: buf.Bytes(),
		size:  int32(buf.Len()),
		Desc:  desc + " handle",
	}
}

func emitObjCall(obj *ast.ObjDecl) *Instruction {
	buf := new(bytes.Buffer)
	if obj.Tok != token.SUBCALL {
		buf.WriteByte(byte(obj.Index + 1))
	} else {
		buf.Write(lcBytes(0, obj.Index+1))
		buf.WriteByte(obj.ParamCount)
	}

	return &Instruction{
		Bytes: buf.Bytes(),
		size:  int32(buf.Len()),
		Desc:  "object call",
	}
}

func emitExpr(expr ast.Expr, globals, locals map[string]int32) (inst *Instruction, err error) {
	switch e := expr.(type) {
	case *ast.BadExpr:
		err = errors.New("Bad expression")
		return
	case *ast.Ident:
		if e.Obj == nil {
			err = errors.New("Unknown identifier in expression")
			return
		}
		switch e.Obj.Kind {
		case ast.Bad:
			err = errors.New("Bad identifier")
		case ast.Con:
			switch c := e.Obj.Data.(type) {
			case bytecodes.Define:
				switch v := c.Value.(type) {
				case int:
					inst = emitIntConst(strconv.Itoa(v), "define")
				case float64:
					inst = emitFloatConst(strconv.FormatFloat(v, 'f', -1, 32), "define")
				case string:
					inst = emitStringConst(v, "define")
				default:
					err = errors.New("Unknown type in bytecode defines")
				}
			case bytecodes.EnumMember:
				inst = emitEnum(c.Value)
			case nil:
				d := e.Obj.Decl.(*ast.DefineSpec)
				inst, err = emitExpr(d.Value, globals, locals)
			default:
				err = errors.New("Unknown constant")
			}
		case ast.Obj:
			o := e.Obj.Decl.(*ast.ObjDecl)
			inst = emitObjCall(o)
		case ast.Var:
			offset, ok := locals[e.Name]
			global := false
			if !ok {
				offset, ok = globals[e.Name]
				global = true
			}
			if ok {
				inst = emitVar(offset, global)
			} else {
				err = errors.New("Unknown variable")
			}
		case ast.Par:
			if offset, ok := locals[e.Name]; ok {
				inst = emitVar(offset, false)
			} else {
				err = errors.New("Unknown parameter")
			}
		case ast.Op:
			err = errors.New("Opcode not allowed as argument")
		case ast.Cmd:
			cmd := e.Obj.Data.(bytecodes.Command)
			inst = emitIntConst(strconv.Itoa(int(cmd.Value)), "subcommand")
		case ast.Lbl:
			// label bytes will be filled in later
			inst = &Instruction{size: 3, Desc: "label", label: e}
		}
	case *ast.BasicLit:
		switch e.Kind {
		case token.INT:
			inst = emitIntConst(e.Value, "literal")
		case token.FLOAT:
			inst = emitFloatConst(e.Value, "literal")
		case token.STRING:
			// e.Value is quoted, so slice to trim the quotes
			inst = emitStringConst(e.Value[1:len(e.Value)-1], "literal")
		}
	case *ast.ParenExpr:
		inst, err = emitExpr(e.X, globals, locals)
	case *ast.UnaryExpr:
		switch e.Op {
		case token.AT:
			if ident, ok := e.X.(*ast.Ident); ok {
				offset, ok := locals[ident.Name]
				global := false
				if !ok {
					offset, ok = globals[ident.Name]
					global = true
				}
				if ok {
					inst = emitHandle(int16(offset), global)
				} else {
					err = errors.New("Unknown handle")
				}
			} else {
				err = errors.New("Expecting identifier")
			}
		case token.ADD:
			inst, err = emitExpr(e.X, globals, locals)
		case token.SUB:
			if lit, ok := e.X.(*ast.BasicLit); ok {
				switch lit.Kind {
				case token.INT:
					inst = emitIntConst("-"+lit.Value, "literal")
				case token.FLOAT:
					inst = emitFloatConst("-"+lit.Value, "literal")
				case token.STRING:
					err = errors.New("Expecting numeric literal")
				}
			} else {
				err = errors.New("Expecting literal")
			}
		default:
			err = errors.New("Unknown unary operator")
		}
	case *ast.BinaryExpr:
		// TODO: should handle types other than int
		var i int32
		if i, err = resolveConstInt(e); err == nil {
			inst = emitIntConst(strconv.Itoa(int(i)), "sum")
		}
	}

	return
}

type Program struct {
	Magic      [4]byte
	Size       int32
	Version    int16
	GlobalSize int32
	Objects    []*Object
}

type Object struct {
	Offset       int32
	Owner        int16
	Trigger      int16
	LocalSize    int32
	Instructions []*Instruction
}

type Instruction struct {
	Bytes []byte
	size  int32
	Desc  string // for debug
	label *ast.Ident
}

func (a *Assembler) Assemble() (Program, error) {
	var nextGlobal int32
	var objects []*Object
	globals := make(map[string]int32)

	var headerSize int32 = 16 // size of program header
	for _, decl := range a.file.Decls {
		if _, ok := decl.(*ast.ObjDecl); ok {
			headerSize += 12 // size of object header
		}
	}
	// program counter starts after headers
	pc := headerSize

	for _, decl := range a.file.Decls {
		switch d := decl.(type) {
		case *ast.BadDecl:
			a.errors.Add(a.fs.Position(d.Pos()), "Bad declaration")
		case *ast.GenDecl:
			switch s := d.Spec.(type) {
			case *ast.ValueSpec:
				size, err := getValueSpecSize(s)
				if err != nil {
					a.errors.Add(a.fs.Position(s.Length.Pos()), err.Error())
					continue
				}
				if size == 2 || size == 4 {
					nextGlobal = align(nextGlobal, size)
				}
				globals[s.Name.Name] = nextGlobal
				nextGlobal += int32(size)
			case *ast.ParamSpec:
				a.errors.Add(a.fs.Position(d.Pos()), "Parameter declaration in global scope")
			}
		case *ast.ObjDecl:
			offset := pc
			var trigger int16
			var nextLocal int32
			var instructions []*Instruction
			locals := make(map[string]int32)
			labels := make(map[string]int32)

			if d.Tok == token.SUBCALL {
				trigger = 1
				i := emitUint8(d.ParamCount, "param count")
				pc += i.size
				instructions = append(instructions, i)
			}

			for _, stmt := range d.Body {
			start:
				switch s := stmt.(type) {
				case *ast.BadStmt:
					a.errors.Add(a.fs.Position(s.Pos()), "Bad statement")
				case *ast.DeclStmt:
					switch d2 := s.Decl.(type) {
					case *ast.BadDecl:
						a.errors.Add(a.fs.Position(d2.Pos()), "Bad declaration")
					case *ast.GenDecl:
						switch s := d2.Spec.(type) {
						case *ast.ValueSpec:
							size, err := getValueSpecSize(s)
							if err != nil {
								a.errors.Add(a.fs.Position(s.Pos()), err.Error())
								continue
							}
							if size == 2 || size == 4 {
								nextLocal = align(nextLocal, size)
							}
							locals[s.Name.Name] = nextLocal
							nextLocal += int32(size)
						case *ast.ParamSpec:
							// TODO: should check that parameters are not declared
							// after the first ObjDecl (and probably before the
							// first ValueSpce too)
							size, err := getParamSpecSize(s)
							if err != nil {
								a.errors.Add(a.fs.Position(s.Pos()), err.Error())
								continue
							}
							if size == 2 || size == 4 {
								nextLocal = align(nextLocal, size)
							}
							locals[s.Name.Name] = nextLocal
							nextLocal += int32(size)
							i := emitParamType(s.Type)
							pc += i.size
							instructions = append(instructions, i)
							switch s.Type {
							case token.IN_S, token.OUT_S, token.IO_S:
								if size, err := resolveConstInt(s.Length); err == nil {
									// TODO: should probably check size here to make sure
									// we are not overflowing uint8
									i := emitUint8(uint8(size), "param size")
									pc += i.size
									instructions = append(instructions, i)
								} else {
									a.errors.Add(a.fs.Position(s.Length.Pos()), err.Error())
								}
							}
						}
					case *ast.ObjDecl:
						a.errors.Add(a.fs.Position(d2.Pos()), "Nested object declaration not allowed")
					}
				case *ast.LabeledStmt:
					labels[s.Label.Name] = pc
					stmt = s.Stmt
					// continue for-loop, but use nested stmt instead of next range d.Body
					goto start
				case *ast.CallStmt:
					x := s.Op.Obj
					if x.Kind == ast.Op {
						i := emitUint8(x.Data.(bytecodes.Opcode).Value, "opcode")
						pc += i.size
						instructions = append(instructions, i)
						for _, arg := range s.Args {
							// TODO: check that the arg type matches the opcode template
							// from yaml and the the number of args is correct
							i, err := emitExpr(arg, globals, locals)
							if err == nil {
								pc += i.size
								instructions = append(instructions, i)
							} else {
								a.errors.Add(a.fs.Position(arg.Pos()), err.Error())
							}
						}
					} else {
						a.errors.Add(a.fs.Position(s.Op.Pos()), "Expecting opcode")
					}
				}
			}

			// subcalls have an implicit RETURN() op at the end

			if d.Tok == token.SUBCALL {
				if x, ok := a.file.Scope.Outer.Objects["RETURN"]; ok {
					i := emitUint8(x.Data.(bytecodes.Opcode).Value, "return")
					pc += i.size
					instructions = append(instructions, i)
				} else {
					// TODO: would be better to add RBrace token to ast.ObjDecl
					// and use it's pos here.
					a.errors.Add(a.fs.Position(d.Pos()), "Missing 'RETURN' in bytecodes")
				}
			}

			// each object has an implicit OBJECT_END() op at the end

			if x, ok := a.file.Scope.Outer.Objects["OBJECT_END"]; ok {
				i := emitUint8(x.Data.(bytecodes.Opcode).Value, "object end")
				pc += i.size
				instructions = append(instructions, i)
			} else {
				// TODO: would be better to add RBrace token to ast.ObjDecl
				// and use it's pos here.
				a.errors.Add(a.fs.Position(d.Pos()), "Missing 'OBJECT_END' in bytecodes")
			}

			// resolve the labels

			ipc := offset
			for _, i := range instructions {
				if i.label != nil {
					if offset, ok := labels[i.label.Name]; ok {
						buf := new(bytes.Buffer)
						buf.WriteByte(PRIMPAR_LONG | PRIMPAR_2_BYTES)
						binary.Write(buf, binary.LittleEndian, int16(offset-ipc-3))
						i.Bytes = buf.Bytes()
					} else {
						a.errors.Add(a.fs.Position(i.label.Pos()), "Could not find matching label")
					}
					i.label = nil
				}
				ipc += i.size
			}

			o := &Object{
				Offset:       offset,
				Owner:        0,
				Trigger:      trigger,
				LocalSize:    nextLocal,
				Instructions: instructions,
			}
			objects = append(objects, o)
		}
	}
	// TODO: get version from yaml
	version := int16(109)
	p := Program{
		Magic:      [4]byte{'L', 'E', 'G', 'O'},
		Size:       pc,
		Version:    version,
		GlobalSize: nextGlobal,
		Objects:    objects,
	}

	return p, a.errors.Err()
}

func (p *Program) Write(w io.Writer) error {
	// program header

	_, err := w.Write(p.Magic[:])
	if err != nil {
		return err
	}

	err = binary.Write(w, binary.LittleEndian, p.Size)
	if err != nil {
		return err
	}

	err = binary.Write(w, binary.LittleEndian, p.Version)
	if err != nil {
		return err
	}

	err = binary.Write(w, binary.LittleEndian, int16(len(p.Objects)))
	if err != nil {
		return err
	}

	err = binary.Write(w, binary.LittleEndian, p.GlobalSize)
	if err != nil {
		return err
	}

	// object headers

	for _, o := range p.Objects {
		err = binary.Write(w, binary.LittleEndian, o.Offset)
		if err != nil {
			return err
		}

		err = binary.Write(w, binary.LittleEndian, o.Owner)
		if err != nil {
			return err
		}

		err = binary.Write(w, binary.LittleEndian, o.Trigger)
		if err != nil {
			return err
		}

		err = binary.Write(w, binary.LittleEndian, o.LocalSize)
		if err != nil {
			return err
		}
	}

	// objects

	for _, o := range p.Objects {
		for _, i := range o.Instructions {
			_, err = w.Write(i.Bytes)
			if err != nil {
				return err
			}
		}
	}

	return nil
}
