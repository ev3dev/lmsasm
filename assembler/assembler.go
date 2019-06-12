// Copyright 2016,2019 David Lechner <david@lechnology.com>
// Copyright 2019 LEGO System A/S
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package assembler

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"

	"github.com/ev3dev/lmsasm/ast"
	"github.com/ev3dev/lmsasm/bytecodes"
	"github.com/ev3dev/lmsasm/scanner"
	"github.com/ev3dev/lmsasm/token"
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
		if e.Obj == nil {
			err = errors.New("Unknown identifier in expression")
			return
		}
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
				if d, ok := e.Obj.Decl.(*ast.DefineSpec); ok {
					value, err = resolveConstInt(d.Value)
				} else {
					err = errors.New("Constant identifier is missing declaration")
				}
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
		err = fmt.Errorf("Unknown value data type '%v'", spec.Type)
	}

	return
}

func getParamSpecSize(spec *ast.ParamSpec) (size int32, err error) {
	switch spec.Type {
	case token.IN_8, token.OUT_8, token.IO_8:
		size = 1
	case token.IN_16, token.OUT_16, token.IO_16, token.IN_H, token.OUT_H, token.IO_H:
		size = 2
	case token.IN_32, token.OUT_32, token.IO_32, token.IN_F, token.OUT_F, token.IO_F:
		size = 4
	case token.IN_S, token.OUT_S, token.IO_S:
		size, err = resolveConstInt(spec.Length)
	default:
		err = fmt.Errorf("Unknown parameter data type '%v'", spec.Type)
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
	case token.IN_8:
		v = 0x80
	case token.IN_16, token.IN_H:
		v = 0x81
	case token.IN_32:
		v = 0x82
	case token.IN_F:
		v = 0x83
	case token.IN_S:
		v = 0x84
	case token.OUT_8:
		v = 0x40
	case token.OUT_16, token.OUT_H:
		v = 0x41
	case token.OUT_32:
		v = 0x42
	case token.OUT_F:
		v = 0x43
	case token.OUT_S:
		v = 0x44
	case token.IO_8:
		v = 0xc0
	case token.IO_16, token.IO_H:
		v = 0xc1
	case token.IO_32:
		v = 0xc2
	case token.IO_F:
		v = 0xc3
	case token.IO_S:
		v = 0xc4
	default:
		panic("Unexpected token.ParamType")
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

func emitIntConst(value, qualifier string, paramType bytecodes.ParamType, quirks QuirkFlags) (*Instruction, error) {
	i, err := strconv.ParseInt(value, 0, 32)
	b := lcBytes(PRIMPAR_CONST, int32(i))
	if err != nil {
		// Allow unsigned int, but only for hex literals
		if strings.HasPrefix(value, "0x") {
			u, err := strconv.ParseUint(value, 0, 32)
			if err != nil {
				return nil, err
			}
			binary.LittleEndian.PutUint32(b[1:], uint32(u))
		} else {
			return nil, err
		}
	}

	switch paramType {
	case bytecodes.ParamTypeInt8, bytecodes.ParamTypeNumberParams:
		if b[0]&PRIMPAR_LONG != 0 {
			if b[0]&PRIMPAR_BYTES != PRIMPAR_1_BYTE {
				return nil, fmt.Errorf("Integer value is too big for %v", paramType)
			}
		}
	case bytecodes.ParamTypeInt16:
		if b[0]&PRIMPAR_LONG != 0 {
			if b[0]&PRIMPAR_BYTES == PRIMPAR_4_BYTES {
				return nil, fmt.Errorf("Integer value is too big for %v", paramType)
			}
		}
	case bytecodes.ParamTypeInt32, bytecodes.ParamTypeVariable:
		// OK
	default:
		return nil, fmt.Errorf("Using int constant for non-integer parameter %v", paramType)
	}

	return &Instruction{
		Bytes: b,
		size:  int32(len(b)),
		Desc:  qualifier + " int",
	}, nil
}

func emitFloatConst(value, qualifier string, paramType bytecodes.ParamType, quirks QuirkFlags) (*Instruction, error) {
	f, err := strconv.ParseFloat(strings.TrimSuffix(value, "F"), 32)
	if err != nil {
		return nil, err
	}

	switch paramType {
	case bytecodes.ParamTypeFloat, bytecodes.ParamTypeVariable:
		// OK
	default:
		return nil, fmt.Errorf("Using float constant for non-float parameter %v", paramType)
	}

	var b []byte
	if quirks&OptimizeFloatConst != 0 {
		i := int32(math.Float32bits(float32(f)))
		b = lcBytes(PRIMPAR_CONST, i)
	} else {
		// EV3-G quirk - 0.0F is always emitted as long value
		buf := new(bytes.Buffer)
		buf.WriteByte(PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_4_BYTES)
		binary.Write(buf, binary.LittleEndian, float32(f))
		b = buf.Bytes()
	}

	return &Instruction{
		Bytes: b,
		size:  int32(len(b)),
		Desc:  qualifier + " float",
	}, nil
}

func emitStringConst(value, qualifier string, paramType bytecodes.ParamType) (*Instruction, error) {
	switch paramType {
	case bytecodes.ParamTypeString, bytecodes.ParamTypeVariable:
		// OK
	case bytecodes.ParamTypeInt8:
		// TODO: ev3.yml needs to be updated to differentiate between PAR8 and array of PAR8
	default:
		return nil, fmt.Errorf("Using string constant for non-string parameter %v", paramType)
	}

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
	}, nil
}

func emitEnum(value int32, paramType bytecodes.ParamType) (*Instruction, error) {
	b := lcBytes(PRIMPAR_CONST, value)

	switch paramType {
	case bytecodes.ParamTypeInt8, bytecodes.ParamTypeNumberParams:
		if b[0]&PRIMPAR_LONG != 0 {
			if b[0]&PRIMPAR_BYTES != PRIMPAR_1_BYTE {
				return nil, fmt.Errorf("Enum value is too big for %v", paramType)
			}
		}
	case bytecodes.ParamTypeInt16:
		if b[0]&PRIMPAR_LONG != 0 {
			if b[0]&PRIMPAR_BYTES == PRIMPAR_4_BYTES {
				return nil, fmt.Errorf("Enum value is too big for %v", paramType)
			}
		}
	case bytecodes.ParamTypeInt32, bytecodes.ParamTypeVariable:
		// OK
	default:
		return nil, fmt.Errorf("Using enum constant for non-integer parameter %v", paramType)
	}

	return &Instruction{
		Bytes: b,
		size:  int32(len(b)),
		Desc:  "enum",
	}, nil
}

func emitVar(variable variableInfo, global bool, paramType bytecodes.ParamType) (*Instruction, error) {
	if !valueTypeOk(variable.valueType, paramType) {
		return nil, fmt.Errorf("Variable type %v does not match parameter type %v",
			variable.valueType, paramType)
	}

	var flag byte
	var desc string
	if global {
		flag = PRIMPAR_GLOBAL
		desc = "global var"
	} else {
		flag = PRIMPAR_LOCAL
		desc = "local var"
	}
	b := lcBytes(PRIMPAR_VARIABLE|flag, variable.offset)

	return &Instruction{
		Bytes: b,
		size:  int32(len(b)),
		Desc:  desc,
	}, nil
}

func emitHandle(variable variableInfo, global bool, paramType bytecodes.ParamType) (*Instruction, error) {
	if variable.valueType != token.HANDLE {
		// TODO: additional type checking could be done to ensure this is an array
		// handle vs. a file handle
		return nil, errors.New("Expecting HANDLE variable")
	}

	switch paramType {
	case bytecodes.ParamTypeString, bytecodes.ParamTypeVariable:
		// OK
	case bytecodes.ParamTypeInt8, bytecodes.ParamTypeInt16, bytecodes.ParamTypeInt32, bytecodes.ParamTypeFloat:
		// TODO: ev3.yml needs to be updated to differentiate between PAR8 and array of PAR8
	default:
		return nil, fmt.Errorf("Cannot use handle for parameter %v", paramType)
	}

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
	if -128 < variable.offset && variable.offset < 128 {
		buf.WriteByte(flags | PRIMPAR_1_BYTE)
		buf.WriteByte(byte(variable.offset))
	} else {
		buf.WriteByte(flags | PRIMPAR_2_BYTES)
		binary.Write(buf, binary.LittleEndian, int16(variable.offset))
	}

	return &Instruction{
		Bytes: buf.Bytes(),
		size:  int32(buf.Len()),
		Desc:  desc + " handle",
	}, nil
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

func emitExpr(expr ast.Expr, paramType bytecodes.ParamType, direction bytecodes.Direction,
	globals, locals map[string]variableInfo, quirks QuirkFlags) (inst *Instruction, err error) {
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
			if direction != bytecodes.DirectionIn {
				err = errors.New("Cannot use constant as argument to out parameter")
				return
			}
			switch c := e.Obj.Data.(type) {
			case bytecodes.Define:
				switch v := c.Value.(type) {
				case int:
					inst, err = emitIntConst(strconv.Itoa(v), "define", paramType, quirks)
				case float64:
					inst, err = emitFloatConst(strconv.FormatFloat(v, 'f', -1, 32), "define", paramType, quirks)
				case string:
					inst, err = emitStringConst(v, "define", paramType)
				default:
					err = errors.New("Unknown type in bytecode defines")
				}
			case bytecodes.EnumMember:
				inst, err = emitEnum(c.Value, paramType)
			case nil:
				d := e.Obj.Decl.(*ast.DefineSpec)
				inst, err = emitExpr(d.Value, paramType, direction, globals, locals, quirks)
			default:
				err = errors.New("Unknown constant")
			}
		case ast.Obj:
			o := e.Obj.Decl.(*ast.ObjDecl)
			inst = emitObjCall(o)
		case ast.Var:
			info, ok := locals[e.Name]
			global := false
			if !ok {
				info, ok = globals[e.Name]
				global = true
			}
			if ok {
				inst, err = emitVar(info, global, paramType)
			} else {
				err = errors.New("Unknown variable")
			}
		case ast.Par:
			if info, ok := locals[e.Name]; ok {
				inst, err = emitVar(info, false, paramType)
			} else {
				err = errors.New("Unknown parameter")
			}
		case ast.Op:
			err = errors.New("Opcode not allowed as argument")
		case ast.Cmd:
			cmd := e.Obj.Data.(bytecodes.Command)
			inst, err = emitIntConst(strconv.Itoa(int(cmd.Value)), "subcommand", bytecodes.ParamTypeInt8, quirks)
		case ast.Lbl:
			// label bytes will be filled in later
			inst = &Instruction{size: 3, Desc: "label", label: e}
		}
	case *ast.BasicLit:
		if direction != bytecodes.DirectionIn {
			err = errors.New("Using literal on out parameter")
			return
		}
		switch e.Kind {
		case token.INT:
			inst, err = emitIntConst(e.Value, "literal", paramType, quirks)
		case token.FLOAT:
			inst, err = emitFloatConst(e.Value, "literal", paramType, quirks)
		case token.STRING:
			// e.Value is quoted, so slice to trim the quotes
			inst, err = emitStringConst(e.Value[1:len(e.Value)-1], "literal", paramType)
		default:
			err = errors.New("Bad literal")
		}
	case *ast.ParenExpr:
		inst, err = emitExpr(e.X, paramType, direction, globals, locals, quirks)
	case *ast.UnaryExpr:
		switch e.Op {
		case token.AT:
			if ident, ok := e.X.(*ast.Ident); ok {
				info, ok := locals[ident.Name]
				global := false
				if !ok {
					info, ok = globals[ident.Name]
					global = true
				}
				if ok {
					inst, err = emitHandle(info, global, paramType)
				} else {
					err = errors.New("Unknown handle")
				}
			} else {
				err = errors.New("Expecting identifier")
			}
		case token.BANG:
			// bang operator lets us pass anything as a parameter - unsafe!
			inst, err = emitExpr(e.X, bytecodes.ParamTypeVariable, bytecodes.DirectionIn, globals, locals, quirks)
		case token.ADD:
			inst, err = emitExpr(e.X, paramType, direction, globals, locals, quirks)
		case token.SUB:
			if lit, ok := e.X.(*ast.BasicLit); ok {
				switch lit.Kind {
				case token.INT:
					inst, err = emitIntConst("-"+lit.Value, "literal", paramType, quirks)
				case token.FLOAT:
					inst, err = emitFloatConst("-"+lit.Value, "literal", paramType, quirks)
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
			inst, err = emitIntConst(strconv.Itoa(int(i)), "sum", paramType, quirks)
		}
	}

	return
}

type Program struct {
	Magic      [4]byte
	Size       int32
	Version    uint16
	GlobalSize int32
	Objects    []*Object
}

type Object struct {
	Offset       int32
	Owner        uint16
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

// QuirkFlags change the compiler behavior
type QuirkFlags uint32

// QuirkFlags values
const (
	OptimizeFloatConst       QuirkFlags = 1 << iota // allow float constant values to be smaller than PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_4_BYTES
	OptimizeLabels                                  // allow labels offset constant values to be smaller than PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_2_BYTES
	OptimizeDuplicateObjects                        // allow multiple object pointer to point to the same bytecodes
)

// AssembleOptions specify options for the Assemble() function
type AssembleOptions struct {
	Version uint16
	Quirks  QuirkFlags
}

// OptimizeLabels reduces the size of offsets to labels to the optimum size
// instead of using the default size of 3 bytes. The return value is the total
// size in which the list of instructions has changed.
func optimizeLabels(instructions []*Instruction, labels map[string]int32, baseOffset int32) int32 {
	totalDiff := int32(0)
	ipc := baseOffset
	for _, i := range instructions {
		if i.label != nil {
			if offset, ok := labels[i.label.Name]; ok {
				// see what the actual size of the label is
				bytes := lcBytes(PRIMPAR_CONST, offset-ipc-i.size)
				diff := i.size - int32(len(bytes))
				// if the size of the label changed from the previous value then
				// update the size of this instruction and the offset of all labels
				// after this instruction
				if diff > 0 {
					totalDiff += diff
					i.size -= diff
					for n, l := range labels {
						if l > ipc {
							labels[n] -= diff
						}
					}
				}
			}
		}
		ipc += i.size
	}
	return totalDiff
}

func tokenParamTypeToBytecodeParamType(paramType token.ParamType) (bytecodes.ParamType, bytecodes.Direction) {
	switch paramType {
	case token.IN_8:
		return bytecodes.ParamTypeInt8, bytecodes.DirectionIn
	case token.IN_16:
		return bytecodes.ParamTypeInt16, bytecodes.DirectionIn
	case token.IN_32:
		return bytecodes.ParamTypeInt32, bytecodes.DirectionIn
	case token.IN_F:
		return bytecodes.ParamTypeFloat, bytecodes.DirectionIn
	case token.IN_S:
		return bytecodes.ParamTypeString, bytecodes.DirectionIn
	case token.IN_H:
		return bytecodes.ParamTypeHandle, bytecodes.DirectionIn
	case token.OUT_8:
		return bytecodes.ParamTypeInt8, bytecodes.DirectionOut
	case token.OUT_16:
		return bytecodes.ParamTypeInt16, bytecodes.DirectionOut
	case token.OUT_32:
		return bytecodes.ParamTypeInt32, bytecodes.DirectionOut
	case token.OUT_F:
		return bytecodes.ParamTypeFloat, bytecodes.DirectionOut
	case token.OUT_S:
		return bytecodes.ParamTypeString, bytecodes.DirectionOut
	case token.OUT_H:
		return bytecodes.ParamTypeHandle, bytecodes.DirectionOut
	case token.IO_8:
		return bytecodes.ParamTypeInt8, bytecodes.DirectionInOut
	case token.IO_16:
		return bytecodes.ParamTypeInt16, bytecodes.DirectionInOut
	case token.IO_32:
		return bytecodes.ParamTypeInt32, bytecodes.DirectionInOut
	case token.IO_F:
		return bytecodes.ParamTypeFloat, bytecodes.DirectionInOut
	case token.IO_S:
		return bytecodes.ParamTypeString, bytecodes.DirectionInOut
	case token.IO_H:
		return bytecodes.ParamTypeHandle, bytecodes.DirectionInOut
	}
	panic("Bad token.ParamType")
}

func valueTypeOk(valueType token.ValueType, paramType bytecodes.ParamType) bool {
	switch valueType {
	case token.DATA8, token.ARRAY8:
		return paramType == bytecodes.ParamTypeInt8 || paramType == bytecodes.ParamTypeVariable
	case token.DATA16, token.ARRAY16:
		return paramType == bytecodes.ParamTypeInt16 || paramType == bytecodes.ParamTypeVariable
	case token.DATA32, token.ARRAY32:
		return paramType == bytecodes.ParamTypeInt32 || paramType == bytecodes.ParamTypeVariable
	case token.DATAF, token.ARRAYF:
		return paramType == bytecodes.ParamTypeFloat || paramType == bytecodes.ParamTypeVariable
	case token.DATAS:
		return paramType == bytecodes.ParamTypeString || paramType == bytecodes.ParamTypeInt8 || paramType == bytecodes.ParamTypeVariable
	case token.HANDLE:
		return paramType == bytecodes.ParamTypeHandle || paramType == bytecodes.ParamTypeVariable
	}
	panic("Bad token.ValueType")
}

func tokenParamTypeToTokenValueType(paramType token.ParamType) token.ValueType {
	switch paramType {
	case token.IN_8, token.OUT_8, token.IO_8:
		return token.DATA8
	case token.IN_16, token.OUT_16, token.IO_16:
		return token.DATA16
	case token.IN_32, token.OUT_32, token.IO_32:
		return token.DATA32
	case token.IN_F, token.OUT_F, token.IO_F:
		return token.DATAF
	case token.IN_S, token.OUT_S, token.IO_S:
		return token.DATAS
	case token.IN_H, token.OUT_H, token.IO_H:
		return token.HANDLE
	}
	panic("Bad token.ParamType")
}

func paramTypes(file *ast.File, arg *ast.Ident) ([]token.ParamType, error) {
	for _, decl := range file.Decls {
		if objDecl, ok := decl.(*ast.ObjDecl); ok && objDecl.Name.Name == arg.Name {
			paramTypes := make([]token.ParamType, 0, objDecl.ParamCount)
			for _, stmt := range objDecl.Body {
				if x, ok := stmt.(*ast.DeclStmt); ok {
					if y, ok := x.Decl.(*ast.GenDecl); ok && y.Tok == token.PARAMTYPE {
						paramTypes = append(paramTypes, y.Spec.(*ast.ParamSpec).Type)
					}
				}
			}
			if len(paramTypes) != int(objDecl.ParamCount) {
				panic("Missing parameters")
			}
			return paramTypes, nil
		}
	}
	return nil, errors.New("Missing parameters")
}

type variableInfo struct {
	offset    int32
	valueType token.ValueType
}

// Assemble compiles code into LMS2012 VM bytecodes
func (a *Assembler) Assemble(options *AssembleOptions) (Program, error) {
	var nextGlobal int32
	var objects []*Object
	globals := make(map[string]variableInfo)

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
				globals[s.Name.Name] = variableInfo{nextGlobal, s.Type}
				nextGlobal += int32(size)
			case *ast.ParamSpec:
				a.errors.Add(a.fs.Position(d.Pos()), "Parameter declaration in global scope")
			}
		case *ast.ObjDecl:
			offset := pc
			var owner uint16
			var trigger int16
			var nextLocal int32
			var instructions []*Instruction
			locals := make(map[string]variableInfo)
			labels := make(map[string]int32)

			if d.Tok == token.BLOCK {
				trigger = 1
				owner = 1
			}

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
							locals[s.Name.Name] = variableInfo{nextLocal, s.Type}
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
							locals[s.Name.Name] = variableInfo{nextLocal, tokenParamTypeToTokenValueType(s.Type)}
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
					if x == nil {
						a.errors.Add(a.fs.Position(s.Op.Pos()), "Unknown opcode")
					} else if x.Kind == ast.Op {
						opcode := x.Data.(bytecodes.Opcode)
						i := emitUint8(opcode.Value, "opcode")
						pc += i.size
						instructions = append(instructions, i)
						for n, arg := range s.Args {
							// These are the "anything goes" settings, so assume that by default
							// Then try to figure out what the parameter type really is
							var paramType bytecodes.ParamType = bytecodes.ParamTypeVariable
							var direction = bytecodes.DirectionIn

							// FIXME: look up CALL opcode instead of using 0x09
							if opcode.Value == 0x09 && n > 0 {
								// If this is a CALL(), then lookup parameter types from the subcall object
								paramTypes, err := paramTypes(a.file, (s.Args[0]).(*ast.Ident))
								if err == nil {
									if n > len(paramTypes) {
										a.errors.Add(a.fs.Position(arg.Pos()), "Too many arguments to subcall")
									} else {
										paramType, direction = tokenParamTypeToBytecodeParamType(paramTypes[n-1])
									}
								} else {
									a.errors.Add(a.fs.Position(arg.Pos()), err.Error())
								}
							} else if n > 0 && opcode.Params[0].Type == bytecodes.ParamTypeSubparam {
								// If this is an opcode with a subcommand, then lookup the parameter
								// types in the subcommand
								if ident, ok := s.Args[0].(*ast.Ident); ok {
									for subp, cmd := range opcode.Params[0].Commands {
										if subp == ident.Name {
											if n <= len(cmd.Params) {
												paramType = cmd.Params[n-1].Type
												direction = cmd.Params[n-1].Dir
											}
											break
										}
									}
								}
								// No match case is handled below and will result in compiler error
								// so it is safe to ignore it here.
							} else if n < len(opcode.Params) {
								paramType = opcode.Params[n].Type
								direction = opcode.Params[n].Dir

								// if the parameter is a subcommand, validate it
								if paramType == bytecodes.ParamTypeSubparam {
									match := false
									if ident, ok := arg.(*ast.Ident); ok {
										for subp := range opcode.Params[n].Commands {
											if subp == ident.Name {
												match = true
												break
											}
										}
									}
									if !match {
										a.errors.Add(a.fs.Position(arg.Pos()), fmt.Sprintf("Bad subparameter for opcode %s", x.Name))
									}
								}
							}

							// check the last parameter to see if it is PARVALUES
							lastParamIndex := len(opcode.Params) - 1
							lastParam := opcode.Params[lastParamIndex]
							if opcode.Params[0].Type == bytecodes.ParamTypeSubparam {
								// If this is an opcode with a subcommand, then lookup the parameter
								// types in the subcommand
								if ident, ok := s.Args[0].(*ast.Ident); ok {
									for subp, cmd := range opcode.Params[0].Commands {
										if subp == ident.Name && len(cmd.Params) > 0 {
											lastParamIndex = len(cmd.Params)
											lastParam = cmd.Params[lastParamIndex-1]
											break
										}
									}
								}
							}
							if paramType == bytecodes.ParamTypeValues || (n > lastParamIndex && lastParam.Type == bytecodes.ParamTypeValues) {
								paramType = lastParam.ElementType
								direction = lastParam.Dir
							}

							i, err := emitExpr(arg, paramType, direction, globals, locals, options.Quirks)
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
					// skip implicit return if last instruction is RETURN
					if !bytes.Equal(instructions[len(instructions)-1].Bytes, i.Bytes) {
						pc += i.size
						instructions = append(instructions, i)
					}
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

			// The EV3-G desktop software optimizes the size of labels
			if options.Quirks&OptimizeLabels != 0 {
				for {
					diff := optimizeLabels(instructions, labels, offset)
					if diff == 0 {
						break
					}
					pc -= diff
				}
			}

			ipc := offset
			for _, i := range instructions {
				if i.label != nil {
					if offset, ok := labels[i.label.Name]; ok {
						if options.Quirks&OptimizeLabels != 0 {
							i.Bytes = lcBytes(PRIMPAR_CONST, int32(offset-ipc-i.size))
						} else {
							// all labels need to be 3 bytes in size
							buf := new(bytes.Buffer)
							buf.WriteByte(PRIMPAR_LONG | PRIMPAR_2_BYTES)
							binary.Write(buf, binary.LittleEndian, int16(offset-ipc-3))
							i.Bytes = buf.Bytes()
						}
					} else {
						a.errors.Add(a.fs.Position(i.label.Pos()), "Could not find matching label")
					}
					i.label = nil
				}
				ipc += i.size
			}

			o := &Object{
				Offset:       offset,
				Owner:        owner,
				Trigger:      trigger,
				LocalSize:    nextLocal,
				Instructions: instructions,
			}

			if options.Quirks&OptimizeDuplicateObjects != 0 {
				for _, o2 := range objects {
					if o.Owner != o2.Owner {
						continue
					}
					if o.Trigger != o2.Trigger {
						continue
					}
					if o.LocalSize != o2.LocalSize {
						continue
					}
					instEqual := func() bool {
						for i := range o.Instructions {
							if !bytes.Equal(o.Instructions[i].Bytes, o2.Instructions[i].Bytes) {
								return false
							}
						}
						return true
					}
					if !instEqual() {
						continue
					}
					// objects are equal, so make new object point to offset of
					// old object
					o.Offset = o2.Offset
					o.Instructions = nil
					pc = offset
					break
				}
			}

			objects = append(objects, o)
		}
	}
	p := Program{
		Magic:      [4]byte{'L', 'E', 'G', 'O'},
		Size:       pc,
		Version:    options.Version,
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
