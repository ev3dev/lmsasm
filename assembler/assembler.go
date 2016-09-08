// Copyright 2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package assembler

import (
	"bytes"
	"encoding/binary"
	"github.com/ev3dev/lmsasm/ast"
	"github.com/ev3dev/lmsasm/token"
	"strconv"
	"strings"
)

const (
	PRIMPAR_SHORT      = 0x00
	PRIMPAR_LONG       = 0x80
	PRIMPAR_CONST      = 0x00
	PRIMPAR_VARIABLE   = 0x40
	PRIMPAR_LOCAL      = 0x00
	PRIMPAR_GLOBAL     = 0x20
	PRIMPAR_HANDLE     = 0x10
	PRIMPAR_ADDR       = 0x08
	PRIMPAR_INDEX      = 0x1F
	PRIMPAR_CONST_SIGN = 0x20
	PRIMPAR_VALUE      = 0x3F
	PRIMPAR_BYTES      = 0x07
	PRIMPAR_STRING_OLD = 0
	PRIMPAR_1_BYTE     = 1
	PRIMPAR_2_BYTES    = 2
	PRIMPAR_4_BYTES    = 3
	PRIMPAR_STRING     = 4
	PRIMPAR_LABEL      = 0x20
)

func getBytes(e ast.Expr) []byte {
	buf := new(bytes.Buffer)

	switch e.(type) {
	case *ast.BasicLit:
		l := e.(*ast.BasicLit)
		switch l.Kind {
		case token.INT:
			i, _ := strconv.ParseInt(l.Value, 0, 32)
			// TODO: check for error
			switch {
			case -32 < i && i < 32:
				buf.WriteByte(byte(PRIMPAR_SHORT | PRIMPAR_CONST | (i & PRIMPAR_VALUE)))
			case -128 < i && i < 128:
				buf.WriteByte(byte(PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_1_BYTE))
				buf.WriteByte(byte(i))
			case -32768 < i && i < 32768:
				buf.WriteByte(byte(PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_2_BYTES))
				binary.Write(buf, binary.LittleEndian, int16(i))
			default:
				buf.WriteByte(byte(PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_4_BYTES))
				binary.Write(buf, binary.LittleEndian, int32(i))
			}
		case token.FLOAT:
			f, _ := strconv.ParseFloat(strings.TrimSuffix(l.Value, "F"), 32)
			// TODO: check for error
			buf.WriteByte(byte(PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_4_BYTES))
			binary.Write(buf, binary.LittleEndian, float32(f))
		case token.STRING:
			buf.WriteByte(byte(PRIMPAR_LONG | PRIMPAR_CONST | PRIMPAR_STRING_OLD))
			s := l.Value
			s = strings.Replace(s, "\\n", "\n", -1)
			// lms2012 uses `"` instead of `'` for \q, but this doesn't make
			// sense because strings are single quoted
			s = strings.Replace(s, "\\q", "'", -1)
			s = strings.Replace(s, "\\r", "\r", -1)
			s = strings.Replace(s, "\\t", "\t", -1)
			buf.WriteString(s)
			buf.WriteByte(0) // null terminator
		}
	}

	return buf.Bytes()
}
