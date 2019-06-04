// Copyright 2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Note: the line below requires go >= 1.4 (it won't work on Debian jessie)
//go:generate go-bindata -prefix data -pkg bytecodes data

package bytecodes

import (
	"fmt"

	"github.com/ev3dev/lmsasm/ast"
	"gopkg.in/yaml.v2"
)

type Defs struct {
	Defines map[string]Define
	Enums   map[string]Enum
	Ops     map[string]Opcode
}

type Opcode struct {
	Desc    string ",omitempty"
	Value   uint8
	Params  []Param
	Support *Support ",omitempty"
	Remarks string   ",omitempty"
}

type Param struct {
	Name        string
	Desc        string ",omitempty"
	Type        ParamType
	ElementType ParamType "element-type,omitempty"
	Dir         Direction
	Commands    map[string]Command ",omitempty"
	Enum        *Enum              ",omitempty"
	Remarks     string             ",omitempty"
}

type ParamType string

const (
	ParamTypeSubparam     ParamType = "SUBP"
	ParamTypeNumberParams           = "PARNO"
	ParamTypeLabel                  = "PARLAB"
	ParamTypeValues                 = "PARVALUES"
	ParamTypeInt8                   = "PAR8"
	ParamTypeInt16                  = "PAR16"
	ParamTypeInt32                  = "PAR32"
	ParamTypeFloat                  = "PARF"
	ParamTypeString                 = "PARS"
	ParamTypeVariable               = "PARV"
)

type Direction string

const (
	DirectionIn    Direction = "in"
	DirectionOut             = "out"
	DirectionInOut           = "inout"
)

type SupportType string

const (
	SupportTypeOfficial SupportType = "official"
	SupportTypeXtended              = "xtended"
	SupportTypeCompat               = "compat"
)

type Command struct {
	Desc    string ",omitempty"
	Value   uint8
	Params  []Param
	Support *Support ",omitempty"
	Remarks string   ",omitempty"
}

type Enum struct {
	Desc    string ",omitempty"
	Members map[string]EnumMember
	Support *Support ",omitempty"
	Remarks string   ",omitempty"
}

type EnumMember struct {
	Desc    string ",omitempty"
	Value   int32
	Support *Support ",omitempty"
	Remarks string   ",omitempty"
}

type Define struct {
	Desc    string ",omitempty"
	Value   interface{}
	Support *Support ",omitempty"
	Remarks string   ",omitempty"
}

type Support struct {
	Official bool
	Xtended  bool
	Compat   bool
}

func (s SupportType) OpcodeLookup(ops map[string]Opcode) map[uint8]string {
	m := make(map[uint8]string)
	for k, v := range ops {
		if !v.Support.Check(s) {
			continue
		}
		m[v.Value] = k
	}
	return m
}

func (s SupportType) CommandLookup(cmds map[string]Command) map[uint8]string {
	m := make(map[uint8]string)
	for k, v := range cmds {
		if !v.Support.Check(s) {
			continue
		}
		m[v.Value] = k
	}
	return m
}

func (s SupportType) EnumLookup(ems map[string]EnumMember) map[int32]string {
	m := make(map[int32]string)
	for k, v := range ems {
		if !v.Support.Check(s) {
			continue
		}
		m[v.Value] = k
	}
	return m
}

func (s *Support) Check(v SupportType) bool {
	if s == nil {
		return true // default value when "support:" is not specified in yaml
	}
	switch v {
	case SupportTypeOfficial:
		return s.Official
	case SupportTypeXtended:
		return s.Xtended
	case SupportTypeCompat:
		return s.Compat
	default:
		panic(fmt.Sprintf("Bad support version name %v", v))
	}
}

func GetDefs(name string) (defs Defs, err error) {
	data, err := Asset(name + ".yml")
	if err != nil {
		return
	}

	err = yaml.Unmarshal(data, &defs)

	return
}

func Scope(name string, support SupportType) (*ast.Scope, error) {
	defs, err := GetDefs(name)
	if err != nil {
		return nil, err
	}

	s := ast.NewScope(nil)

	for k, v := range defs.Defines {
		if !v.Support.Check(support) {
			continue
		}
		o := ast.NewObj(ast.Con, k)
		o.Decl = s
		o.Data = v
		s.Insert(o)
	}

	for _, e := range defs.Enums {
		for k, v := range e.Members {
			if !v.Support.Check(support) {
				continue
			}
			o := ast.NewObj(ast.Con, k)
			o.Decl = s
			o.Data = v
			s.Insert(o)
		}
	}

	for k, v := range defs.Ops {
		if !v.Support.Check(support) {
			continue
		}
		o := ast.NewObj(ast.Op, k)
		o.Decl = s
		o.Data = v
		s.Insert(o)
		for _, p := range v.Params {
			for k, v := range p.Commands {
				if !v.Support.Check(support) {
					continue
				}
				o := ast.NewObj(ast.Cmd, k)
				o.Decl = s
				o.Data = v
				s.Insert(o)
			}
		}
	}

	return s, nil
}
