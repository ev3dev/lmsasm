// Copyright 2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Note: the line below requires go >= 1.4 (it won't work on Debian jessie)
//go:generate go-bindata -prefix data -pkg bytecodes data

package bytecodes

import "gopkg.in/yaml.v2"

type Defs struct {
	Enums map[string]Enum
	Ops   map[string]Opcode
}

type Opcode struct {
	Desc    string ",omitempty"
	Value   uint8
	Params  []Param
	Support Support
	Remarks string ",omitempty"
}

type Param struct {
	Name     string
	Desc     string ",omitempty"
	Type     ParamType
	Commands map[string]Command ",omitempty"
	Enum     Enum               ",omitempty"
	Remarks  string             ",omitempty"
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

type Command struct {
	Desc    string ",omitempty"
	Value   uint8
	Params  []Param
	Support Support
	Remarks string ",omitempty"
}

type Enum struct {
	Desc    string ",omitempty"
	Members map[string]EnumMember
	Remarks string ",omitempty"
}

type EnumMember struct {
	Desc    string ",omitempty"
	Value   int
	Remarks string ",omitempty"
}

type Support struct {
	Official bool
	Xtended  bool
	Compat   bool
}

func GetDefs(name string) (defs Defs, err error) {
	data, err := Asset(name + ".yml")
	if err != nil {
		return
	}

	err = yaml.Unmarshal(data, &defs)

	return
}
