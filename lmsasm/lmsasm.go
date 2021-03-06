// Copyright 2016,2019 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"flag"
	"github.com/ev3dev/lmsasm/assembler"
	"github.com/ev3dev/lmsasm/bytecodes"
	"github.com/ev3dev/lmsasm/parser"
	"github.com/ev3dev/lmsasm/token"
	"log"
	"os"
)

func main() {
	support := flag.String("support", string(bytecodes.SupportTypeOfficial),
		"Supported bytecode definitions to use. 'official', 'xtended' or 'compat'")
	output := flag.String("output", "out.rbf", "Output file name.")
	version := flag.Uint("version", 0, "Bytecode version identifier.")
	debug := flag.Bool("debug", false, "Enable debug output.")
	ev3g := flag.Bool("ev3g", false, "Enable EV3-G compiler quirks.")
	flag.Parse()

	if flag.NArg() == 0 {
		log.Fatal("Missing input file name")
	}
	filename := flag.Arg(0)

	fs := new(token.FileSet)
	s, err := bytecodes.Scope("ev3", bytecodes.SupportType(*support))
	if err != nil {
		log.Fatal("Error reading bytecodes:", err)
	}

	var mode parser.Mode = parser.DeclarationErrors
	if *debug {
		mode |= parser.Trace
	}

	f, err := parser.ParseFile(fs, filename, nil, s, mode)
	if err != nil {
		log.Fatal("Error parsing file:", err)
	}

	a := assembler.NewAssembler(fs, f)
	options := assembler.AssembleOptions{Version: uint16(*version), Quirks: assembler.OptimizeFloatConst}
	if *ev3g {
		options.Quirks &^= assembler.OptimizeFloatConst
		options.Quirks |= assembler.OptimizeLabels | assembler.OptimizeDuplicateObjects
	}
	p, err := a.Assemble(&options)
	if err != nil {
		log.Fatal("Error assembling file:", err)
	}

	file, err := os.Create(*output)
	if err != nil {
		log.Fatal("Error creating file:", err)
	}

	defer file.Close()
	err = p.Write(file)
	if err != nil {
		log.Fatal("Error writing output:", err)
	}
}
