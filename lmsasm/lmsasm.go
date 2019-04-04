// Copyright 2016 David Lechner <david@lechnology.com>
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

	f, err := parser.ParseFile(fs, filename, nil, s, parser.DeclarationErrors)
	if err != nil {
		log.Fatal("Error parsing file:", err)
	}

	a := assembler.NewAssembler(fs, f)
	options := assembler.AssembleOptions{Version: uint16(*version)}
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
