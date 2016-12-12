// Copyright 2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"flag"
	"github.com/ev3dev/lmsasm/bytecodes"
	"io/ioutil"
	"log"
	"os"
	"text/template"
)

func isString(value interface{}) bool {
	switch value.(type) {
	case string:
		return true
	default:
		return false
	}
}

func official() bytecodes.SupportType {
	return bytecodes.SupportTypeOfficial
}

func xtended() bytecodes.SupportType {
	return bytecodes.SupportTypeXtended
}

func compat() bytecodes.SupportType {
	return bytecodes.SupportTypeCompat
}

func main() {
	in := flag.String("in", "-", `The input template file. "-" is STDIN.`)
	out := flag.String("out", "-", `The generated output file. "-" is STDOUT.`)
	flag.Parse()

	var inFile *os.File
	if *in == "-" {
		inFile = os.Stdin
	} else {
		var err error
		inFile, err = os.Open(*in)
		if err != nil {
			log.Fatalln("Could not open input file:", err)
		}
		defer inFile.Close()
	}

	text, err := ioutil.ReadAll(inFile)
	if err != nil {
		log.Fatalln("Failed to read input:", err)
	}

	var outFile *os.File
	if *out == "-" {
		outFile = os.Stdout
	} else {
		var err error
		outFile, err = os.Create(*out)
		if err != nil {
			log.Fatalln("Could not create output file:", err)
		}
		defer outFile.Close()
	}

	funcs := template.FuncMap{
		"isString": isString,
		"official": official,
		"xtended":  xtended,
		"compat":   compat,
	}

	defs, err := bytecodes.GetDefs("ev3")
	if err != nil {
		log.Fatalln("Error reading bytecodes:", err)
	}
	tmpl := template.Must(template.New("test").Funcs(funcs).Parse(string(text)))
	err = tmpl.Execute(outFile, defs)
	if err != nil {
		log.Fatalln("Error parsing template:", err)
	}
}
