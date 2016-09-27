// Copyright 2014 Rob Thornton, All rights reserved.
//           2016 David Lechner <david@lechnology.com>
// This source code is governed by a Simplied BSD-License. Please see the
// LICENSE included in this distribution for a copy of the full license
// or, if one is not included, you may also find a copy at
// http://opensource.org/licenses/BSD-2-Clause

package ast

import (
	"fmt"
)

func Print(node Node) {
	Inspect(node, print())
}

func print() func(node Node) bool {
	indent := 0
	return func(node Node) bool {
		// this function is called with node == nil after each leaf node
		if node == nil {
			indent--
			return true
		}

		for i := 0; i < indent; i++ {
			fmt.Print("  ")
		}

		switch n := node.(type) {
		case *BadExpr:
			fmt.Println("BadExpr:")
		case *Ident:
			fmt.Println("Ident:", "Name:", n.Name, "Obj:", n.Obj)
		case *BasicLit:
			fmt.Println("BasicLit:", "Kind:", n.Kind, "Value:", n.Value)
		case *ParenExpr:
			fmt.Println("ParenExpr:")
		case *UnaryExpr:
			fmt.Println("UnaryExpr:", "Op:", n.Op)
		case *BinaryExpr:
			fmt.Println("BinaryExpr:", "Op:", n.Op)
		case *BadStmt:
			fmt.Println("BadStmt:")
		case *DeclStmt:
			fmt.Println("DeclStmt:")
		case *EmptyStmt:
			fmt.Println("EmptyStmt:")
		case *LabeledStmt:
			fmt.Println("LabeledStmt:")
		case *CallStmt:
			fmt.Println("CallStmt:")
		case *DefineSpec:
			fmt.Println("DefineSpec:")
		case *ValueSpec:
			fmt.Println("ValueSpec:", "Type:", n.Type)
		case *ParamSpec:
			fmt.Println("ParamSpec:", "Type:", n.Type)
		case *BadDecl:
			fmt.Println("BadDecl:")
		case *GenDecl:
			fmt.Println("GenDecl:", n.Tok)
		case *ObjDecl:
			fmt.Println("ObjDecl:", n.Tok)
		case *File:
			fmt.Println("File:", "Scope:", n.Scope, "Unresolved: ", n.Unresolved)
		default:
			fmt.Println("dunno what I got...that can't be good")
			fmt.Println(n)
		}
		indent++
		return true
	}
}
