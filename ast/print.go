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
	Inspect(node, print)
}

func print(node Node) bool {
	switch n := node.(type) {
	case *BasicLit:
		fmt.Println("BasicLit:", n.ValuePos, n.Kind, n.Value)
	case *ParenExpr:
		fmt.Println("ParenExpr:")
	case *CallExpr:
		fmt.Println("CallExpr:", n.Fun)
	case *UnaryExpr:
		fmt.Println("UnaryExpr:", n.OpPos, n.Op)
	case *BinaryExpr:
		fmt.Println("BinaryExpr:", n.OpPos, n.Op)
	case *File:
		fmt.Println("File:")
	default:
		fmt.Println("dunno what I got...that can't be good")
		fmt.Println(n)
	}
	return true
}
