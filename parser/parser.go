// Copyright 2009 The Go Authors. All rights reserved.
//           2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package parser implements a parser for lms source files. Input may be
// provided in a variety of forms (see the various Parse* functions); the
// output is an abstract syntax tree (AST) representing the lms source. The
// parser is invoked through one of the Parse* functions.
package parser

import (
	"bytes"
	"fmt"
	"github.com/ev3dev/lmsasm/ast"
	"github.com/ev3dev/lmsasm/scanner"
	"github.com/ev3dev/lmsasm/token"
)

// The parser structure holds the parser's internal state.
type parser struct {
	file    *token.File
	errors  scanner.ErrorList
	scanner scanner.Scanner

	// Tracing/debugging
	mode   Mode // parsing mode
	trace  bool // == (mode & Trace != 0)
	indent int  // indentation used for tracing output

	// Next token
	pos token.Pos   // token position
	tok token.Token // one token look-ahead
	lit string      // token literal

	// Error recovery
	// (used to limit the number of calls to syncXXX functions
	// w/o making scanning progress - avoids potential endless
	// loops across multiple parser functions during error recovery)
	syncPos token.Pos // last synchronization position
	syncCnt int       // number of calls to syncXXX without progress

	// Non-syntactic parser control
	exprLev int  // < 0: in control clause, >= 0: in expression
	inRhs   bool // if set, the parser is parsing a rhs expression

	// Ordinary identifier scopes
	pkgScope   *ast.Scope   // pkgScope.Outer == nil
	topScope   *ast.Scope   // top-most scope; may be pkgScope
	unresolved []*ast.Ident // unresolved identifiers

	// Label scopes
	// (maintained by open/close LabelScope)
	labelScope  *ast.Scope     // label scope for current function
	targetStack [][]*ast.Ident // stack of unresolved labels
}

func (p *parser) init(fset *token.FileSet, filename string, src []byte, mode Mode) {
	p.file = fset.AddFile(filename, -1, len(src))
	eh := func(pos token.Position, msg string) { p.errors.Add(pos, msg) }
	p.scanner.Init(bytes.NewBuffer(src), filename)
	p.scanner.Error = func(s *scanner.Scanner, msg string) { eh(s.Pos(), msg) }

	p.mode = mode
	p.trace = mode&Trace != 0 // for convenience (p.trace is used frequently)

	p.next()
}

// ----------------------------------------------------------------------------
// Scoping support

func (p *parser) openScope() {
	p.topScope = ast.NewScope(p.topScope)
}

func (p *parser) closeScope() {
	p.topScope = p.topScope.Outer
}

func (p *parser) openLabelScope() {
	p.labelScope = ast.NewScope(p.labelScope)
	p.targetStack = append(p.targetStack, nil)
}

func (p *parser) closeLabelScope() {
	// resolve labels
	n := len(p.targetStack) - 1
	scope := p.labelScope
	for _, ident := range p.targetStack[n] {
		ident.Obj = scope.Lookup(ident.Name)
		if ident.Obj == nil && p.mode&DeclarationErrors != 0 {
			p.error(ident.Pos(), fmt.Sprintf("label %s undefined", ident.Name))
		}
	}
	// pop label scope
	p.targetStack = p.targetStack[0:n]
	p.labelScope = p.labelScope.Outer
}

func (p *parser) declare(decl, data interface{}, scope *ast.Scope, kind ast.ObjKind, idents ...*ast.Ident) {
	for _, ident := range idents {
		assert(ident.Obj == nil, "identifier already declared or resolved")
		obj := ast.NewObj(kind, ident.Name)
		// remember the corresponding declaration for redeclaration
		// errors and global variable resolution/typechecking phase
		obj.Decl = decl
		obj.Data = data
		ident.Obj = obj
		if ident.Name != "_" {
			if alt := scope.Insert(obj); alt != nil && p.mode&DeclarationErrors != 0 {
				prevDecl := ""
				if pos := alt.Pos(); pos.IsValid() {
					prevDecl = fmt.Sprintf("\n\tprevious declaration at %s", p.file.Position(pos))
				}
				p.error(ident.Pos(), fmt.Sprintf("%s redeclared in this block%s", ident.Name, prevDecl))
			}
		}
	}
}

// The unresolved object is a sentinel to mark identifiers that have been added
// to the list of unresolved identifiers. The sentinel is only used for verifying
// internal consistency.
var unresolved = new(ast.Object)

// If x is an identifier, tryResolve attempts to resolve x by looking up
// the object it denotes. If no object is found and collectUnresolved is
// set, x is marked as unresolved and collected in the list of unresolved
// identifiers.
//
func (p *parser) tryResolve(x ast.Expr, collectUnresolved bool) {
	// nothing to do if x is not an identifier or the blank identifier
	ident, _ := x.(*ast.Ident)
	if ident == nil {
		return
	}
	assert(ident.Obj == nil, "identifier already declared or resolved")
	if ident.Name == "_" {
		return
	}
	// try to resolve the identifier
	for s := p.topScope; s != nil; s = s.Outer {
		if obj := s.Lookup(ident.Name); obj != nil {
			ident.Obj = obj
			return
		}
	}
	// all local scopes are known, so any unresolved identifier
	// must be found either in the file scope, package scope
	// (perhaps in another file), or universe scope --- collect
	// them so that they can be resolved later
	if collectUnresolved {
		ident.Obj = unresolved
		p.unresolved = append(p.unresolved, ident)
	}
}

func (p *parser) resolve(x ast.Expr) {
	p.tryResolve(x, true)
}

// ----------------------------------------------------------------------------
// Parsing support

func (p *parser) printTrace(a ...interface{}) {
	const dots = ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
	const n = len(dots)
	pos := p.file.Position(p.pos)
	fmt.Printf("%5d:%3d: ", pos.Line, pos.Column)
	i := 2 * p.indent
	for i > n {
		fmt.Print(dots)
		i -= n
	}
	// i <= n
	fmt.Print(dots[0:i])
	fmt.Println(a...)
}

func trace(p *parser, msg string) *parser {
	p.printTrace(msg, "(")
	p.indent++
	return p
}

// Usage pattern: defer un(trace(p, "..."))
func un(p *parser) {
	p.indent--
	p.printTrace(")")
}

// Advance to the next token.
func (p *parser) next0() {
	// Because of one-token look-ahead, print the previous token
	// when tracing as it provides a more readable output. The
	// very first token (!p.pos.IsValid()) is not initialized
	// (it is token.ILLEGAL), so don't print it .
	if p.trace && p.pos.IsValid() {
		s := p.tok.String()
		switch {
		case p.tok.IsLiteral():
			p.printTrace(s, p.lit)
		case p.tok.IsOperator(), p.tok.IsKeyword():
			p.printTrace("\"" + s + "\"")
		default:
			p.printTrace(s)
		}
	}

	p.tok = p.scanner.Scan()
	p.pos = p.file.Pos(p.scanner.Pos().Offset)
	p.lit = p.scanner.TokenText()
}

// Advance to the next token.
func (p *parser) next() {
	p.next0()
}

// A bailout panic is raised to indicate early termination.
type bailout struct{}

func (p *parser) error(pos token.Pos, msg string) {
	epos := p.file.Position(pos)

	// If AllErrors is not set, discard errors reported on the same line
	// as the last recorded error and stop parsing if there are more than
	// 10 errors.
	if p.mode&AllErrors == 0 {
		n := len(p.errors)
		if n > 0 && p.errors[n-1].Pos.Line == epos.Line {
			return // discard - likely a spurious error
		}
		if n > 10 {
			panic(bailout{})
		}
	}

	p.errors.Add(epos, msg)
}

func (p *parser) errorExpected(pos token.Pos, msg string) {
	msg = "expected " + msg
	if pos == p.pos {
		// the error happened at the current position;
		// make the error message more specific
		if p.tok == token.SEMICOLON && p.lit == "\n" {
			msg += ", found newline"
		} else {
			msg += ", found '" + p.tok.String() + "'"
			if p.tok.IsLiteral() {
				msg += " " + p.lit
			}
		}
	}
	p.error(pos, msg)
}

func (p *parser) expect(tok token.Token) token.Pos {
	pos := p.pos
	if p.tok != tok {
		p.errorExpected(pos, "'"+tok.String()+"'")
	}
	p.next() // make progress
	return pos
}

// expectClosing is like expect but provides a better error message
// for the common case of a missing comma before a newline.
//
func (p *parser) expectClosing(tok token.Token, context string) token.Pos {
	if p.tok != tok && p.tok == token.SEMICOLON && p.lit == "\n" {
		p.error(p.pos, "missing ',' before newline in "+context)
		p.next()
	}
	return p.expect(tok)
}

func (p *parser) expectSemi() {
	// semicolon is optional before a closing ')' or '}'
	if p.tok != token.RPAREN && p.tok != token.RBRACE {
		switch p.tok {
		case token.COMMA:
			// permit a ',' instead of a ';' but complain
			p.errorExpected(p.pos, "';'")
			fallthrough
		case token.SEMICOLON:
			p.next()
		default:
			p.errorExpected(p.pos, "';'")
			syncStmt(p)
		}
	}
}

func (p *parser) atComma(context string, follow token.Token) bool {
	if p.tok == token.COMMA {
		return true
	}
	if p.tok != follow {
		msg := "missing ','"
		if p.tok == token.SEMICOLON && p.lit == "\n" {
			msg += " before newline"
		}
		p.error(p.pos, msg+" in "+context)
		return true // "insert" comma and continue
	}
	return false
}

func assert(cond bool, msg string) {
	if !cond {
		panic("go/parser internal error: " + msg)
	}
}

// syncStmt advances to the next statement.
// Used for synchronization after an error.
//
func syncStmt(p *parser) {
	for {
		switch p.tok {
		// case token.BREAK, token.CONST, token.CONTINUE, token.DEFER,
		// 	token.FALLTHROUGH, token.FOR, token.GO, token.GOTO,
		// 	token.IF, token.RETURN, token.SELECT, token.SWITCH,
		// 	token.TYPE, token.VAR:
		// 	// Return only if parser made some progress since last
		// 	// sync or if it has not reached 10 sync calls without
		// 	// progress. Otherwise consume at least one token to
		// 	// avoid an endless parser loop (it is possible that
		// 	// both parseOperand and parseStmt call syncStmt and
		// 	// correctly do not advance, thus the need for the
		// 	// invocation limit p.syncCnt).
		// 	if p.pos == p.syncPos && p.syncCnt < 10 {
		// 		p.syncCnt++
		// 		return
		// 	}
		// 	if p.pos > p.syncPos {
		// 		p.syncPos = p.pos
		// 		p.syncCnt = 0
		// 		return
		// 	}
		// 	// Reaching here indicates a parser bug, likely an
		// 	// incorrect token list in this function, but it only
		// 	// leads to skipping of possibly correct code if a
		// 	// previous error is present, and thus is preferred
		// 	// over a non-terminating parse.
		case token.EOF:
			return
		}
		p.next()
	}
}

// syncDecl advances to the next declaration.
// Used for synchronization after an error.
//
func syncDecl(p *parser) {
	for {
		switch p.tok {
		// case token.CONST, token.TYPE, token.VAR:
		// 	// see comments in syncStmt
		// 	if p.pos == p.syncPos && p.syncCnt < 10 {
		// 		p.syncCnt++
		// 		return
		// 	}
		// 	if p.pos > p.syncPos {
		// 		p.syncPos = p.pos
		// 		p.syncCnt = 0
		// 		return
		// 	}
		case token.EOF:
			return
		}
		p.next()
	}
}

// safePos returns a valid file position for a given position: If pos
// is valid to begin with, safePos returns pos. If pos is out-of-range,
// safePos returns the EOF position.
//
// This is hack to work around "artificial" end positions in the AST which
// are computed by adding 1 to (presumably valid) token positions. If the
// token positions are invalid due to parse errors, the resulting end position
// may be past the file's EOF position, which would lead to panics if used
// later on.
//
func (p *parser) safePos(pos token.Pos) (res token.Pos) {
	defer func() {
		if recover() != nil {
			res = token.Pos(p.file.Base() + p.file.Size()) // EOF position
		}
	}()
	_ = p.file.Offset(pos) // trigger a panic if position is out-of-range
	return pos
}

// ----------------------------------------------------------------------------
// Identifiers

func (p *parser) parseIdent() *ast.Ident {
	pos := p.pos
	name := "_"
	if p.tok == token.IDENT {
		name = p.lit
		p.next()
	} else {
		p.expect(token.IDENT) // use expect() error handling
	}
	return &ast.Ident{NamePos: pos, Name: name}
}

// func (p *parser) parseIdentList() (list []*ast.Ident) {
// 	if p.trace {
// 		defer un(trace(p, "IdentList"))
// 	}

// 	list = append(list, p.parseIdent())
// 	for p.tok == token.COMMA {
// 		p.next()
// 		list = append(list, p.parseIdent())
// 	}

// 	return
// }

// ----------------------------------------------------------------------------
// Common productions

// If lhs is set, result list elements which are identifiers are not resolved.
// func (p *parser) parseExprList(lhs bool) (list []ast.Expr) {
// 	if p.trace {
// 		defer un(trace(p, "ExpressionList"))
// 	}

// 	list = append(list, p.checkExpr(p.parseExpr(lhs)))
// 	for p.tok == token.COMMA {
// 		p.next()
// 		list = append(list, p.checkExpr(p.parseExpr(lhs)))
// 	}

// 	return
// }

// func (p *parser) parseLhsList() []ast.Expr {
// 	old := p.inRhs
// 	p.inRhs = false
// 	list := p.parseExprList(true)
// 	switch p.tok {
// 	case token.DEFINE:
// 		// lhs of a short variable declaration
// 		// but doesn't enter scope until later:
// 		// caller must call p.shortVarDecl(p.makeIdentList(list))
// 		// at appropriate time.
// 	case token.COLON:
// 		// lhs of a label declaration or a communication clause of a select
// 		// statement (parseLhsList is not called when parsing the case clause
// 		// of a switch statement):
// 		// - labels are declared by the caller of parseLhsList
// 		// - for communication clauses, if there is a stand-alone identifier
// 		//   followed by a colon, we have a syntax error; there is no need
// 		//   to resolve the identifier in that case
// 	default:
// 		// identifiers must be declared elsewhere
// 		for _, x := range list {
// 			p.resolve(x)
// 		}
// 	}
// 	p.inRhs = old
// 	return list
// }

// func (p *parser) parseRhsList() []ast.Expr {
// 	old := p.inRhs
// 	p.inRhs = true
// 	list := p.parseExprList(false)
// 	p.inRhs = old
// 	return list
// }

// ----------------------------------------------------------------------------
// Types

func (p *parser) parseType() ast.Expr {
	if p.trace {
		defer un(trace(p, "Type"))
	}

	typ := p.tryType()

	if typ == nil {
		pos := p.pos
		p.errorExpected(pos, "type")
		p.next() // make progress
		return &ast.BadExpr{From: pos, To: p.pos}
	}

	return typ
}

// // If the result is an identifier, it is not resolved.
func (p *parser) parseTypeName() ast.Expr {
	if p.trace {
		defer un(trace(p, "TypeName"))
	}

	ident := p.parseIdent()
	// don't resolve ident yet - it may be a parameter or field name

	return ident
}

// func (p *parser) makeIdentList(list []ast.Expr) []*ast.Ident {
// 	idents := make([]*ast.Ident, len(list))
// 	for i, x := range list {
// 		ident, isIdent := x.(*ast.Ident)
// 		if !isIdent {
// 			if _, isBad := x.(*ast.BadExpr); !isBad {
// 				// only report error if it's a new one
// 				p.errorExpected(x.Pos(), "identifier")
// 			}
// 			ident = &ast.Ident{NamePos: x.Pos(), Name: "_"}
// 		}
// 		idents[i] = ident
// 	}
// 	return idents
// }

// // If the result is an identifier, it is not resolved.
// func (p *parser) tryVarType(isParam bool) ast.Expr {
// 	return p.tryIdentOrType()
// }

// // If the result is an identifier, it is not resolved.
// func (p *parser) parseVarType(isParam bool) ast.Expr {
// 	typ := p.tryVarType(isParam)
// 	if typ == nil {
// 		pos := p.pos
// 		p.errorExpected(pos, "type")
// 		p.next() // make progress
// 		typ = &ast.BadExpr{From: pos, To: p.pos}
// 	}
// 	return typ
// }

// func (p *parser) parseParameterList(scope *ast.Scope, ellipsisOk bool) (params []*ast.Field) {
// 	if p.trace {
// 		defer un(trace(p, "ParameterList"))
// 	}

// 	// 1st ParameterDecl
// 	// A list of identifiers looks like a list of type names.
// 	var list []ast.Expr
// 	for {
// 		list = append(list, p.parseVarType(ellipsisOk))
// 		if p.tok != token.COMMA {
// 			break
// 		}
// 		p.next()
// 		if p.tok == token.RPAREN {
// 			break
// 		}
// 	}

// 	// analyze case
// 	if typ := p.tryVarType(ellipsisOk); typ != nil {
// 		// IdentifierList Type
// 		idents := p.makeIdentList(list)
// 		field := &ast.Field{Names: idents, Type: typ}
// 		params = append(params, field)
// 		// Go spec: The scope of an identifier denoting a function
// 		// parameter or result variable is the function body.
// 		p.declare(field, nil, scope, ast.Var, idents...)
// 		p.resolve(typ)
// 		if !p.atComma("parameter list", token.RPAREN) {
// 			return
// 		}
// 		p.next()
// 		for p.tok != token.RPAREN && p.tok != token.EOF {
// 			idents := p.parseIdentList()
// 			typ := p.parseVarType(ellipsisOk)
// 			field := &ast.Field{Names: idents, Type: typ}
// 			params = append(params, field)
// 			// Go spec: The scope of an identifier denoting a function
// 			// parameter or result variable is the function body.
// 			p.declare(field, nil, scope, ast.Var, idents...)
// 			p.resolve(typ)
// 			if !p.atComma("parameter list", token.RPAREN) {
// 				break
// 			}
// 			p.next()
// 		}
// 		return
// 	}

// 	// Type { "," Type } (anonymous parameters)
// 	params = make([]*ast.Field, len(list))
// 	for i, typ := range list {
// 		p.resolve(typ)
// 		params[i] = &ast.Field{Type: typ}
// 	}
// 	return
// }

// func (p *parser) parseFuncType() (*ast.FuncType, *ast.Scope) {
// 	if p.trace {
// 		defer un(trace(p, "FuncType"))
// 	}

// 	pos := p.expect(token.FUNC)
// 	scope := ast.NewScope(p.topScope) // function scope
// 	params, results := p.parseSignature(scope)

// 	return &ast.FuncType{Func: pos, Params: params, Results: results}, scope
// }

// func (p *parser) parseMethodSpec(scope *ast.Scope) *ast.Field {
// 	if p.trace {
// 		defer un(trace(p, "MethodSpec"))
// 	}

// 	var idents []*ast.Ident
// 	var typ ast.Expr
// 	x := p.parseTypeName()
// 	if ident, isIdent := x.(*ast.Ident); isIdent && p.tok == token.LPAREN {
// 		// method
// 		idents = []*ast.Ident{ident}
// 		scope := ast.NewScope(nil) // method scope
// 		params, results := p.parseSignature(scope)
// 		typ = &ast.FuncType{Func: token.NoPos, Params: params, Results: results}
// 	} else {
// 		// embedded interface
// 		typ = x
// 		p.resolve(typ)
// 	}
// 	p.expectSemi() // call before accessing p.linecomment

// 	spec := &ast.Field{Names: idents, Type: typ, Comment: p.lineComment}
// 	p.declare(spec, nil, scope, ast.Fun, idents...)

// 	return spec
// }

// If the result is an identifier, it is not resolved.
func (p *parser) tryIdentOrType() ast.Expr {
	switch p.tok {
	case token.IDENT:
		return p.parseTypeName()
	case token.LPAREN:
		lparen := p.pos
		p.next()
		typ := p.parseType()
		rparen := p.expect(token.RPAREN)
		return &ast.ParenExpr{Lparen: lparen, X: typ, Rparen: rparen}
	}

	// no type found
	return nil
}

func (p *parser) tryType() ast.Expr {
	typ := p.tryIdentOrType()
	if typ != nil {
		p.resolve(typ)
	}
	return typ
}

// ----------------------------------------------------------------------------
// Blocks

// func (p *parser) parseStmtList() (list []ast.Stmt) {
// 	if p.trace {
// 		defer un(trace(p, "StatementList"))
// 	}

// 	for p.tok != token.EOF {
// 		list = append(list, p.parseStmt())
// 	}

// 	return
// }

// func (p *parser) parseBody(scope *ast.Scope) []ast.Stmt {
// 	if p.trace {
// 		defer un(trace(p, "Body"))
// 	}

// 	_ = p.expect(token.LBRACE)
// 	p.topScope = scope // open function scope
// 	p.openLabelScope()
// 	list := p.parseStmtList()
// 	p.closeLabelScope()
// 	p.closeScope()
// 	_ = p.expect(token.RBRACE)

// 	return list
// }

// ----------------------------------------------------------------------------
// Expressions

// parseExpr may return an expression or a raw type.
//
func (p *parser) parseExpr() ast.Expr {
	if p.trace {
		defer un(trace(p, "Expr"))
	}

	switch p.tok {
	case token.IDENT:
		x := p.parseIdent()
		//p.resolve(x)
		return x

	case token.INT, token.FLOAT, token.STRING:
		x := &ast.BasicLit{ValuePos: p.pos, Kind: p.tok, Value: p.lit}
		p.next()
		return x

	case token.LPAREN:
		lparen := p.pos
		p.next()
		p.exprLev++
		x := p.parseBinaryExpr()
		p.exprLev--
		rparen := p.expect(token.RPAREN)
		return &ast.ParenExpr{Lparen: lparen, X: x, Rparen: rparen}
	}

	// we have an error
	pos := p.pos
	p.errorExpected(pos, "operand")
	syncStmt(p)
	return &ast.BadExpr{From: pos, To: p.pos}
}

// checkExpr checks that x is an expression (and not a type).
func (p *parser) checkExpr(x ast.Expr) ast.Expr {
	if x == nil {
		return nil
	}
	switch unparen(x).(type) {
	case *ast.BadExpr:
	case *ast.Ident:
	case *ast.BasicLit:
	case *ast.ParenExpr:
		panic("unreachable")
	case *ast.CallExpr:
	case *ast.UnaryExpr:
	case *ast.BinaryExpr:
	default:
		// all other nodes are not proper expressions
		p.errorExpected(x.Pos(), "expression")
		x = &ast.BadExpr{From: x.Pos(), To: p.safePos(x.End())}
	}
	return x
}

// isTypeName reports whether x is a (qualified) TypeName.
func isTypeName(x ast.Expr) bool {
	switch x.(type) {
	case *ast.BadExpr:
	case *ast.Ident:
	default:
		return false // all other nodes are not type names
	}
	return true
}

// isLiteralType reports whether x is a legal composite literal type.
func isLiteralType(x ast.Expr) bool {
	switch x.(type) {
	case *ast.BadExpr:
	case *ast.Ident:
	default:
		return false // all other nodes are not legal composite literal types
	}
	return true
}

// If x is of the form (T), unparen returns unparen(T), otherwise it returns x.
func unparen(x ast.Expr) ast.Expr {
	if p, isParen := x.(*ast.ParenExpr); isParen {
		x = unparen(p.X)
	}
	return x
}

func (p *parser) parseUnaryExpr() ast.Expr {
	if p.trace {
		defer un(trace(p, "UnaryExpr"))
	}

	switch p.tok {
	case token.ADD, token.SUB:
		pos, op := p.pos, p.tok
		p.next()
		x := p.parseUnaryExpr()
		return &ast.UnaryExpr{OpPos: pos, Op: op, X: p.checkExpr(x)}
	}

	return p.parseExpr()
}

func (p *parser) tokPrec() (token.Token, int) {
	tok := p.tok
	return tok, tok.Precedence()
}

func (p *parser) parseBinaryExpr() ast.Expr {
	if p.trace {
		defer un(trace(p, "BinaryExpr"))
	}

	x := p.parseUnaryExpr()
	for {
		op, oprec := p.tokPrec()
		if oprec < token.LowestPrec+1 {
			return x
		}
		pos := p.expect(op)
		//p.resolve(x)
		y := p.parseUnaryExpr()
		x = &ast.BinaryExpr{X: p.checkExpr(x), OpPos: pos, Op: op, Y: p.checkExpr(y)}
	}
}

// ----------------------------------------------------------------------------
// Statements

// // Parsing modes for parseSimpleStmt.
// const (
// 	basic = iota
// 	labelOk
// 	rangeOk
// )

// // parseSimpleStmt returns true as 2nd result if it parsed the assignment
// // of a range clause (with mode == rangeOk). The returned statement is an
// // assignment with a right-hand side that is a single unary expression of
// // the form "range x". No guarantees are given for the left-hand side.
// func (p *parser) parseSimpleStmt(mode int) (ast.Stmt, bool) {
// 	if p.trace {
// 		defer un(trace(p, "SimpleStmt"))
// 	}

// 	x := p.parseLhsList()

// 	switch p.tok {
// 	case
// 		token.DEFINE, token.ASSIGN, token.ADD_ASSIGN,
// 		token.SUB_ASSIGN, token.MUL_ASSIGN, token.QUO_ASSIGN,
// 		token.REM_ASSIGN, token.AND_ASSIGN, token.OR_ASSIGN,
// 		token.XOR_ASSIGN, token.SHL_ASSIGN, token.SHR_ASSIGN, token.AND_NOT_ASSIGN:
// 		// assignment statement, possibly part of a range clause
// 		pos, tok := p.pos, p.tok
// 		p.next()
// 		var y []ast.Expr
// 		isRange := false
// 		if mode == rangeOk && p.tok == token.RANGE && (tok == token.DEFINE || tok == token.ASSIGN) {
// 			pos := p.pos
// 			p.next()
// 			y = []ast.Expr{&ast.UnaryExpr{OpPos: pos, Op: token.RANGE, X: p.parseRhs()}}
// 			isRange = true
// 		} else {
// 			y = p.parseRhsList()
// 		}
// 		as := &ast.AssignStmt{Lhs: x, TokPos: pos, Tok: tok, Rhs: y}
// 		if tok == token.DEFINE {
// 			p.shortVarDecl(as, x)
// 		}
// 		return as, isRange
// 	}

// 	if len(x) > 1 {
// 		p.errorExpected(x[0].Pos(), "1 expression")
// 		// continue with first expression
// 	}

// 	switch p.tok {
// 	case token.COLON:
// 		// labeled statement
// 		colon := p.pos
// 		p.next()
// 		if label, isIdent := x[0].(*ast.Ident); mode == labelOk && isIdent {
// 			// Go spec: The scope of a label is the body of the function
// 			// in which it is declared and excludes the body of any nested
// 			// function.
// 			stmt := &ast.LabeledStmt{Label: label, Colon: colon, Stmt: p.parseStmt()}
// 			p.declare(stmt, nil, p.labelScope, ast.Lbl, label)
// 			return stmt, false
// 		}
// 		// The label declaration typically starts at x[0].Pos(), but the label
// 		// declaration may be erroneous due to a token after that position (and
// 		// before the ':'). If SpuriousErrors is not set, the (only) error re-
// 		// ported for the line is the illegal label error instead of the token
// 		// before the ':' that caused the problem. Thus, use the (latest) colon
// 		// position for error reporting.
// 		p.error(colon, "illegal label declaration")
// 		return &ast.BadStmt{From: x[0].Pos(), To: colon + 1}, false

// 	case token.ARROW:
// 		// send statement
// 		arrow := p.pos
// 		p.next()
// 		y := p.parseRhs()
// 		return &ast.SendStmt{Chan: x[0], Arrow: arrow, Value: y}, false

// 	case token.INC, token.DEC:
// 		// increment or decrement
// 		s := &ast.IncDecStmt{X: x[0], TokPos: p.pos, Tok: p.tok}
// 		p.next()
// 		return s, false
// 	}

// 	// expression
// 	return &ast.ExprStmt{X: x[0]}, false
// }

// func (p *parser) parseCallExpr(callType string) *ast.CallExpr {
// 	x := p.parseRhsOrType() // could be a conversion: (some type)(x)
// 	if call, isCall := x.(*ast.CallExpr); isCall {
// 		return call
// 	}
// 	if _, isBad := x.(*ast.BadExpr); !isBad {
// 		// only report error if it's a new one
// 		p.error(p.safePos(x.End()), fmt.Sprintf("function must be invoked in %s statement", callType))
// 	}
// 	return nil
// }

// func (p *parser) makeExpr(s ast.Stmt, kind string) ast.Expr {
// 	if s == nil {
// 		return nil
// 	}
// 	if es, isExpr := s.(*ast.ExprStmt); isExpr {
// 		return p.checkExpr(es.X)
// 	}
// 	p.error(s.Pos(), fmt.Sprintf("expected %s, found simple statement (missing parentheses around composite literal?)", kind))
// 	return &ast.BadExpr{From: s.Pos(), To: p.safePos(s.End())}
// }

// func (p *parser) parseStmt() (s ast.Stmt) {
// 	if p.trace {
// 		defer un(trace(p, "Statement"))
// 	}

// 	switch p.tok {
// 	case token.CONST, token.TYPE, token.VAR:
// 		s = &ast.DeclStmt{Decl: p.parseDecl(syncStmt)}
// 	case
// 		// tokens that may start an expression
// 		token.IDENT, token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING, token.FUNC, token.LPAREN, // operands
// 		token.LBRACK, token.STRUCT, token.MAP, token.CHAN, token.INTERFACE, // composite types
// 		token.ADD, token.SUB, token.MUL, token.AND, token.XOR, token.ARROW, token.NOT: // unary operators
// 		s, _ = p.parseSimpleStmt(labelOk)
// 		// because of the required look-ahead, labeled statements are
// 		// parsed by parseSimpleStmt - don't expect a semicolon after
// 		// them
// 		if _, isLabeledStmt := s.(*ast.LabeledStmt); !isLabeledStmt {
// 			p.expectSemi()
// 		}
// 	case token.SEMICOLON:
// 		// Is it ever possible to have an implicit semicolon
// 		// producing an empty statement in a valid program?
// 		// (handle correctly anyway)
// 		s = &ast.EmptyStmt{Semicolon: p.pos, Implicit: p.lit == "\n"}
// 		p.next()
// 	case token.RBRACE:
// 		// a semicolon may be omitted before a closing "}"
// 		s = &ast.EmptyStmt{Semicolon: p.pos, Implicit: true}
// 	default:
// 		// no statement found
// 		pos := p.pos
// 		p.errorExpected(pos, "statement")
// 		syncStmt(p)
// 		s = &ast.BadStmt{From: pos, To: p.pos}
// 	}

// 	return
// }

// ----------------------------------------------------------------------------
// Declarations

type parseSpecFunction func(lit string) ast.Spec

func (p *parser) parseDefineSpec(_ string) ast.Spec {
	if p.trace {
		defer un(trace(p, "DefineSpec"))
	}

	ident := p.parseIdent()
	value := p.parseExpr()

	spec := &ast.DefineSpec{
		Name:  ident,
		Value: value,
	}
	p.declare(spec, nil, p.topScope, ast.Con, ident)

	return spec
}

func (p *parser) parseValueSpec(lit string) ast.Spec {
	if p.trace {
		defer un(trace(p, "ValueSpec"))
	}

	typ := lit
	ident := p.parseIdent()
	var length ast.Expr = nil
	switch typ {
	case "DATAS", "ARRAY8", "ARRAY16", "ARRAY32", "ARRAYF":
		length = p.parseExpr()
	}

	spec := &ast.ValueSpec{
		Type:   typ,
		Name:   ident,
		Length: length,
	}
	p.declare(spec, nil, p.topScope, ast.Var, ident)

	return spec
}

func (p *parser) parseParamSpec(lit string) ast.Spec {
	if p.trace {
		defer un(trace(p, "ParamSpec"))
	}

	typ := lit
	ident := p.parseIdent()
	var length ast.Expr = nil
	switch typ {
	case "IN_S", "OUT_S", "IO_S":
		length = p.parseExpr()
	}

	spec := &ast.ParamSpec{
		Type:   typ,
		Name:   ident,
		Length: length,
	}
	p.declare(spec, nil, p.topScope, ast.Var, ident)

	return spec
}

func (p *parser) parseGenDecl(tok token.Token, lit string, f parseSpecFunction) *ast.GenDecl {
	if p.trace {
		defer un(trace(p, "GenDecl("+lit+")"))
	}

	pos := p.expect(token.IDENT)
	spec := f(lit)

	return &ast.GenDecl{
		TokPos: pos,
		Tok:    tok,
		Spec:   spec,
	}
}

func (p *parser) parseObjDecl() ast.Decl {
	return nil
}

func (p *parser) parseDecl(sync func(*parser)) ast.Decl {
	if p.trace {
		defer un(trace(p, "Declaration"))
	}

	var f parseSpecFunction
	tok := token.Lookup(p.lit)
	switch tok {
	case token.DEFINE:
		f = p.parseDefineSpec

	case token.DATATYPE:
		f = p.parseValueSpec

	case token.PARAMTYPE:
		f = p.parseParamSpec

	case token.VMTHREAD, token.SUBCALL:
		return p.parseObjDecl()

	default:
		pos := p.pos
		p.errorExpected(pos, "declaration")
		sync(p)
		return &ast.BadDecl{From: pos, To: p.pos}
	}

	return p.parseGenDecl(tok, p.lit, f)
}

// ----------------------------------------------------------------------------
// Source files

func (p *parser) parseFile() *ast.File {
	if p.trace {
		defer un(trace(p, "File"))
	}

	// Don't bother parsing the rest if we had errors scanning the first token.
	// Likely not a lms source file at all.
	if p.errors.Len() != 0 {
		return nil
	}

	p.openScope()
	p.pkgScope = p.topScope
	var decls []ast.Decl

	for p.tok != token.EOF {
		decls = append(decls, p.parseDecl(syncDecl))
	}

	p.closeScope()
	assert(p.topScope == nil, "unbalanced scopes")
	assert(p.labelScope == nil, "unbalanced label scopes")

	// resolve global identifiers within the same file
	i := 0
	for _, ident := range p.unresolved {
		// i <= index for current ident
		assert(ident.Obj == unresolved, "object already resolved")
		ident.Obj = p.pkgScope.Lookup(ident.Name) // also removes unresolved sentinel
		if ident.Obj == nil {
			p.unresolved[i] = ident
			i++
		}
	}

	return &ast.File{
		Decls:      decls,
		Scope:      p.pkgScope,
		Unresolved: p.unresolved[0:i],
	}
}
