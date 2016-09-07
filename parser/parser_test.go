// Copyright 2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package parser

import (
	"github.com/ev3dev/lmsasm/ast"
	"github.com/ev3dev/lmsasm/token"
	"testing"
)

func parseSnippit(code string) (file *ast.File, err error) {
	return ParseFile(token.NewFileSet(), "", code, 0)
}

func TestDefine(t *testing.T) {
	// a valid define declaration
	src := "define x 1"
	f, err := parseSnippit(src)
	if err == nil {
		if len(f.Decls) != 1 {
			t.Errorf("parseSnippit(%q): incorrect number of Decls (%d)", src, len(f.Decls))
		}
		d := f.Decls[0].(*ast.GenDecl)
		if d == nil {
			t.Errorf("parseSnippit(%q): expecting ast.GenDecl but got %T", src, f.Decls[0])
		} else {
			if d.Tok != token.DEFINE {
				t.Errorf("parseSnippit(%q): expecting token.DEFINE but got %v", src, d.Tok.String())
			}
			s := d.Spec.(*ast.DefineSpec)
			if s == nil {
				t.Errorf("parseSnippit(%q): expecting ast.DefineSpec but got %T", src, d.Spec)
			} else {
				if s.Name.Name != "x" {
					t.Errorf("parseSnippit(%q): expecting s.Name == x but got %v", src, s.Name)
				}
				v := s.Value.(*ast.BasicLit)
				if v == nil {
					t.Errorf("parseSnippit(%q): expecting ast.BasicLit but got %T", src, s.Value)
				} else {
					if v.Kind != token.INT {
						t.Errorf("parseSnippit(%q): expecting token.INT but got %v", src, v.Kind)
					}
					if v.Value != "1" {
						t.Errorf("parseSnippit(%q): expecting 1 but got %v", src, v.Value)
					}
				}
			}
		}
	} else {
		t.Errorf("parseSnippit(%q): %v", src, err)
	}
}

func TestDefineWithParen(t *testing.T) {
	src := "define x (y + 1)"
	f, err := parseSnippit(src)
	if err == nil {
		if len(f.Decls) != 1 {
			t.Errorf("parseSnippit(%q): incorrect number of Decls (%d)", src, len(f.Decls))
		}
		d := f.Decls[0].(*ast.GenDecl)
		if d == nil {
			t.Errorf("parseSnippit(%q): expecting ast.GenDecl but got %T", src, f.Decls[0])
		} else {
			if d.Tok != token.DEFINE {
				t.Errorf("parseSnippit(%q): expecting token.DEFINE but got %v", src, d.Tok)
			}
			s := d.Spec.(*ast.DefineSpec)
			if s == nil {
				t.Errorf("parseSnippit(%q): expecting ast.DefineSpec but got %T", src, d.Spec)
			} else {
				if s.Name.Name != "x" {
					t.Errorf("parseSnippit(%q): expecting 'x' but got '%v'", src, s.Name)
				}
				v := s.Value.(*ast.ParenExpr)
				if v == nil {
					t.Errorf("parseSnippit(%q): expecting ast.ParenExpr but got %T", src, s.Value)
				} else {
					b := v.X.(*ast.BinaryExpr)
					if b == nil {
						t.Errorf("parseSnippit(%q): expecting ast.BinaryExpr but got %T", src, v.X)
					} else {
						x := b.X.(*ast.Ident)
						if x == nil {
							t.Errorf("parseSnippit(%q): expecting ast.Ident but got %T", src, b.X)
						} else {
							if x.Name != "y" {
								t.Errorf("parseSnippit(%q): expecting 'y' but got '%v'", src, x.Name)
							}
						}
						if b.Op != token.ADD {
							t.Errorf("parseSnippit(%q): expecting '+' but got '%v'", src, b.Op)
						}
						y := b.Y.(*ast.BasicLit)
						if y == nil {
							t.Errorf("parseSnippit(%q): expecting ast.BasicLit but got %T", src, b.Y)
						} else {
							if y.Kind != token.INT {
								t.Errorf("parseSnippit(%q): expecting token.INT but got %v", src, y.Kind)
							}
							if y.Value != "1" {
								t.Errorf("parseSnippit(%q): expecting 1 but got %v", src, y.Value)
							}
						}
					}
				}
			}
		}
	} else {
		t.Errorf("parseSnippit(%q): %v", src, err)
	}
}

func TestVariable(t *testing.T) {
	src := "DATA8 x"
	f, err := parseSnippit(src)
	if err == nil {
		if len(f.Decls) != 1 {
			t.Errorf("parseSnippit(%q): incorrect number of Decls (%d)", src, len(f.Decls))
		}
		d := f.Decls[0].(*ast.GenDecl)
		if d == nil {
			t.Errorf("parseSnippit(%q): expecting ast.GenDecl but got %T", src, f.Decls[0])
		} else {
			if d.Tok != token.DATATYPE {
				t.Errorf("parseSnippit(%q): expecting token.DATATYPE but got %v", src, d.Tok.String())
			}
			s := d.Spec.(*ast.ValueSpec)
			if s == nil {
				t.Errorf("parseSnippit(%q): expecting ast.ValueSpec but got %T", src, d.Spec)
			} else {
				if s.Type != "DATA8" {
					t.Errorf("parseSnippit(%q): expecting DATA8 but got %v", src, s.Type)
				}
				if s.Name.Name != "x" {
					t.Errorf("parseSnippit(%q): expecting x but got %v", src, s.Name)
				}
				if s.Length != nil {
					t.Errorf("parseSnippit(%q): expecting nil but got %v", src, s.Length)
				}
			}
		}
	} else {
		t.Errorf("parseSnippit(%q): %v", src, err)
	}
}

func TestVariableWithLength(t *testing.T) {
	src := "DATAS x 10"
	f, err := parseSnippit(src)
	if err == nil {
		if len(f.Decls) != 1 {
			t.Errorf("parseSnippit(%q): incorrect number of Decls (%d)", src, len(f.Decls))
		}
		d := f.Decls[0].(*ast.GenDecl)
		if d == nil {
			t.Errorf("parseSnippit(%q): expecting ast.GenDecl but got %T", src, f.Decls[0])
		} else {
			if d.Tok != token.DATATYPE {
				t.Errorf("parseSnippit(%q): expecting token.DATATYPE but got %v", src, d.Tok.String())
			}
			s := d.Spec.(*ast.ValueSpec)
			if s == nil {
				t.Errorf("parseSnippit(%q): expecting ast.ValueSpec but got %T", src, d.Spec)
			} else {
				if s.Type != "DATAS" {
					t.Errorf("parseSnippit(%q): expecting DATAS but got %v", src, s.Type)
				}
				if s.Name.Name != "x" {
					t.Errorf("parseSnippit(%q): expecting x but got %v", src, s.Name)
				}
				l := s.Length.(*ast.BasicLit)
				if l == nil {
					t.Errorf("parseSnippit(%q): expecting ast.BasicLit but got %T", src, s.Length)
				} else {
					if l.Kind != token.INT {
						t.Errorf("parseSnippit(%q): expecting token.INT but got %v", src, l.Kind)
					}
					if l.Value != "10" {
						t.Errorf("parseSnippit(%q): expecting 10 but got %v", src, l.Value)
					}
				}
			}
		}
	} else {
		t.Errorf("parseSnippit(%q): %v", src, err)
	}
}

func TestParam(t *testing.T) {
	src := "IN_8 x"
	f, err := parseSnippit(src)
	if err == nil {
		if len(f.Decls) != 1 {
			t.Errorf("parseSnippit(%q): incorrect number of Decls (%d)", src, len(f.Decls))
		}
		d := f.Decls[0].(*ast.GenDecl)
		if d == nil {
			t.Errorf("parseSnippit(%q): expecting ast.GenDecl but got %T", src, f.Decls[0])
		} else {
			if d.Tok != token.PARAMTYPE {
				t.Errorf("parseSnippit(%q): expecting token.PARAMTYPE but got %v", src, d.Tok.String())
			}
			s := d.Spec.(*ast.ParamSpec)
			if s == nil {
				t.Errorf("parseSnippit(%q): expecting ast.ParamSpec but got %T", src, d.Spec)
			} else {
				if s.Type != "IN_8" {
					t.Errorf("parseSnippit(%q): expecting IN_8 but got %v", src, s.Type)
				}
				if s.Name.Name != "x" {
					t.Errorf("parseSnippit(%q): expecting x but got %v", src, s.Name)
				}
				if s.Length != nil {
					t.Errorf("parseSnippit(%q): expecting nil but got %v", src, s.Length)
				}
			}
		}
	} else {
		t.Errorf("parseSnippit(%q): %v", src, err)
	}
}

func TestParamWithLength(t *testing.T) {
	src := "IN_S x 10"
	f, err := parseSnippit(src)
	if err == nil {
		if len(f.Decls) != 1 {
			t.Errorf("parseSnippit(%q): incorrect number of Decls (%d)", src, len(f.Decls))
		}
		d := f.Decls[0].(*ast.GenDecl)
		if d == nil {
			t.Errorf("parseSnippit(%q): expecting ast.GenDecl but got %T", src, f.Decls[0])
		} else {
			if d.Tok != token.PARAMTYPE {
				t.Errorf("parseSnippit(%q): expecting token.PARAMTYPE but got %v", src, d.Tok.String())
			}
			s := d.Spec.(*ast.ParamSpec)
			if s == nil {
				t.Errorf("parseSnippit(%q): expecting ast.ParamSpec but got %T", src, d.Spec)
			} else {
				if s.Type != "IN_S" {
					t.Errorf("parseSnippit(%q): expecting IN_S but got %v", src, s.Type)
				}
				if s.Name.Name != "x" {
					t.Errorf("parseSnippit(%q): expecting x but got %v", src, s.Name)
				}
				l := s.Length.(*ast.BasicLit)
				if l == nil {
					t.Errorf("parseSnippit(%q): expecting ast.BasicLit but got %T", src, s.Length)
				} else {
					if l.Kind != token.INT {
						t.Errorf("parseSnippit(%q): expecting token.INT but got %v", src, l.Kind)
					}
					if l.Value != "10" {
						t.Errorf("parseSnippit(%q): expecting 10 but got %v", src, l.Value)
					}
				}
			}
		}
	} else {
		t.Errorf("parseSnippit(%q): %v", src, err)
	}
}
