// Copyright 2016 David Lechner <david@lechnology.com>
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package bytecodes

import (
	"testing"
)

func TestEv3(t *testing.T) {
	ev3, err := GetDefs("ev3")
	if err != nil {
		t.Errorf("Error getting ev3 defs: %v", err)
		return
	}

	if len(ev3.Enums) == 0 {
		t.Errorf("No Enums")
	}

	for k, v := range ev3.Enums {
		if len(v.Members) == 0 {
			t.Errorf("Enum '%v' has no members", k)
		}
	}

	if len(ev3.Ops) == 0 {
		t.Errorf("No Ops")
	}

	for k, v := range ev3.Ops {
		// make sure value: is not ommitted
		if v.Value == 0 && k != "ERROR" {
			t.Errorf("Bad value '%v' for opcode '%v'", v.Value, k)
		}

		// check the params
		for i, p := range v.Params {
			if p.Name == "" {
				t.Errorf("Missing 'name:' for param %v in opcode '%v'", i, k)
			}
			switch p.Type {
			case ParamTypeNumberParams, ParamTypeLabel, ParamTypeValues,
				ParamTypeInt8, ParamTypeInt16, ParamTypeInt32, ParamTypeFloat,
				ParamTypeString, ParamTypeVariable:
				// we're good
			case ParamTypeSubparam:
				if p.Commands == nil {
					t.Errorf("Missing 'commands:' in opcode '%v', param '%v'", k, p.Name)
				} else {
					for sk, sv := range p.Commands {
						for si, sp := range sv.Params {
							if sp.Name == "" {
								t.Errorf("Missing 'name:' for command '%v' param %v in opcode '%v'", sk, si, k)
							}
							switch sp.Type {
							case ParamTypeNumberParams, ParamTypeLabel, ParamTypeValues,
								ParamTypeInt8, ParamTypeInt16, ParamTypeInt32, ParamTypeFloat,
								ParamTypeString, ParamTypeVariable:
								// we're good
							case ParamTypeSubparam:
								// nested subparam not allowed
								fallthrough
							default:
								t.Errorf("Bad param type '%v' for command '%v' in opcode '%v', param '%v", sp.Type, sk, k, sp.Name)
							}
							switch sp.Dir  {
							case DirectionIn, DirectionOut, DirectionInOut:
								// OK
							case "":
								t.Errorf("Missing 'dir:' for param %v for command %v in opcode %v", si, sk, k)
							default:
								t.Errorf("Bad 'dir: %v' for param %v for command %v in opcode %v", sp.Dir, si, sk, k)
							}
						}
					}
				}
				}
			default:
				t.Errorf("Bad param type '%v' for opcode '%v', param '%v", p.Type, k, p.Name)
			}
			switch p.Dir  {
			case DirectionIn, DirectionOut, DirectionInOut:
				// OK
			case "":
				t.Errorf("Missing 'dir:' for param %v in opcode %v", i, k)
			default:
				t.Errorf("Bad 'dir: %v' for param %v in opcode %v", p.Dir, i, k)
			}
		}
	}
}
