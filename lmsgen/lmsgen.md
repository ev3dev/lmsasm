% LMSGEN(1) | User's Manual
% David Lechner
% December 2016

# NAME

lmsgen - LEGO MINDSTORMS bytecode code generator

# SYNOPSIS

lmsgen [--in *in-file*] [--out *out-file*]

# DESCRIPTION

Generates code from a template using LEGO MINDSTORMS bytecode information.

# OPTIONS

--in *in-file*
: The input template file name. Default is "-", which will cause the program to
use STDIN. See **TEMPLATE FILE** below.

--out *out-file*
: The name of the generated output file. The default is "-", which will cause
the program to use STDOUT.

# TEMPLATE FILE

The input file is a template for the generated code that uses Go's text/template
format. The data passed to the template is the bytecode definitions from ev3.yml.

There are also some helper functions provided in addition to the built-in functions:

* **isString** Can be used to test if a member of `.Defines` is a string.
* **official** Can be passed to `$x.Support.Check` to test if an item is
  supported in the official LEGO firmware.
* **xtended** Can be passed to `$x.Support.Check` to test if an item is
  supported in the RobotC/LabView firmware.
* **compat** Can be passed to `$x.Support.Check` to test if an item is
  supported in the ev3dev lms2012-compat VM.

Here is an example that prints C/C++ `#define`s from the data.

    {{range $k, $v := .Defines}}
    {{- if $v.Support.Check official}}
    #define vm{{$k}} {{if isString $v.Value -}}
        "{{$v.Value}}"
    {{- else -}}
        ({{$v.Value}})
    {{- end -}}
    {{with $v.Desc}} // {{.}}{{end}}
    {{- end}}
    {{- end}}

For more info, see <https://golang.org/pkg/text/template>.
