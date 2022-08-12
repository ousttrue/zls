# zls

This is an experimental modified version

## TODO

* [x] cursor position => token => ast node
* [x] @import completion (zig_files, std, user_pkg...etc)
* [x] @cImport completion (use --verbose-cimport)
* [x] goto: .enum_literal
* [ ] completion: add keywords to global completion(undefined, unreachable... etc)
* [x] completion: container field completion (trigger .)
* [ ] hover: show local reference count for no pub decl(remove unused import)
* [ ] hover: show global reference count for pub decl
* [x] signature: not builtin
* [x] field_access
* [ ] package: zigmod
* [ ] package: gyro

## MEMO

* std.zig.Ast
* std.zig.Ast.Node と std.zig.Token の対応表
* std.zig.Ast.Node の親ノード表
* build.zig は root にひとつに決め打ち。簡略化する


`bytePosition` => `std.zig.Token` => `std.zig.Ast.Node` と情報を得る。


```
+-------+ +---+
|astutil| |lsp|
+-------+ +---+
    A       A
+---------------+
|language_server|
+---------------+
```
