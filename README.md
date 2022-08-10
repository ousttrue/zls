# zls

This is an experimental modified version

## TODO

* [x] cursor position => token => ast node
* [x] @import completion (zig_files, std, user_pkg...etc)
* [ ] @cImport completion (use zig translate-c ?)
* [ ] remove workspace dependency to lsp
* [ ] goto: .enum_literal
* [ ] diagnostics: when format error
* [ ] add keywords to global completion(undefined, unreachable... etc)
* [ ] container field completion (trigger .)
* [ ] hover: show reference count
* [ ] field_access

## MEMO

* std.zig.Ast
* std.zig.Ast.Node と std.zig.Token の対応表
* std.zig.Ast.Node の親ノード表

以外は事前に情報を蓄えずに、必要に応じて情報を引き出す方向性に改変する

* Ast 処理では token / node レベルで操作する。bytePosition による処理を避ける
* build.zig は root にひとつに決め打ち。簡略化する
* completion => snippet, signature help
