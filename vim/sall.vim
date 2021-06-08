" $ cp sall.vim ~/.vim/syntax/sall.vim
" $ grep '.sall' ~/.vimrc
" autocmd BufNewFile,BufRead *.sall setlocal filetype=sall

if exists("b:current_syntax")
    finish
endif

syn match Character     "'[^']'"
syn match Comment       "#.*$"
syn match Operator      "[(){}\[\]=;,!+\-<>\$@]"
syn match SpecialChar   "'\\['n]'"
syn match Number        "\<[0-9]\+\>"
syn match Float         "\<[0-9]\+\.[0-9]\+\>"

syn region String start=+"+ skip=+\\"+ end=+"+

syn keyword Boolean
    \ true
    \ false
syn keyword Conditional
    \ if
    \ else
syn keyword Keyword
    \ fn
    \ in
    \ break
    \ continue
    \ return
syn keyword Repeat
    \ for
    \ loop
syn keyword Type
    \ i32
    \ f32
    \ bool
    \ char

let b:current_syntax = "sall"
