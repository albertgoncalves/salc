" $ cp sall.vim ~/.vim/syntax/sall.vim
" $ grep '.sall' ~/.vimrc
" autocmd BufNewFile,BufRead *.sall setlocal filetype=sall

if exists("b:current_syntax")
    finish
endif

syn match Character     "'[^']'"
syn match Comment       "#.*$"
syn match Float         "\<[0-9]\+\.[0-9]\+\>"
syn match Function      "\<call[ \n]\+[^ \n]\+\>"
syn match Number        "\<[0-9]\+\>"
syn match Operator      "[(){}\[\]=;,!+\-<>]"
syn match SpecialChar   "'\\['n]'"
syn match String        "\"[^\"]*\"\?"

syn keyword Boolean
    \ true
    \ false
syn keyword Conditional
    \ if
syn keyword Keyword
    \ break
    \ continue
    \ return
syn keyword Repeat
    \ loop
syn keyword Type
    \ i32
    \ f32
    \ bool
    \ char

let b:current_syntax = "sall"
