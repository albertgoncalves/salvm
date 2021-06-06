" $ cp sals.vim ~/.vim/syntax/sals.vim
" $ grep '.sals' ~/.vimrc
" autocmd BufNewFile,BufRead *.sals setlocal filetype=sals

if exists("b:current_syntax")
    finish
endif

syn match Comment   ";.*$"
syn match Delimiter "[:\-+\[\]]"
syn match Number    "\<[0-9]\+\>"
syn match Float     "\<[0-9]\+\.[0-9]\+\>"

syn region String start=+"+ skip=+\\"+ end=+"+

syn keyword Keyword
    \ halt
    \ push
    \ top
    \ copy
    \ put
    \ call
    \ scl
    \ ret
    \ base
    \ frame
    \ reset
    \ jpz
    \ jump
    \ rd8
    \ rd16
    \ rd32
    \ sv8
    \ sv16
    \ sv32
    \ rdf32
    \ svf32
    \ not
    \ eq
    \ addi
    \ subi
    \ muli
    \ divi
    \ addf
    \ subf
    \ mulf
    \ divf
    \ lti
    \ lei
    \ gti
    \ gei
    \ ltf
    \ lef
    \ gtf
    \ gef
    \ native
    \ i8
    \ i16
    \ i32
    \ f32

let b:current_syntax = "sals"
