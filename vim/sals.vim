" $ cp sals.vim ~/.vim/syntax/sals.vim
" $ grep '.sals' ~/.vimrc
" autocmd BufNewFile,BufRead *.sals setlocal filetype=sals

if exists("b:current_syntax")
    finish
endif

syn match Comment   ";.*$"
syn match Delimiter "[:\-]"
syn match Number    "\<[0-9]\+\>"
syn match Number    "\<[0-9]\+\.[0-9]\+\>"
syn match String    "\<\"[^\"]*\"\?\>"

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

let b:current_syntax = "sals"
