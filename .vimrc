set number
set background=dark
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set spell spelllang=en_au
syntax enable
match ErrorMsg '\s\+$'
" Removes trailing spaces
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction

nnoremap <silent> <Leader>RemoveTrailingWhiteSpace :call TrimWhiteSpace()<CR>
autocmd FileWritePre    * :call TrimWhiteSpace()
autocmd FileAppendPre   * :call TrimWhiteSpace()
autocmd FilterWritePre  * :call TrimWhiteSpace()
autocmd BufWritePre     * :call TrimWhiteSpace()

autocmd BufNewFile,BufFilePre,BufRead *.md set filetype=markdown

function! HaskellSettings()
    set tabstop=8
    set shiftwidth=8
    set expandtab
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.hs :call HaskellSettings()

function! GoSettings()
    set tabstop=7
    set shiftwidth=7
    set noexpandtab
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.go :call GoSettings()

function! PythonSettings()
    set tabstop=4
    set shiftwidth=4
    set expandtab
    set textwidth=79
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.py :call PythonSettings()
