" Preferred global default settings:
set number
set background=dark
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
syntax enable
match ErrorMsg '\s\+$'

" Set up the status line so it's colored and always on
set laststatus=2
highlight StatusLine cterm=none ctermbg=darkgreen ctermfg=black
highlight StatusLineNC cterm=none ctermbg=darkgreen ctermfg=black
highlight VertSplit cterm=none ctermbg=darkgreen ctermfg=black

" Removes trailing spaces:
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction

nnoremap <silent> <Leader>RemoveTrailingWhiteSpace :call TrimWhiteSpace()<CR>
autocmd FileWritePre    * :call TrimWhiteSpace()
autocmd FileAppendPre   * :call TrimWhiteSpace()
autocmd FilterWritePre  * :call TrimWhiteSpace()
autocmd BufWritePre     * :call TrimWhiteSpace()

" Add files ending in md to the list of files recognised as markdown:
autocmd BufNewFile,BufFilePre,BufRead *.md set filetype=markdown

" My Markdown environment
function! MarkdownSettings()
    set textwidth=79
    set spell spelllang=en_au
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.mdwn :call MarkdownSettings()

" My ReStructured Text environment
function! ReStructuredSettings()
    set textwidth=79
    set spell spelllang=en_au
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.rst :call ReStructuredSettings()

" My LaTeX environment:
function! LaTeXSettings()
    set textwidth=79
    set spell spelllang=en_au
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.tex :call LaTeXSettings()

" Settings for my Haskell environment:
function! HaskellSettings()
    set tabstop=4
    set shiftwidth=4
    set expandtab
    set textwidth=79
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.hs :call HaskellSettings()

" Settings for my Golang environment:
function! GoSettings()
    set tabstop=7
    set shiftwidth=7
    set noexpandtab
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.go :call GoSettings()

" Settings for my Python environment:
function! PythonSettings()
    set tabstop=4
    set shiftwidth=4
    set expandtab
    set textwidth=79
    set spell!
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.py :call PythonSettings()
