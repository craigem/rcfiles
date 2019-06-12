"" Preferred global default settings:
set number
set background=dark
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
syntax enable
colorscheme solarized
set t_Co=256            " use 265 colors in vim
" hi Normal guibg=NONE ctermbg=NONE
hi SpellBad cterm=underline
match ErrorMsg '\s\+$'

set rtp+=/run/current-system/sw/share/vim-plugins/vim-airline
set rtp+=/run/current-system/sw/share/vim-plugins/vim-airline-themes
set rtp+=/run/current-system/sw/share/vim-plugins/neocomplete-vim

let g:airline_powerline_fonts = 1
let g:airline_theme='solarized'
set laststatus=2

call togglebg#map("<F10>")

" Set up the status line so it's colored and always on
set laststatus=2
" highlight StatusLine cterm=none ctermbg=black ctermfg=244
"highlight StatusLineNC cterm=none ctermbg=black ctermfg=244
"highlight VertSplit cterm=none ctermbg=black ctermfg=244
"highlight LineNr cterm=none ctermbg=black ctermfg=244

" Removes trailing spaces:
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction

nnoremap <silent> <Leader>RemoveTrailingWhiteSpace :call TrimWhiteSpace()<CR>
autocmd FileWritePre    * :call TrimWhiteSpace()
autocmd FileAppendPre   * :call TrimWhiteSpace()
autocmd FilterWritePre  * :call TrimWhiteSpace()
autocmd BufWritePre     * :call TrimWhiteSpace()

" Transparent editing of gpg encrypted files.
" By Wouter Hanegraaff <wouter@blub.net>
augroup encrypted
    au!

    " First make sure nothing is written to ~/.viminfo while editing
    " an encrypted file.
    autocmd BufReadPre,FileReadPre      *.gpg set viminfo=
    " We don't want a swap file, as it writes unencrypted data to disk
    autocmd BufReadPre,FileReadPre      *.gpg set noswapfile
    " Switch to binary mode to read the encrypted file
    autocmd BufReadPre,FileReadPre      *.gpg set bin
    autocmd BufReadPre,FileReadPre      *.gpg let ch_save = &ch|set ch=2
    autocmd BufReadPost,FileReadPost    *.gpg '[,']!gpg --decrypt 2> /dev/null
    " Switch to normal mode for editing
    autocmd BufReadPost,FileReadPost    *.gpg set nobin
    autocmd BufReadPost,FileReadPost    *.gpg let &ch = ch_save|unlet ch_save
    autocmd BufReadPost,FileReadPost    *.gpg execute ":doautocmd BufReadPost " . expand("%:r")

    " Convert all text to encrypted text before writing
    autocmd BufWritePre,FileWritePre    *.gpg   '[,']!gpg --default-key=04CE4B93 --default-recipient-self -ae 2>/dev/null
    " Undo the encryption so we are back in the normal text, directly
    " after the file has been written.
    autocmd BufWritePost,FileWritePost    *.gpg   u
augroup END

" Add files ending in md to the list of files recognised as markdown:
autocmd BufNewFile,BufFilePre,BufRead *.md set filetype=markdown

" My Markdown environment
function! MarkdownSettings()
    set textwidth=79
    set spell spelllang=en_au
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.mdwn :call MarkdownSettings()
autocmd BufNewFile,BufFilePre,BufRead *.md :call MarkdownSettings()

" My ReStructured Text environment
function! ReStructuredSettings()
    set textwidth=79
    set spell spelllang=en_au
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.rst :call ReStructuredSettings()
autocmd BufNewFile,BufFilePre,BufRead *.txt :call ReStructuredSettings()

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

" My Mutt environment
function! MuttSettings()
    set textwidth=79
    set spell spelllang=en_au
    "set tabstop=4
    "set shiftwidth=4
    "set expandtab
endfunction
autocmd BufNewFile,BufFilePre,BufRead /tmp/mutt-* :call MuttSettings()
autocmd BufNewFile,BufFilePre,BufRead /tmp/neomutt-* :call MuttSettings()

" Settings for my C environment:
function! CSettings()
    set tabstop=2
    set shiftwidth=2
    set expandtab
    set textwidth=79
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.c :call CSettings()

" Settings for my YAML environment:
function! YAMLSettings()
    set tabstop=2
    set shiftwidth=2
    set expandtab
    set textwidth=79
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.yaml :call YAMLSettings()

" Settings for my Bash environment:
function! BashSettings()
    set tabstop=4
    set shiftwidth=4
    set expandtab
    set textwidth=79
    set spell!
endfunction
autocmd BufNewFile,BufFilePre,BufRead *.sh :call BashSettings()

" My Bzr commit environment
function! BzrSettings()
    set textwidth=79
    set spell spelllang=en_au
    set tabstop=4
    set shiftwidth=4
    set expandtab
endfunction
autocmd BufNewFile,BufFilePre,BufRead bzr_* :call BzrSettings()
