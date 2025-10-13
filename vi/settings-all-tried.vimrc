" -- Programmer Settings --

"Set title of terminal window
set title
"Keep buffer undo history after leaving it
set hidden
"Enable loading of vimrc files in current directory
set exrc
"Enable syntax highlight
:syntax enable
"Enable instant search highlight
set incsearch
"Don't insert first completion automatically
set completeopt=longest,menuone
"Tab & Indent size settings
set autoindent          " Controls automatic indentation
set softtabstop=0       " Controls mix of tabs and spaces, 0 disables it
set noexpandtab         " Controls whether to expand tab to the corresponding # of chars
set shiftwidth=4        " Number of spaces to use for each step of (auto)indent
set tabstop=4           " Indentation size

"Colors visible both from terminal and GUI
colorscheme delek

set wildmode=list:longest
set completeopt=longest,menu,preview

"Smart indent settings
set cindent
:au BufNewFile,BufRead *.c set cindent
:au BufNewFile,BufRead *.h set cindent
:au BufNewFile,BufRead *.cpp set cindent
:au BufNewFile,BufRead *.hpp set cindent
:au BufNewFile,BufRead *.java set cindent
:au BufNewFile,BufRead *.pl set cindent
:au BufNewFile,BufRead *.tcl set cindent
"Use both cscope and ctags in that order
set cscopetag
"Use cscope in error list
set cscopequickfix=s-,g-,d-,i-,t-,e-
"Automatically add CScope connection
:cscope add cscope.out
"Automatically load file type plugins
filetype plugin on
"Quickly redo ("u" is for Undo, S-u for Redo)
map <S-u> :redo<CR>
"Set backspace to work anyhow
set backspace=2
"Display a status line
set laststatus=2
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P
"Keep caret N lines from window edge
set scrolloff=4

"Restore last cursor position
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

"Recursive external grep
set grepprg=grep\ -rOsInHE\ --exclude=tags\ --exclude=TAGS\ --exclude-dir=.git\ --exclude-dir=build\ --exclude-dir=Build\ --exclude-dir=coverage\ --exclude-dir=./framework/assets\ --exclude-dir=ASC.xcworkspace\ --exclude-dir=Libraries\ --exclude=build*.log\ --exclude=ctags*.log\ --exclude=*~\ --exclude=*.min.js\ --exclude=*.pbxproj\ $*

" -- Useful settings -- 

"Disable auto-commenting for all files (i.e. auto-inserting the comment characters)
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

"Wrap at whole words
set linebreak
"Display paragraphs that end below the screen
set display+=lastline
"Incremental searching
set incsearch
"Highlight searched item
set hlsearch
"Write buffers before make
set autowrite
"Command line settings
set cmdheight=3
"set laststatus=2
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

" -- Extra keyboard shortcuts --

"See status of cvs files in current buffer
map <F2> :r !cvs -n -q update<CR>
inoremap <F2> <C-O>:r !cvs -n -q update<CR>
"Run jedit on current buffer
map <S-F8> :!jedit % &<CR>
inoremap <S-F8> <C-O>:!jedit % &<CR>
"Run jedit on file under cursor
nmap <F8> :!jedit <C-R>=expand("<cfile>") <CR>&<CR>
"Run emacs on current buffer (useful for it's M-x ediff... commands)
map <S-F9> :!emacs % &<CR>
inoremap <S-F9> <C-O>:!emacs % &<CR>
"Run emacs on file under cursor
nmap <F9> :!emacs <C-R>=expand("<cfile>") <CR>&<CR>
"Cscope shortcuts - no prompt
"0 or s: Find this C symbol
nmap <F11>0 :cs find 0 <C-R>=expand("<cword>")<CR><CR> 
"1 or g: Find this definition
nmap <F11>1 :cs find 1 <C-R>=expand("<cword>")<CR><CR>
"2 or d: Find functions called by this function
nmap <F11>2 :cs find 2 <C-R>=expand("<cword>")<CR><CR>
"3 or c: Find functions calling this function
nmap <F11>3 :cs find 3 <C-R>=expand("<cword>")<CR><CR>
"4 or t: Find this text string
nmap <F11>4 :cs find 4 <C-R>=expand("<cword>")<CR><CR>
"6 or e: Find this egrep pattern
nmap <F11>6 :cs find 6 <C-R>=expand("<cword>")<CR><CR>
"7 or f: Find this file
nmap <F11>7 :cs find 7 <C-R>=expand("<cfile>")<CR><CR>
"8 or i: Find files #including this file
nmap <F11>8 :cs find 8 ^<C-R>=expand("<cfile>")<CR>$<CR>
"Cscope shortcuts - with prompt
nmap <F12>0 :cs find 0 <C-R>=expand("<cword>")<CR>
nmap <F12>1 :cs find 1 <C-R>=expand("<cword>")<CR>
nmap <F12>2 :cs find 2 <C-R>=expand("<cword>")<CR>
nmap <F12>3 :cs find 3 <C-R>=expand("<cword>")<CR>
nmap <F12>4 :cs find 4 <C-R>=expand("<cword>")<CR>
nmap <F12>6 :cs find 6 <C-R>=expand("<cword>")<CR>
nmap <F12>7 :cs find 7 <C-R>=expand("<cfile>")<CR>
nmap <F12>8 :cs find 8 ^<C-R>=expand("<cfile>")<CR>$

" -- Maybe useful settings --

"Find next
noremap <F3> n
inoremap <F3> <C-O>n
"Find previous
noremap <S-F3> <S-n>
inoremap <S-F3> <C-O><S-n>
"Next error
map <F4> :cn<CR>
inoremap <F4> <C-O>:cn<CR>
"Previous error
map <S-F4> :cp<CR>
inoremap <S-F4> <C-O>:cp<CR>
"Close current buffer
map <C-F4> :bd<CR>
inoremap <C-F4> <C-O>:bd<CR>
"Next tag match
map <F5> :tn<CR>
inoremap <F5> <C-O>:tn<CR>
"Previous tag match
map <S-F5> :tp<CR>
inoremap <S-F5> <C-O>:tp<CR>
"Next buffer
map <F6> :bn<CR>
inoremap <F6> <C-O>:bn<CR>
"Previous buffer
map <S-F6> :bp<CR>
inoremap <S-F6> <C-O>:bp<CR>
"Quickly start a make
map <F7> :make<CR>
inoremap <F7> <C-O>:make<CR>
"Start a make, and prompt for target
map <S-F7> :make 
inoremap <S-F7> <C-O>:make 
"Replace selected text in Visual, on crt line
vmap <F8> yq:is/<Esc>pa/<Esc>pa/gc
"Quickly start a replace command in Normal mode
nmap <F8> q:is///gc
"Grep for the selected text in Visual
vmap <C-F8> yq:ivimgrep /<Esc>pa/ **/*
"Search for selected text in Visual
vmap <S-F8> yq:i/<Esc>p<CR>
"Quicly start a grep
nmap <C-F8> q:ivimgrep // **/*
"Completion with Ctrl-Space, like in most editors
inoremap <C-Space> <C-n>
inoremap <C-S-Space> <C-p>
"Save with Ctrl-S
noremap <C-s> :update<CR>
inoremap <C-s> <C-O>:update<CR>
cnoremap <C-s> <C-C>:update<CR>
onoremap <C-s> <C-C>:update<CR>
"Better <Up>
noremap <Up> gk
inoremap <Up> <C-O>gk
inoremap <expr> <Up> pumvisible() ? "<Up>" : "<C-O>gk"
"Better <Down>
noremap <Down> gj
inoremap <Down> <C-O>gj
inoremap <expr> <Down> pumvisible() ? "<Down>" : "<C-O>gj"
"Better <Home>
noremap <Home> g0
inoremap <Home> <C-O>g0
"Better <Home>
noremap <End> g$
inoremap <End> <C-O>g$

command -nargs=1 CVSdiff silent call CVSdiff("%", "<args>")
function! CVSdiff(filename, cvsversion)
    " append a:filename to keep extension and therefore highlighting mode
    let patchname = tempname() . a:filename
    let tempname  = tempname() . a:filename
    let newname   = tempname() . a:filename
    execute "!cvs diff -a -r " . a:cvsversion . " " . a:filename . " > " . patch
    execute "!cp " . a:filename . " " . tempname
    execute "!patch -R -o " . newname . " " . tempname . " < " . patchname
    execute "vertical diffsplit " . newname
    call delete(patchname)
    call delete(tempname)
    call delete(newname)
endfunction


