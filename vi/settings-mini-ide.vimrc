" More colors in the terminal version
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif
let g:rehash256 = 1

"set compatible
if has("gui")
	set guifont=Monaco:h13
	set mouse=a
	set clipboard=unnamed
else
	set mouse=
endif
set exrc
if has("vms")
	set nobackup          " do not keep a backup file, use versions instead
else
	set nobackup          " still don't keep a backup file - annyoing on Mac
endif
syntax on
set autoindent          " Controls automatic indentation
set softtabstop=0       " Controls mix of tabs and spaces, 0 disables it
set expandtab           " Expand tab to the corresponding # of chars
"set noexpandtab         " Don't expand tab to the corresponding # of chars
set shiftwidth=2        " Number of spaces to use for each step of (auto)indent
set tabstop=2           " Indentation size
set ignorecase
set smartcase
set showmode
set hlsearch
set scrolloff=0
set regexpengine=2      " seems to fix syntax highlight issues; default is 1
set foldopen-=search

" These don't work well if VIM colors don't match terminal's
let &t_EI = "\<Esc>[1 q"  " block cursor for normal mode
let &t_SR = "\<Esc>[3 q"  " underline cursor for replace mode
let &t_SI = "\<Esc>[5 q"  " line cursor for insert mode

" delete without yanking
nnoremap x "_x
vnoremap x "_x
nnoremap d "_d
vnoremap d "_d
nnoremap D "_D
vnoremap D "_D

" Make Undo more granular - each time you add a space
" The default is each time you press Enter, or for a whole Insert session
inoremap <space> <C-g>u<space>

" replace currently selected text with default register
" without yanking it
vnoremap p "_dP
vnoremap P "_dP

"Restore last cursor position
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup vimrc
    autocmd QuickFixCmdPost * botright copen 8
augroup END

" Enable '%' to go to matching tags and other stuff
runtime macros/matchit.vim

function GitDiff()
  :silent write
  :silent execute '!git diff --color=always -- ' . expand('%:p') . ' | less --RAW-CONTROL-CHARS'
  :redraw!
endfunction

map <F9> :make compile<CR>
map <F8> :make test<CR>
map <F7> :call GitDiff()<CR>
map <F3> :grep <c-r>=expand("<cword>")<cr> 
" The idea is mimic a replace in files
" Do your grep, then run a command like
" :%s/searched/to replace/
" Then press F4 followed by F6 to replace 
" the thing on the current line
map <F6> :s<CR>

map <F5> :cn<CR>
map <F4> :cp<CR>

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

" Fuzzy finder
set rtp+=/usr/local/opt/fzf
map <F2> :FZF<CR>

call plug#begin()

" multiple cursors
Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" status line
Plug 'git@github.com:maciakl/vim-neatstatus.git'

" color schemes
" everforest
Plug 'git@github.com:sainnhe/everforest.git'

" fzf native plugin
Plug 'junegunn/fzf'

" fzf.vim
Plug 'junegunn/fzf.vim'

" Fugitive - a Git plug-in
Plug 'https://tpope.io/vim/fugitive.git'

call plug#end()

set background=dark
colorscheme everforest

"Recursive external grep
set grepprg=grep\ -rnHE\ --binary-files=without-match\ --exclude-dir=.git\ --exclude-dir=build\ --exclude-dir=Build\ --exclude-dir=coverage\ --exclude-dir=target\ --exclude-dir=dist\ --exclude-dir=node_modules\ --exclude-dir='.sst'\ --exclude='*.log'\ --exclude='*~'\ --exclude=tags\ $*
set tags=tags

" Different colors when vimdiff is launched
if &diff
    colorscheme evening
		syntax off
endif

