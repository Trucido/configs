" ~/.vimrc 2.0.1
" Last Modified: Tue Nov 18 00:32:05 PST 2014

" Facilitate some things:
syntax on
filetype plugin indent on

" Improve the % key
runtime macros/matchit.vim

" More visual feedback and no splash screen
set shortmess+=I
set showcmd
set ruler

" Buffers
set hidden

" Split correctly
set splitbelow
set splitright

" Stop leaving droppings in the current working directory
set directory=$HOME/.vim/swap
set backupdir=$HOME/tmp

" Fix tab completion
set wildmenu
set wildmode=longest,list

" Search
set incsearch
set smartcase

" Highlight margin
set colorcolumn=80

" Do that thing Emacs does with brackets
set showmatch

" Spelling
set spell spelllang=en

" As per RFC 1855:
autocmd FileType mail set textwidth=64

" Custom functions
function PostUpdate()
	execute "normal I[" . strftime("%a %b %d %H:%M:%S %Z %Y") . "] [b]Update:[/b] "
endfunction

" Keybindings
map ZQ <Nop>
map <Leader>u :call PostUpdate()<CR>a

map <Leader>bp :bprevious<CR>
map <Leader>bn :bnext<CR>
map <Leader>ba :ball<CR>
map <Leader>bd :bdelete<CR>
map <Leader>bl :blast<CR>

map <Leader>tn :tabnew<CR>
map <Leader>tc :tabclose<CR>
map <Leader>to :tabonly<cr>

" Emacs-style keybindings in command line
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-B> <Left>
cnoremap <C-F> <Right>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <C-D> <Del>

" Colour scheme
if &t_Co >= 256 || has("gui_running")
	colorscheme calmar256-dark
else
	colorscheme polemon
endif

" gVim
if has("gui_running")
	set guioptions=aic
	set guifont=Terminus\ 11

	set columns=80
	set lines=50
endif

" Pathogen
execute pathogen#infect()

" Time stamps
let g:timestamp_rep="%a %b %d %H:%M:%S %Z %Y"
let timestamp_regexp='\v\C%(<Last %([cC]hanged?|[Mm]odified):\s+)@<=.*$'

" Status bar (vim-airline)
set laststatus=2

let g:airline_left_sep=''
let g:airline_right_sep=''

let g:airline#extensions#tabline#enabled=1
