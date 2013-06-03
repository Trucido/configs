" woddfellow2's Vim Config
" by woddfellow2 | http://wlair.us.to/

syntax on

set showcmd
set ruler
set splitbelow
set splitright

set directory=$HOME/.vim/swap
set backupdir=$HOME/tmp

" Fix tab completion
set wildmenu
set wildmode=longest,list

" Because I can:
set shortmess+=I
map ZQ <Nop>

" Disable mouse reporting:
set mouse=

" As per RFC 1855:
autocmd FileType mail set textwidth=64

" Pointless eye candy is pointless
colorscheme polemon
