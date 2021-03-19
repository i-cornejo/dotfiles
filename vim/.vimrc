" Filetype settings
filetype plugin indent on
syntax on

" Set numbers and ruler
set number
set relativenumber
set ruler

" Undo and swap files
set undodir=~/.vim/undo
set undofile
set directory=~/.vim/swapfiles//

" Set tab size to 4 spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4

" Mark the 80 character column
set colorcolumn=80

" Variables
let mapleader=","
let g:session_dir='~/.vim/sessions'

" Commands
command Clang w|!gcc -Wall % && ./a.out
command Cplus w|!g++ -Wall % && ./a.out
command Clear !for i in {1..5}; do clear; done
command Py w|!python3 %
command Sesh Ex ~/.vim/sessions

" Mappings
nnoremap j  gj
nnoremap k  gk
exec 'nnoremap <Leader>ss :mks! ' . g:session_dir . '/'
exec 'nnoremap <Leader>sr :so ' . g:session_dir.
			\'/*.vim<C-D><BS><BS><BS><BS><BS>'
nnoremap <leader>n :terminal<ENTER>
nnoremap <leader>v :vertical terminal<ENTER>

" Folding
set foldenable
set foldlevelstart=10
set foldmethod=syntax

" Split behavior
set splitbelow
set splitright

" Python settings
autocmd BufRead,BufNew *.py setlocal foldmethod=indent expandtab textwidth=80

" C/C++
autocmd BufRead,BufNew *.cpp,*.c,*.h setlocal tabstop=8 softtabstop=8
			\ shiftwidth=8

" Highlight Bad Whitespace
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/


" Load badass theme if not running in tty
if has("gui running") || &term == "xterm" || &term == "xterm-256color"
   colorscheme molokai
else
	colorscheme darkblue
endif

set background=dark

" Cool command menu completion
set wildmenu
