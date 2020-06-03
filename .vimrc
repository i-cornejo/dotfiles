"------------------------------------------------------------------
" Vundle Configuration
set nocompatible              " Vim required
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

	" let Vundle manage Vundle, required
	Plugin 'VundleVim/Vundle.vim'
	Plugin 'ycm-core/YouCompleteMe'
	Plugin 'tpope/vim-commentary'
	Plugin 'tpope/vim-repeat'

	" All of your Plugins must be added before the following line
call vundle#end()				" Required
filetype plugin indent on		" Required
"------------------------------------------------------------------

" YouCompleteMe Config
let g:ycm_confirm_extra_conf=0
let g:ycm_autoclose_preview_window_after_completion=1
command Doc YcmCompleter GetDoc
command Goto YcmCompleter GoTo

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
autocmd BufRead,BufNew *.py setlocal foldmethod=indent expandtab

" C/C++
autocmd BufRead,BufNew *.cpp,*.c,*.h setlocal tabstop=8 softtabstop=8
			\ shiftwidth=8

" Highlight Bad Whitespace
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/

" Load badass theme
colorscheme badwolf
set background=dark
syntax on

" Set Spellcheck
autocmd BufRead,BufNew *.txt setlocal spell
highlight SpellBad ctermfg=red

" Cool command menu completion
set wildmenu

" Special fixes for WSL
function! IsWSL()
	if has("unix")
		let lines = readfile("/proc/version")
		if lines[0] =~ "Microsoft"
			return 1
		endif
	endif
	return 0
endfunction

if (IsWSL())
	" WSL background color erase bug
	" https://github.com/microsoft/terminal/issues/832
	if (&term =~ '^xterm' && &t_Co == 256)
		  set t_ut= | set ttyscroll=1
	endif

	" Silence annoying bell
	set visualbell
endif
