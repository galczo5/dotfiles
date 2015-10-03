set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Plugins
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'lilydjwg/colorizer'
Plugin 'Shougo/neocomplcache.vim'
Plugin 'itchyny/lightline.vim'
Plugin 'benjaminwhite/Benokai'
Plugin 'chankaward/vim-railscasts-theme'
Plugin 'mattn/emmet-vim'
Plugin 'scrooloose/syntastic'
Plugin 'pangloss/vim-javascript'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'hail2u/vim-css3-syntax'


call vundle#end()            " required
filetype plugin indent on    " required

" Good looking GVIM toolbar
set guioptions-=m
set guioptions-=r
set guioptions-=L
set tb=icons,text
aunmenu ToolBar.Drukuj
aunmenu ToolBar.Pomoc
aunmenu ToolBar.FindHelp

set number

if has("gui_running")
  set columns=150
  set lines=40
  colorscheme railscasts
else
  colorscheme Benokai
  set t_Co=1024
endif

autocmd VimEnter * NeoComplCacheEnable

" Switch window from NERDTree to empty
autocmd VimEnter * wincmd p 

set cindent
set smartindent
set autoindent
set expandtab
set tabstop=2
set shiftwidth=2
set cursorline
set numberwidth=5
set autochdir
set autoread
set ignorecase
set smartindent
set magic
set hlsearch

" NO TO BACKUP
set nobackup
set nowb
set noswapfile

set guifont=Source\ Code\ Pro\ 10

filetype plugin on
let g:lightline = {'colorscheme': 'wombat',}
set laststatus=2

" NeoComplCache works only triggered on tab
let g:neocomplcache_disable_auto_complete = 1

inoremap <expr><TAB> pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : "\<C-x>\<C-u>"
function! s:check_back_space()"{{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1] =~ '\s'
endfunction"}}}

highlight NonText ctermfg=bg guifg=bg

" Mappings
map <C-t> :vspl <CR>
map <silent> <C-n> :NERDTreeToggle <CR>
map <C-Tab> <C-w><C-w>
let g:user_emmet_leader_key='<C-Z>'
