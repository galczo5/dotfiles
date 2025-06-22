" === General Settings ===
set nocompatible              " Don't be compatible with vi
set number                    " Show line numbers
:set numberwidth=5
" set relativenumber            " Show relative line numbers
set showcmd                   " Show (partial) command in status line
set cursorline                " Highlight current line
set wildmenu                  " Enable command-line completion enhancement
set lazyredraw                " Don't redraw while executing macros

" === Indentation ===
set tabstop=4                 " Number of spaces a <Tab> in the file counts for
set shiftwidth=4              " Number of spaces to use for each step of (auto)indent
set expandtab                 " Use spaces instead of tabs
set autoindent                " Copy indent from current line
set smartindent               " Smarter indentation

" === Search ===
set hlsearch                  " Highlight search results
set incsearch                 " Show search matches as you type
set ignorecase                " Case-insensitive search...
set smartcase                 " ... unless uppercase is used

" === File handling ===
set hidden                    " Allow buffer switching without saving
set undofile                  " Persistent undo
set noswapfile                " Don't use swap files

" === Appearance ===
syntax on                     " Enable syntax highlighting
set background=dark           " For dark terminal themes
set termguicolors             " True color support (for terminals like Kitty)

highlight lineNr term=bold cterm=NONE ctermbg=none  ctermfg=DarkGray gui=bold guifg=DarkGray
highlight CursorLineNr term=bold cterm=none ctermbg=none ctermfg=DarkGray gui=bold guifg=DarkGray
highlight CursorLine term=bold cterm=NONE ctermbg=none  ctermfg=none gui=bold

" === Plugins ===
call plug#begin()
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'kien/ctrlp.vim'
call plug#end()

let g:airline_theme='base16_spacemacs'
