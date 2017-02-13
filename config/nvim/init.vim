"======================================================================
" Vim settings
"======================================================================

syntax on
set background=dark
set shell=/bin/zsh
set encoding=utf-8
"set spell                  " Spell check
set spelllang=en_us,ru_ru
set showcmd                " Shows which command is printed yet
set mouse=a                " Adds a mouse support to
set number                 " Displays line numbers
set incsearch              " Incremental search feature
set nohlsearch             " Prevent highlighting search results
set ignorecase
set smartcase              " Smart case in search sequence
set wrap                   " Wraps text
set autoread               " Autoread file if it was changed outside vim
set scrolljump=4           " Minimal number of lines to scroll whet the cursor gets off the screen
set scrolloff=4            " Minimal number of screen lines to keep above and below the cursor
set ruler                  " Shows the line and column number of cursor position
set hidden
set tw=79                  " Text width
set colorcolumn=80         " Column to prevent long lines in code
set modeline
set modelines=5

let mapleader="\<Space>"

" Indents
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" History
set history=64
set undolevels=128
set undofile
set undodir=~/.config/nvim/undodir
set undolevels=1000
set undoreload=10000

" Buffers
nmap <leader>s<left>   :leftabove  vnew<cr>
nmap <leader>s<right>  :rightbelow vnew<cr>
nmap <leader>s<up>     :leftabove  new<cr>
nmap <leader>s<down>   :rightbelow new<cr>

noremap <tab> <c-w><c-w>

nnoremap <leader>x      :bp <bar> bd #<cr>
nnoremap <leader>n      :bn<cr>
nnoremap <leader>p      :bp<cr>

" Improved navigation on wrapped lines
nnoremap j gj
nnoremap k gk

"======================================================================
" SPECIAL VIM MAGIC
"======================================================================

" Switch to US layout on normal mode
let g:layout='dvorak'
function! SetUsLayout()
    let g:layout=system('xkblayout-state print %v')
    silent ! xkblayout-state set 0
endfunction

function! RestoreLayout()
    if g:layout != 'dvorak'
        silent ! xkblayout-state set 1
    endif
endfunction

autocmd InsertLeave * call SetUsLayout()
autocmd InsertEnter * call RestoreLayout()

" Save files which require root permission
cmap w!! %!sudo tee > /dev/null %

inoremap jj <Esc> " Esc is so far away without this mapping...

" Disable arrows
map <Up> <NOP>
map <Down> <NOP>
map <Left> <NOP>
map <Right> <NOP>
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Move visual blocks
vnoremap < <gv
vnoremap > >gv

" Shift-Enter
"imap  <CR><CR><Esc>-cc

" Expand visual region by pressing v
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

"======================================================================
" Plugins
"======================================================================

" Install dein if not exists
let conf_dir = '~/.config/nvim/'
let dein_path = conf_dir . 'repos/' . 'github.com/Shougo/dein.vim'
if empty(glob(dein_path))
  silent execute '!curl
\ https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh |
\ bash -s' conf_dir
endif
exe 'set rtp+=' . dein_path

call dein#begin(conf_dir)
call dein#add('Shougo/dein')

" Better feeling
call dein#add('easymotion/vim-easymotion')      " Fast navigation using shortcuts
call dein#add('godlygeek/tabular')              " Easy aligning
call dein#add('vim-airline/vim-airline')        " Fancy status line as fuck
call dein#add('vim-airline/vim-airline-themes') " Fancy themes for fancy status line
call dein#add('Raimondi/delimitMate')           " Auto close quotes and etc.
call dein#add('SirVer/ultisnips')               " Ultisnippets configuration
call dein#add('lilydjwg/colorizer')             " Hilight hex colors

" Themes
call dein#add('squarefrog/tomorrow-night.vim')

" Deoplete as comletion engine
function! DoRemote(arg)
  UpdateRemotePlugins
endfunction
call dein#add('Shougo/deoplete.nvim', { 'do': function('DoRemote') })
call dein#add('zchee/deoplete-jedi')
call dein#add('fishbullet/deoplete-ruby')
call dein#add('zchee/deoplete-go', { 'do': 'make'})

" Languages extensions
call dein#add('neomake/neomake')                    " Advanced linter
call dein#add('eagletmt/neco-ghc')                  " Haskell completion
call dein#add('jvirtanen/vim-octave')               " Octave completion support
call dein#add('vim-scripts/dbext.vim')              " Databases support
call dein#add('lervag/vimtex')                      " LaTeX
call dein#add('vim-ruby/vim-ruby')                  " Ruby
call dein#add('tpope/vim-rails')                    " Ruby on Rails support
call dein#add('tpope/vim-endwise')                  " wisely add 'end' in ruby
call dein#add('kchmck/vim-coffee-script')           " Support of Coffee script
call dein#add('cakebaker/scss-syntax.vim')          " Sass syntax files
call dein#add('mattn/emmet-vim')                    " Make HTML usable
call dein#add('fatih/vim-go')                       " Full feature GO support
call dein#add('slim-template/vim-slim')             " Slim for vim
call dein#add('yosssi/vim-ace')
call dein#add('critiqjo/lldb.nvim')
call dein#add('isRuslan/vim-es6')                   " New javascript ES6
call dein#add('hail2u/vim-css3-syntax')             " CSS3 support

" Navigation
call dein#add('scrooloose/nerdtree') " File explorer
call dein#add('junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' })

" Uncategorized
call dein#add('airblade/vim-gitgutter')          " Git command supports
call dein#add('bronson/vim-trailing-whitespace') " Removes useless whitespaces
call dein#add('tpope/vim-surround')              " Surround quotes, tags and other
call dein#add('scrooloose/nerdcommenter')        " Comment tool
call dein#add('Xuyuanp/nerdtree-git-plugin')     " Git support for NERDTree

call dein#end()

syntax on
filetype on
filetype indent on
filetype plugin on
filetype plugin indent on
syntax enable
colorscheme tomorrow-night

"======================================================================
" EasyAlign
"======================================================================

map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

"======================================================================
" FZF
"======================================================================

nmap <leader><tab> :FZF<CR>

"=====================================================================
" Vimtex
"=====================================================================

let g:vimtex_view_method='zathura'
let g:vimtex_latexmk_progname='nvr'
let g:vimtex_view_general_viewer='zathura'
let g:vimtex_fold_manual=1
let g:vimtex_motion_matchparen=0
let g:vimtex_quickfix_open_on_warning=0
let g:vimtex_quickfix_mode=1

"autocmd FileType tex let b:vimtex_main = 'main.tex'
let g:tex_flavor = 'latex'

if !exists('g:deoplete#omni_patterns')
      let g:deoplete#omni_patterns = {}
  endif
let g:deoplete#omni_patterns.tex =
    \ '\v\\%('
    \ . '\a*cite\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
    \ . '|\a*ref%(\s*\{[^}]*|range\s*\{[^,}]*%(}\{)?)'
    \ . '|hyperref\s*\[[^]]*'
    \ . '|includegraphics\*?%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
    \ . '|%(include%(only)?|input)\s*\{[^}]*'
    \ . '|\a*(gls|Gls|GLS)(pl)?\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
    \ . '|includepdf%(\s*\[[^]]*\])?\s*\{[^}]*'
    \ . '|includestandalone%(\s*\[[^]]*\])?\s*\{[^}]*'
    \ . ')\m'

"======================================================================
" Deoplete
"======================================================================

let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 100

set completeopt-=preview

"======================================================================
" UltiSnips
"======================================================================

let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

"======================================================================
" Neomake
"======================================================================

autocmd! BufWritePost * Neomake
let g:neomake_ruby_enabled_makers = ['rubocop']

"======================================================================
" NERDTree
"======================================================================

nmap <leader>m :NERDTreeToggle<CR>
let  NERDTreeHighlightCursorline=1
let  NERDTreeIgnore=['.yardoc', 'pkg']

"======================================================================
" Golang
"======================================================================

let g:go_term_mode = "split"
let g:go_term_enabled = 1

let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_interfaces = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

"======================================================================
" Ruby
"======================================================================

autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
autocmd Filetype coffee setlocal ts=2 sts=2 sw=2
autocmd Filetype slim setlocal ts=2 sts=2 sw=2

let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_rails = 1

let g:monster#completion#rcodetools#backend = "async_rct_complete"
let g:deoplete#sources#omni#input_patterns = {
\   "ruby" : '[^. *\t]\.\w*\|\h\w*::',
\}

"======================================================================
" Airline
"======================================================================
set laststatus=2


let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts = 1
let g:Powerline_symbols='unicode'

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

set showcmd

