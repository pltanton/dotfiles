" TODO:
"  - snipptes plugin
"  - TODO, and other tag words support (hilighting easy tagging)
"  - deal with LLDB

"==============================================================================
"=== Vim settings
"==============================================================================

syntax on
set background=dark
set shell=/bin/zsh
set encoding=utf-8
set spelllang=en_us,ru_ru
set showcmd                
set mouse=a
set incsearch
set nohlsearch
set ignorecase
set smartcase
set wrap
set autoread
set ruler
set hidden
set colorcolumn=80
set modeline
set modelines=5
set relativenumber

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

" Windows
nmap <leader>s<left>   :leftabove  vnew<cr>
nmap <leader>s<right>  :rightbelow vnew<cr>
nmap <leader>s<up>     :leftabove  new<cr>
nmap <leader>s<down>   :rightbelow new<cr>

map <leader>h :wincmd h<CR>
map <leader>j :wincmd j<CR>
map <leader>k :wincmd k<CR>
map <leader>l :wincmd l<CR>


" Buffers
nnoremap <leader>x      :bp <bar> bd #<cr>
nnoremap <leader>n      :bn<cr>
nnoremap <leader>p      :bp<cr>

" Improved navigation on wrapped lines
nnoremap j gj
nnoremap k gk

"==============================================================================
"=== SPECIAL VIM MAGIC
"==============================================================================

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

"==============================================================================
"=== Plugins
"==============================================================================

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
call dein#add('Shougo/vimproc.vim', {'build' : 'make'})

"=== Editor improvements
call dein#add('godlygeek/tabular')
call dein#add('Raimondi/delimitMate')
call dein#add('neomake/neomake')
call dein#add('tpope/vim-surround')
call dein#add('scrooloose/nerdtree')
call dein#add('junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' })
call dein#add('majutsushi/tagbar')
call dein#add('scrooloose/nerdcommenter')
call dein#add('tpope/vim-repeat')

"=== Deoplete as completion engine
function! DoRemote(arg)
  UpdateRemotePlugins
endfunction
call dein#add('Shougo/deoplete.nvim', { 'do': function('DoRemote') })
call dein#add('zchee/deoplete-jedi')
call dein#add('fishbullet/deoplete-ruby')
call dein#add('zchee/deoplete-go', { 'do': 'make'})

"=== Debug
call dein#add('critiqjo/lldb.nvim')

"=== Language extensions
" Go
call dein#add('fatih/vim-go')
" Haskell
call dein#add('eagletmt/ghcmod-vim')
call dein#add('eagletmt/neco-ghc')
" Octave
call dein#add('jvirtanen/vim-octave')
" Databases
call dein#add('vim-scripts/dbext.vim')
" LaTeX
call dein#add('lervag/vimtex')
" Ruby
call dein#add('vim-ruby/vim-ruby')
call dein#add('tpope/vim-rails')
call dein#add('tpope/vim-endwise')
" Slim
call dein#add('slim-template/vim-slim')
" CoffeScript
call dein#add('kchmck/vim-coffee-script')
" CSS
call dein#add('cakebaker/scss-syntax.vim')
call dein#add('hail2u/vim-css3-syntax')
" HTML
call dein#add('mattn/emmet-vim')
" JavaScript
call dein#add('neoclide/vim-jsx-improve')
" Json
call dein#add('elzr/vim-json')
" Markdown
function! BuildComposer(info)
  if a:info.status != 'unchanged' || a:info.force
    !cargo build --release
  endif
endfunction
call dein#add('euclio/vim-markdown-composer', { 'do': function('BuildComposer') })

"=== Git
call dein#add('airblade/vim-gitgutter')
call dein#add('Xuyuanp/nerdtree-git-plugin')

"=== Visual improvements
call dein#add('squarefrog/tomorrow-night.vim')
call dein#add('vim-airline/vim-airline')
call dein#add('vim-airline/vim-airline-themes')
call dein#add('lilydjwg/colorizer')
"call dein#add('ryanoasis/vim-devicons')
"call dein#add('tiagofumo/vim-nerdtree-syntax-highlight')

call dein#end()

syntax on
filetype on
filetype indent on
filetype plugin on
filetype plugin indent on
syntax enable

colorscheme tomorrow-night

"==============================================================================
"=== Languages configuration
"==============================================================================

"=== Json
autocmd Filetype json setlocal ts=2 sts=2 sw=2

"=== Ruby
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_rails = 1

"=== Coffee
autocmd Filetype coffee setlocal ts=2 sts=2 sw=2

"=== Slim
autocmd Filetype slim setlocal ts=2 sts=2 sw=2

"=== LaTeX
autocmd Filetype tex setlocal ts=2 sts=2 sw=2

let g:vimtex_view_method='zathura'
let g:vimtex_latexmk_progname='nvr'
let g:vimtex_view_general_viewer='zathura'
let g:vimtex_fold_manual=1
let g:vimtex_motion_matchparen=0
let g:vimtex_quickfix_open_on_warning=0
let g:vimtex_quickfix_mode=1

let g:tex_flavor = 'latex'

"=== Haskell
" Add extra source directory if editing xmonad files
autocmd BufRead,BufNewFile */xmonad/*.hs call s:add_xmonad_path()
function! s:add_xmonad_path()
    let lib_path = resolve(expand('~/.xmonad/lib'))
    " For ghcmod-vim plugin
    if !exists('b:ghcmod_ghc_options')
        let b:ghcmod_ghc_options = []
    endif
    call add(b:ghcmod_ghc_options, '-i' . lib_path)

    " For neomake
    let cur = neomake#makers#ft#haskell#ghcmod()
    let cur.args = ['-g', '-i' . lib_path, 'check']
    let b:neomake_haskell_ghcmod_maker = cur
endfunction

"=== Golang
let g:go_term_mode = "split"
let g:go_term_enabled = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_interfaces = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1


"==============================================================================
" Plugins configuration
"==============================================================================

"=== FZF
nmap <leader><tab> :FZF<CR>
set completeopt-=preview

"=== Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 100

" LaTeX
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

" Golang
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

" Ruby
let g:deoplete#sources#omni#input_patterns = {
\   "ruby" : '[^. *\t]\.\w*\|\h\w*::',
\}

"=== Neomake
autocmd! BufWritePost * Neomake!
" Haskell
let g:neomake_haskell_enabled_makers = ['ghcmod']
" JavaScript
" JSX
let g:neomake_javascript_jsx_enabled_makers = ['eslint']

"=== NERDTree

nmap <leader>m :NERDTreeToggle<CR>
nmap <F10> :NERDTreeToggle<CR>
let  NERDTreeHighlightCursorline=1
let  NERDTreeIgnore=['.yardoc', 'pkg']

"=== Airline
set laststatus=2

let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts = 1
let g:Powerline_symbols='unicode'

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

set showcmd

"=== Tabularize
inoremap <silent> <Bar> <Bar><Esc>:call <SID>align()<CR>a

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction

"=== Tagbar
nmap <F8> :TagbarToggle<CR>

